const RIDGE: f64 = 1e-6;

/// The memory stores from the article. All of them share one lookup:
/// score every stored key against the query, gate the score, blend.
#[derive(Clone, Copy, Debug)]
pub enum Store {
    OuterProduct,
    OptimalLinear,
    Relu,
    ReluPow(i32),
    SwiGlu,
    Softmax { beta: f32 },
    /// Qwen-style LogN scaling: beta grows with ln(M) past `train_len`.
    SoftmaxLogN { beta: f32, train_len: usize },
}

#[derive(Clone, Copy, Debug)]
enum Gate {
    Linear,
    Relu,
    ReluPow(i32),
    SwiGlu,
    Exp(f32),
}

impl Gate {
    fn apply(self, s: f32) -> f32 {
        match self {
            Gate::Linear => s,
            Gate::Relu => s.max(0.0),
            Gate::ReluPow(p) => s.max(0.0).powi(p),
            Gate::SwiGlu => s * s / (1.0 + (-s).exp()),
            // s <= 1 for unit vectors, so shifting by 1 never overflows
            Gate::Exp(beta) => (beta * (s - 1.0)).exp(),
        }
    }
}

pub struct FuzzyMap<V> {
    keys: Vec<Vec<f32>>,
    ids: Vec<usize>,
    values: Vec<V>,
    gate: Gate,
}

impl<V: PartialEq> FuzzyMap<V> {
    pub fn build(store: Store, entries: impl IntoIterator<Item = (Vec<f32>, V)>) -> Self {
        let (keys, vals): (Vec<_>, Vec<_>) = entries
            .into_iter()
            .map(|(k, v)| (normalize(k), v))
            .unzip();
        let gate = gate_for(store, keys.len());
        let keys = match store {
            Store::OptimalLinear => whiten(keys),
            _ => keys,
        };
        let (ids, values) = intern(vals);
        Self { keys, ids, values, gate }
    }

    /// One weight per stored memory: gate(similarity(key_i, query)).
    pub fn weights(&self, query: &[f32]) -> Vec<f32> {
        let q = normalize(query.to_vec());
        self.keys.iter().map(|k| self.gate.apply(dot(k, &q))).collect()
    }

    /// Best value plus confidence = signal² / (signal² + interference²),
    /// i.e. 1 / (1 + ISR²) with the article's interference-to-signal ratio.
    pub fn get(&self, query: &[f32]) -> Option<(&V, f32)> {
        let per_value = self.per_value(query);
        let (best, &w) = per_value.iter().enumerate().max_by(|a, b| a.1.total_cmp(b.1))?;
        let total: f32 = per_value.iter().map(|x| x * x).sum();
        if w <= 0.0 || total == 0.0 {
            return Some((&self.values[best], 0.0));
        }
        Some((&self.values[best], w * w / total))
    }

    pub fn len(&self) -> usize {
        self.keys.len()
    }

    pub fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }

    fn per_value(&self, query: &[f32]) -> Vec<f32> {
        self.ids
            .iter()
            .zip(self.weights(query))
            .fold(vec![0.0; self.values.len()], |mut acc, (&id, w)| {
                acc[id] += w;
                acc
            })
    }
}

impl<V: PartialEq + AsRef<[f32]>> FuzzyMap<V> {
    /// The article's f(x) = Σ v_i · gate(k_i·x): a blended vector, not a pick.
    pub fn recall(&self, query: &[f32]) -> Vec<f32> {
        let w = self.weights(query);
        let norm = match self.gate {
            Gate::Exp(_) => w.iter().sum::<f32>().max(f32::MIN_POSITIVE),
            _ => 1.0,
        };
        let dim = self.values.first().map_or(0, |v| v.as_ref().len());
        self.ids.iter().zip(w).fold(vec![0.0; dim], |mut acc, (&id, wi)| {
            let v = self.values[id].as_ref();
            acc.iter_mut().zip(v).for_each(|(a, x)| *a += wi / norm * x);
            acc
        })
    }
}

fn gate_for(store: Store, m: usize) -> Gate {
    match store {
        Store::OuterProduct | Store::OptimalLinear => Gate::Linear,
        Store::Relu => Gate::Relu,
        Store::ReluPow(p) => Gate::ReluPow(p),
        Store::SwiGlu => Gate::SwiGlu,
        Store::Softmax { beta } => Gate::Exp(beta),
        Store::SoftmaxLogN { beta, train_len } => {
            let scale = (m.max(2) as f32).ln() / (train_len.max(2) as f32).ln();
            Gate::Exp(beta * scale.max(1.0))
        }
    }
}

fn intern<V: PartialEq>(vals: Vec<V>) -> (Vec<usize>, Vec<V>) {
    vals.into_iter().fold((Vec::new(), Vec::new()), |(mut ids, mut uniq), v| {
        let id = match uniq.iter().position(|u| *u == v) {
            Some(id) => id,
            None => {
                uniq.push(v);
                uniq.len() - 1
            }
        };
        ids.push(id);
        (ids, uniq)
    })
}

pub fn dot(a: &[f32], b: &[f32]) -> f32 {
    a.iter().zip(b).map(|(x, y)| x * y).sum()
}

pub fn normalize(v: Vec<f32>) -> Vec<f32> {
    let n = dot(&v, &v).sqrt();
    if n == 0.0 {
        return v;
    }
    v.into_iter().map(|x| x / n).collect()
}

/// Least-squares store: k̃_i = (KᵀK + λI)⁻¹ k_i, so k̃_i·q = k_i ᵀ(KᵀK)⁻¹ q.
fn whiten(keys: Vec<Vec<f32>>) -> Vec<Vec<f32>> {
    let Some(d) = keys.first().map(Vec::len) else {
        return keys;
    };
    let l = cholesky(&gram(&keys, d), d);
    keys.iter().map(|k| solve(&l, d, k)).collect()
}

fn gram(keys: &[Vec<f32>], d: usize) -> Vec<f64> {
    (0..d * d)
        .map(|ij| {
            let (i, j) = (ij / d, ij % d);
            let s: f64 = keys.iter().map(|k| k[i] as f64 * k[j] as f64).sum();
            if i == j { s + RIDGE } else { s }
        })
        .collect()
}

fn cholesky(a: &[f64], d: usize) -> Vec<f64> {
    let mut l = vec![0.0; d * d];
    for i in 0..d {
        for j in 0..=i {
            let s: f64 = (0..j).map(|k| l[i * d + k] * l[j * d + k]).sum();
            l[i * d + j] = match i == j {
                true => (a[i * d + i] - s).sqrt(),
                false => (a[i * d + j] - s) / l[j * d + j],
            };
        }
    }
    l
}

fn solve(l: &[f64], d: usize, b: &[f32]) -> Vec<f32> {
    let y = (0..d).fold(vec![0.0; d], |mut y, i| {
        let s: f64 = (0..i).map(|k| l[i * d + k] * y[k]).sum();
        y[i] = (b[i] as f64 - s) / l[i * d + i];
        y
    });
    let x = (0..d).rev().fold(vec![0.0; d], |mut x, i| {
        let s: f64 = (i + 1..d).map(|k| l[k * d + i] * x[k]).sum();
        x[i] = (y[i] - s) / l[i * d + i];
        x
    });
    x.into_iter().map(|v| v as f32).collect()
}
