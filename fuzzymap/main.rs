use fuzzy_map::{FuzzyMap, Store};

const DK: usize = 64;
const QUERIES: usize = 40;

const STORES: [(&str, Store); 8] = [
    ("outer product", Store::OuterProduct),
    ("optimal linear", Store::OptimalLinear),
    ("ReLU", Store::Relu),
    ("ReLU²", Store::ReluPow(2)),
    ("SwiGLU", Store::SwiGlu),
    ("ReLU³", Store::ReluPow(3)),
    ("softmax β=8", Store::Softmax { beta: 8.0 }),
    ("softmax logN", Store::SoftmaxLogN { beta: 8.0, train_len: 1024 }),
];

struct Rng(u64);

impl Rng {
    fn next_f32(&mut self) -> f32 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        ((self.0 >> 40) as f32 + 0.5) / (1u64 << 24) as f32
    }

    fn gaussian(&mut self) -> f32 {
        let (u, v) = (self.next_f32(), self.next_f32());
        (-2.0 * u.ln()).sqrt() * (std::f32::consts::TAU * v).cos()
    }

    fn vector(&mut self, d: usize) -> Vec<f32> {
        (0..d).map(|_| self.gaussian()).collect()
    }
}

/// Article metric: sqrt(Σ_{i≠j} w_i²) / w_j when querying with stored key j.
fn isr(weights: &[f32], target: usize) -> f32 {
    let signal = weights[target];
    let noise: f32 = weights
        .iter()
        .enumerate()
        .filter(|&(i, _)| i != target)
        .map(|(_, w)| w * w)
        .sum();
    noise.sqrt() / signal
}

fn experiment(store: Store, m: usize, rng: &mut Rng) -> (f32, f32) {
    let keys: Vec<Vec<f32>> = (0..m).map(|_| rng.vector(DK)).collect();
    let map = FuzzyMap::build(store, keys.iter().cloned().zip(0..m));
    let targets: Vec<usize> = (0..QUERIES).map(|i| i * m / QUERIES).collect();
    let mean_isr = targets.iter().map(|&j| isr(&map.weights(&keys[j]), j)).sum::<f32>() / QUERIES as f32;
    let hits = targets.iter().filter(|&&j| map.get(&keys[j]).map(|(v, _)| *v) == Some(j)).count();
    (mean_isr, hits as f32 / QUERIES as f32)
}

fn capacity_table() {
    let sizes = [16, 64, 128, 512, 2048, 8192];
    println!("interference / signal, querying with a stored key (d_k = {DK}); > 1 means recall fails\n");
    print!("{:<16}", "M =");
    sizes.iter().for_each(|m| print!("{m:>9}"));
    println!();
    let mut rng = Rng(0x9E3779B97F4A7C15);
    for (name, store) in STORES {
        print!("{name:<16}");
        for &m in &sizes {
            let (r, _) = experiment(store, m, &mut rng);
            print!("{r:>9.2}");
        }
        println!();
    }
}

fn trigram_embed(s: &str) -> Vec<f32> {
    let chars: Vec<char> = format!("  {}  ", s.to_lowercase()).chars().collect();
    chars.windows(3).fold(vec![0.0; 256], |mut v, w| {
        let h = w.iter().fold(0u32, |h, &c| h.wrapping_mul(31).wrapping_add(c as u32));
        v[(h % 256) as usize] += 1.0;
        v
    })
}

fn relation_demo() {
    let entries = [
        ("replaces", "Replaces"),
        ("replaced", "Replaces"),
        ("enables", "Enables"),
        ("is written in", "WrittenIn"),
        ("is a", "IsA"),
    ];
    let queries = ["replacs", "enabled", "written in", "superseded"];
    println!("\nrelation lookup with a spelling embedding (char trigrams)\n");
    for (name, store) in STORES {
        let map = FuzzyMap::build(store, entries.iter().map(|(k, v)| (trigram_embed(k), *v)));
        let row: Vec<String> = queries
            .iter()
            .filter_map(|q| map.get(&trigram_embed(q)).map(|(v, p)| format!("{q}→{v} {p:.2}")))
            .collect();
        println!("{name:<16}{}", row.join("   "));
    }
}

fn main() {
    capacity_table();
    relation_demo();
}
