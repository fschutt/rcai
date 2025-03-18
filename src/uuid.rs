use serde_derive::{Deserialize, Serialize};
use std::sync::atomic::AtomicU64;

static COUNTER1: AtomicU64 = AtomicU64::new(0);

/// UUID to identify something
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Uuid(u128);

impl core::fmt::Debug for Uuid {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl core::fmt::Display for Uuid {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let id = self.0;
        write!(
            f,
            "{:08x}-{:04x}-{:04x}-{:04x}-{:012x}",
            (id >> 96) & 0xFFFFFFFF,
            (id >> 80) & 0xFFFF,
            (id >> 64) & 0xFFFF,
            (id >> 48) & 0xFFFF,
            id & 0xFFFFFFFFFFFF
        )
    }
}

impl Uuid {
    /// Generate a new pseudo-UUID without external dependencies
    pub fn new() -> Self {
        let id1 = COUNTER1.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Uuid(id1 as u128)
    }
}
