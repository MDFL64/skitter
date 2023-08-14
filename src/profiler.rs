use ahash::AHashMap;

use std::{
    sync::LazyLock,
    sync::RwLock,
    time::{Duration, Instant},
};

static PROFILER_TABLE: LazyLock<RwLock<AHashMap<&'static str, Duration>>> =
    LazyLock::new(|| RwLock::new(AHashMap::new()));

pub fn profile<T: FnOnce() -> X, X>(tag: &'static str, f: T) -> (X, Duration) {
    let start = Instant::now();
    let res = f();
    let time = start.elapsed();
    let mut table = PROFILER_TABLE.write().unwrap();
    let entry = table.entry(tag).or_default();
    *entry += time;
    (res, time)
}

pub fn profile_log() {
    let table = PROFILER_TABLE.read().unwrap();
    let mut pairs: Vec<_> = table.iter().collect();
    pairs.sort_by(|(_, b), (_, a)| a.cmp(b));
    println!("PROFILE RESULTS:");
    for (name, t) in pairs {
        println!("    {:20} {:?}", name, t);
    }
}
