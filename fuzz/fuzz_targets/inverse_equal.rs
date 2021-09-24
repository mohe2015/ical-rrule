#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: RecurRule| {
    // fuzzed code goes here
});
