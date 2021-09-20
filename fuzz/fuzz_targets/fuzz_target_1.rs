#![no_main]
use libfuzzer_sys::fuzz_target;
use ical_rrule::parser::rrulparams;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = rrulparams(s);
    }
});
