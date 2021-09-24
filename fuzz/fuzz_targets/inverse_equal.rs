#![no_main]
use libfuzzer_sys::fuzz_target;
use ical_rrule::parser::RecurRule;
use ical_rrule::parser::rrule;

fuzz_target!(|data: RecurRule| {
    let val = data.to_string();
    //println!("{}", val);
    let parsed = rrule(&val);
    /*match &parsed {
        Err(nom::Err::Error(nom::error::Error { input: _, code: nom::error::ErrorKind::Verify })) => {
            // TODO FIXME limited ranges in e.g. byhour should be validated otherwise or simply not generated
            return;
        }
        _ => {

        }
    };*/
    assert_eq!(Ok(("", data)), parsed);
});
