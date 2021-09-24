use std::fmt;

use nom::{branch::alt, IResult};

use crate::parser::enum_utils::enum_element;

// TODO FIXME use chrono Weekday when arbitrary is implemented
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Weekday {
    /// Monday.
    Mon = 0,
    /// Tuesday.
    Tue = 1,
    /// Wednesday.
    Wed = 2,
    /// Thursday.
    Thu = 3,
    /// Friday.
    Fri = 4,
    /// Saturday.
    Sat = 5,
    /// Sunday.
    Sun = 6,
}

impl fmt::Display for Weekday {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Weekday::Mon => "MO",
                Weekday::Tue => "TU",
                Weekday::Wed => "WE",
                Weekday::Thu => "TH",
                Weekday::Fri => "FR",
                Weekday::Sat => "SA",
                Weekday::Sun => "SU",
            }
        )
    }
}

pub fn weekday(input: &str) -> IResult<&str, Weekday> {
    alt((
        enum_element("MO", Weekday::Mon),
        enum_element("TU", Weekday::Tue),
        enum_element("WE", Weekday::Wed),
        enum_element("TH", Weekday::Thu),
        enum_element("FR", Weekday::Fri),
        enum_element("SA", Weekday::Sat),
        enum_element("SU", Weekday::Sun),
    ))(input)
}
