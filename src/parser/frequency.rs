use std::fmt;

use nom::{branch::alt, IResult};

use super::enum_utils::enum_element;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Frequency {
    Secondly,
    Minutely,
    Hourly,
    Daily,
    Weekly,
    Monthly,
    Yearly,
}

impl fmt::Display for Frequency {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Frequency::Secondly => "SECONDLY",
                Frequency::Minutely => "MINUTELY",
                Frequency::Hourly => "HOURLY",
                Frequency::Daily => "DAILY",
                Frequency::Weekly => "WEEKLY",
                Frequency::Monthly => "MONTHLY",
                Frequency::Yearly => "YEARLY",
            }
        )
    }
}

pub fn freq(input: &str) -> IResult<&str, Frequency> {
    alt((
        enum_element("SECONDLY", Frequency::Secondly),
        enum_element("MINUTELY", Frequency::Minutely),
        enum_element("HOURLY", Frequency::Hourly),
        enum_element("DAILY", Frequency::Daily),
        enum_element("WEEKLY", Frequency::Weekly),
        enum_element("MONTHLY", Frequency::Monthly),
        enum_element("YEARLY", Frequency::Yearly),
    ))(input)
}
