use std::fmt;

#[cfg(feature = "arbitrary")]
use arbitrary::{Arbitrary, Result, Unstructured};
use chrono::{DateTime, NaiveDate, NaiveDateTime, TimeZone, Utc};
use nom::{branch::alt, bytes::complete::take, error::ErrorKind, IResult};

use crate::parser::arbitrary_enums::Enum2;

// The UNTIL or COUNT rule parts are OPTIONAL, but they MUST NOT occur in the same 'recur'.
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum RecurEnd {
    Until(RRuleDateOrDateTime),
    Count(u64),
    Forever,
}

impl fmt::Display for RecurEnd {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RecurEnd::Until(v) => match v {
                RRuleDateOrDateTime::Date(d) => write!(f, ";UNTIL={}", d.format("%Y%m%d")),
                RRuleDateOrDateTime::DateTime(d) => match d {
                    RRuleDateTime::Utc(dt) => write!(f, ";UNTIL={}", dt.format("%Y%m%dT%H%M%SZ")),
                    RRuleDateTime::Unspecified(dt) => {
                        write!(f, ";UNTIL={}", dt.format("%Y%m%dT%H%M%S"))
                    } //RRuleDateTime::Offset(dt) => write!(f, ";UNTIL={}", dt.format("%Y%m%dT%H%M%S")), // TODO FIXME
                },
            },
            RecurEnd::Count(v) => write!(f, ";COUNT={}", v),
            RecurEnd::Forever => Ok(()),
        }
    }
}

// https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.4
pub fn date(input: &str) -> IResult<&str, NaiveDate> {
    let (input, value) = take(8u32)(input)?;

    // https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html#specifiers
    match NaiveDate::parse_from_str(value, "%Y%m%d") {
        Ok(datetime) => Ok((input, datetime)),
        Err(_) => Err(nom::Err::Error(nom::error::Error {
            input, // TODO FIXME
            code: ErrorKind::Fail,
        })),
    }
    //let b = Utc.datetime_from_str(value, "%Y%m%d")?;
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RRuleDateTime {
    Utc(DateTime<Utc>),
    Unspecified(NaiveDateTime),
    //Offset(DateTime<FixedOffset>),
}

pub fn datetime_to_utc(date_time: RRuleDateTime) -> DateTime<Utc> {
    match date_time {
        RRuleDateTime::Utc(v) => v,
        RRuleDateTime::Unspecified(v) => DateTime::from_utc(v, Utc),
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RRuleDateOrDateTime {
    Date(NaiveDate),
    DateTime(RRuleDateTime),
}

pub fn date_or_datetime_to_utc(date_or_date_time: RRuleDateOrDateTime) -> DateTime<Utc> {
    match date_or_date_time {
        RRuleDateOrDateTime::Date(v) => DateTime::from_utc(v.and_hms(0, 0, 0), Utc),
        RRuleDateOrDateTime::DateTime(v) => datetime_to_utc(v),
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> Arbitrary<'a> for RRuleDateOrDateTime {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        match u.choose(&[Enum2::A, Enum2::B])? {
            Enum2::A => Ok(RRuleDateOrDateTime::DateTime(RRuleDateTime::arbitrary(u)?)),
            Enum2::B => {
                // https://github.com/chronotope/chrono/blob/3467172c31188006147585f6ed3727629d642fed/src/naive/internals.rs#L27
                //let year = u.int_in_range((i32::MIN >> 13)..=(i32::MAX >> 13))?;
                // we'll ical is stupid
                let year = u.int_in_range(0..=9999)?;
                // https://github.com/chronotope/chrono/blob/3467172c31188006147585f6ed3727629d642fed/src/naive/internals.rs#L268
                // https://github.com/chronotope/chrono/blob/3467172c31188006147585f6ed3727629d642fed/src/naive/internals.rs#L173
                let day = u.int_in_range(1..=365)?;
                Ok(RRuleDateOrDateTime::Date(
                    NaiveDate::from_yo_opt(year, day).unwrap(), //ok_or(arbitrary::Error::IncorrectFormat)?,
                ))
            }
        }
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> Arbitrary<'a> for RRuleDateTime {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let min = NaiveDate::from_yo(0, 1).and_hms(0, 0, 0).timestamp();
        let max = NaiveDate::from_yo(9999, 365).and_hms(0, 0, 0).timestamp();
        match u.choose(&[Enum2::A, Enum2::B])? {
            // seems to not be supported for ical
            /*0 => {
                let secs = u.int_in_range(i64::MIN..=i64::MAX)?;
                Ok(RRuleDateTime::Offset(
                    FixedOffset::east_opt(0)
                        .and_then(|v| v.timestamp_opt(secs, 0).single())
                        .ok_or(arbitrary::Error::IncorrectFormat)?,
                ))
            }*/
            Enum2::A => {
                let secs = u.int_in_range(min..=max)?;
                Ok(RRuleDateTime::Unspecified(
                    NaiveDateTime::from_timestamp_opt(secs, 0).unwrap(),
                    //.ok_or(arbitrary::Error::IncorrectFormat)?,
                ))
            }
            Enum2::B => {
                let secs = u.int_in_range(min..=max)?;
                Ok(RRuleDateTime::Utc(DateTime::from_utc(
                    NaiveDateTime::from_timestamp_opt(secs, 0).unwrap(),
                    //.ok_or(arbitrary::Error::IncorrectFormat)?,
                    Utc,
                )))
            }
        }
    }
}

fn datetime_utc(input: &str) -> IResult<&str, RRuleDateTime> {
    let (input, value) = take(16u32)(input)?;

    // https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html#specifiers
    match Utc.datetime_from_str(value, "%Y%m%dT%H%M%SZ") {
        Ok(datetime) => Ok((input, RRuleDateTime::Utc(datetime))),
        Err(_) => Err(nom::Err::Error(nom::error::Error {
            input, // TODO FIXME
            code: ErrorKind::Fail,
        })),
    }
}

fn datetime_unspecified(input: &str) -> IResult<&str, RRuleDateTime> {
    let (input, value) = take(15u32)(input)?;

    // https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html#specifiers
    match NaiveDateTime::parse_from_str(value, "%Y%m%dT%H%M%S") {
        Ok(datetime) => Ok((input, RRuleDateTime::Unspecified(datetime))),
        Err(_) => Err(nom::Err::Error(nom::error::Error {
            input, // TODO FIXME
            code: ErrorKind::Fail,
        })),
    }
}
/*
fn datetime_timezone(input: &str) -> IResult<&str, RRuleDateTime> {
    Err(nom::Err::Error(nom::error::Error {
        input, // TODO FIXME
        code: ErrorKind::Fail,
    }))
    //unimplemented!();
    /*let (input, value) = take(16u32)(input)?;

    // TZID=America/New_York:19980119T020000

    // https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html#specifiers
    match Utc.datetime_from_str(value, "%Y%m%dT%H%M%SZ") {
        Ok(datetime) => Ok((input, RRuleDateTime::Offset(datetime))),
        Err(_) => Err(nom::Err::Error(nom::error::Error {
            input: input, // TODO FIXME
            code: ErrorKind::Fail,
        })),
    }*/
}
*/

// https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.5
pub fn datetime(input: &str) -> IResult<&str, RRuleDateTime> {
    alt((
        datetime_utc,
        datetime_unspecified, /*, datetime_timezone*/
    ))(input)
}

pub fn enddate(input: &str) -> IResult<&str, RRuleDateOrDateTime> {
    alt((
        |i| datetime(i).map(|o| (o.0, RRuleDateOrDateTime::DateTime(o.1))),
        |i| date(i).map(|o| (o.0, RRuleDateOrDateTime::Date(o.1))),
    ))(input)
}
