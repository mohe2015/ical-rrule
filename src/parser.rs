use std::fmt;
use std::{
    num::{NonZeroI16, NonZeroI8, NonZeroU64, NonZeroU8},
    ops::RangeBounds,
    str::FromStr,
};

use chrono::{DateTime, FixedOffset, NaiveDate, NaiveDateTime, TimeZone, Utc};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till, take_while, take_while1},
    combinator::{map_res, opt, verify},
    error::ErrorKind,
    multi::{fold_many0, many0, separated_list0, separated_list1},
    sequence::{preceded, tuple},
    IResult,
};

// The UNTIL or COUNT rule parts are OPTIONAL, but they MUST NOT occur in the same 'recur'.
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum RecurEnd {
    Until(RRuleDateOrDateTime),
    Count(NonZeroU64),
    Forever,
}

impl fmt::Display for RecurEnd {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RecurEnd::Until(v) => match v {
                RRuleDateOrDateTime::Date(d) => write!(f, ";UNTIL={}", d.format("%Y%m%d")),
                RRuleDateOrDateTime::DateTime(d) => match d {
                    RRuleDateTime::Utc(dt) => write!(f, ";UNTIL={}", dt.format("%Y%m%dT%H%M%S")),
                    RRuleDateTime::Unspecified(dt) => {
                        write!(f, ";UNTIL={}", dt.format("%Y%m%dT%H%M%S"))
                    }
                    RRuleDateTime::Offset(dt) => write!(f, ";UNTIL={}", dt.format("%Y%m%dT%H%M%S")),
                },
            },
            RecurEnd::Count(v) => write!(f, ";COUNT={}", v),
            RecurEnd::Forever => Ok(()),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct WeekdayNum {
    //relative: WeekdayRelative,
    pub ordwk: Option<i8>,
    pub weekday: Weekday,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct RecurRule {
    freq: Frequency,
    end: RecurEnd,
    interval: NonZeroU64,
    bysecond: Option<Vec<u8>>,
    byminute: Option<Vec<u8>>,
    byhour: Option<Vec<u8>>,
    byday: Option<Vec<WeekdayNum>>,
    bymonthday: Option<Vec<NonZeroI8>>,
    byyearday: Option<Vec<NonZeroI16>>,
    byweekno: Option<Vec<NonZeroI8>>,
    bymonth: Option<Vec<NonZeroU8>>,
    bysetpos: Option<Vec<NonZeroI16>>,
    weekstart: Weekday,
}

impl fmt::Display for RecurRule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bysecondstring = match &self.bysecond {
            Some(v) => {
                ";BYSECOND=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
            }
            None => "".to_string(),
        };
        let byminutestring = match &self.byminute {
            Some(v) => {
                ";BYMINUTE=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
            }
            None => "".to_string(),
        };
        let byhourstring = match &self.byhour {
            Some(v) => {
                ";BYHOUR=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
            }
            None => "".to_string(),
        };
        let bydaystring = match &self.byday {
            Some(v) => ";BYDAY=".to_string() + &v.iter().map(|v| match v.ordwk {
                Some(q) => q.to_string(),
                None => "".to_string(),
            } + &v.weekday.to_string()).collect::<Vec<String>>().join(", "),
            None => "".to_string(),
        };
        let bymonthdaystring = match &self.bymonthday {
            Some(v) => {
                ";BYMONTHDAY=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
            }
            None => "".to_string(),
        };
        let byyeardaystring = match &self.byyearday {
            Some(v) => {
                ";BYYEARDAY=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
            }
            None => "".to_string(),
        };
        let byweeknostring = match &self.byweekno {
            Some(v) => {
                ";BYWEEKNO=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
            }
            None => "".to_string(),
        };
        let bymonthstring = match &self.bymonth {
            Some(v) => {
                ";BYMONTH=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
            }
            None => "".to_string(),
        };
        let bysetposstring = match &self.bysetpos {
            Some(v) => {
                ";BYSETPOS=".to_string()
                    + &v.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
            }
            None => "".to_string(),
        };
        let intervalstring = if self.interval == NonZeroU64::new(1).unwrap() {
            "".to_string()
        } else {
            ";INTERVAL=".to_string() + &self.interval.to_string()
        };
        write!(
            f,
            "RRULE:FREQ={}{}{}{}{}{}{}{}{}{}{}{}",
            self.freq,
            self.end,
            intervalstring,
            bysecondstring,
            byminutestring,
            byhourstring,
            bydaystring,
            bymonthdaystring,
            byyeardaystring,
            byweeknostring,
            bymonthstring,
            bysetposstring
        )
    }
}

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

impl Default for RecurRule {
    fn default() -> Self {
        RecurRule {
            freq: Frequency::Yearly, // TODO FIXME no default
            end: RecurEnd::Forever,
            interval: NonZeroU64::new(1).unwrap(),
            bysecond: None,
            byminute: None,
            byhour: None,
            byday: None,
            bymonthday: None,
            byyearday: None,
            byweekno: None,
            bymonth: None,
            bysetpos: None,
            weekstart: Weekday::Mon,
        }
    }
}

enum RecurRulePart {
    Freq(Frequency),
    End(RecurEnd),
    Interval(NonZeroU64),
    Bysecond(Vec<u8>),
    Byminute(Vec<u8>),
    Byhour(Vec<u8>),
    Byday(Vec<WeekdayNum>),
    Bymonthday(Vec<NonZeroI8>),
    Byyearday(Vec<NonZeroI16>),
    Byweekno(Vec<NonZeroI8>),
    Bymonth(Vec<NonZeroU8>),
    Bysetpos(Vec<NonZeroI16>),
    Weekstart(Weekday),
}

pub fn constant_rrule(input: &str) -> IResult<&str, &str> {
    tag("RRULE")(input)
}

pub fn iana_token(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_ascii_alphanumeric() || c == '-')(input)
}

pub fn paramtext(input: &str) -> IResult<&str, &str> {
    take_till(|c: char| {
        (char::from(0x00) <= c && c <= char::from(0x08))
            || (char::from(0x0a) <= c && c <= char::from(0x1f))
            || c == char::from(0x7f)
            || c == ';'
            || c == ':'
            || c == ','
            || c == '"'
    })(input)
}

pub fn param_value(input: &str) -> IResult<&str, &str> {
    paramtext(input)
    // TODO FIXME
    //nom::branch::alt((paramtext, quoted_string))(input)
}

pub fn iana_param(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    let (input, iana_token) = iana_token(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, param_values) = separated_list1(tag(","), param_value)(input).unwrap(); // shouldn't be able to panic as param_value can be empty. will need to fuzz.
    Ok((input, (iana_token, param_values)))
}

// https://datatracker.ietf.org/doc/html/rfc5545#section-3.2
pub fn other_param(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    iana_param(input)
    // TODO FIXME maybe support x_param
}

pub fn rrulparam(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    let (input, _) = tag(";")(input)?;
    other_param(input)
}

pub fn rrulparams(input: &str) -> IResult<&str, Vec<(&str, Vec<&str>)>> {
    many0(rrulparam)(input)
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

pub const fn enum_element<'a, T: Copy>(
    string: &'static str,
    enum_element: T,
) -> impl FnMut(&'a str) -> IResult<&'a str, T> {
    move |input| {
        let (input, _) = tag(string)(input)?;
        Ok((input, enum_element))
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
    Offset(DateTime<FixedOffset>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RRuleDateOrDateTime {
    Date(NaiveDate),
    DateTime(RRuleDateTime),
}

#[cfg(feature = "arbitrary")]
use arbitrary::{Arbitrary, Result, Unstructured};

#[cfg(feature = "arbitrary")]
impl<'a> Arbitrary<'a> for RRuleDateOrDateTime {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        match u.int_in_range(0..=1)? {
            0 => Ok(RRuleDateOrDateTime::DateTime(RRuleDateTime::arbitrary(u)?)),
            1 => {
                // https://github.com/chronotope/chrono/blob/3467172c31188006147585f6ed3727629d642fed/src/naive/internals.rs#L27
                let year = u.int_in_range((i32::MIN >> 13)..=(i32::MAX >> 13))?;
                // https://github.com/chronotope/chrono/blob/3467172c31188006147585f6ed3727629d642fed/src/naive/internals.rs#L268
                // https://github.com/chronotope/chrono/blob/3467172c31188006147585f6ed3727629d642fed/src/naive/internals.rs#L173
                let day = u.int_in_range(1..=366)?;
                Ok(RRuleDateOrDateTime::Date(
                    NaiveDate::from_yo_opt(year, day)
                        .ok_or_else(|| arbitrary::Error::IncorrectFormat)?,
                ))
            }
            _ => panic!("not possible"),
        }
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> Arbitrary<'a> for RRuleDateTime {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        match u.int_in_range(0..=2)? {
            0 => {
                let secs = u.int_in_range(i64::MIN..=i64::MAX)?;
                Ok(RRuleDateTime::Offset(
                    FixedOffset::east_opt(0)
                        .and_then(|v| v.timestamp_opt(secs, 0).single())
                        .ok_or_else(|| arbitrary::Error::IncorrectFormat)?,
                ))
            }
            1 => {
                let secs = u.int_in_range(i64::MIN..=i64::MAX)?;
                Ok(RRuleDateTime::Unspecified(
                    NaiveDateTime::from_timestamp_opt(secs, 0)
                        .ok_or_else(|| arbitrary::Error::IncorrectFormat)?,
                ))
            }
            2 => {
                let secs = u.int_in_range(i64::MIN..=i64::MAX)?;
                Ok(RRuleDateTime::Utc(DateTime::from_utc(
                    NaiveDateTime::from_timestamp_opt(secs, 0)
                        .ok_or_else(|| arbitrary::Error::IncorrectFormat)?,
                    Utc,
                )))
            }
            _ => panic!("not possible"),
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

// https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.5
pub fn datetime(input: &str) -> IResult<&str, RRuleDateTime> {
    alt((datetime_utc, datetime_unspecified, datetime_timezone))(input)
}

pub fn enddate(input: &str) -> IResult<&str, RRuleDateOrDateTime> {
    alt((
        |i| datetime(i).map(|o| (o.0, RRuleDateOrDateTime::DateTime(o.1))),
        |i| date(i).map(|o| (o.0, RRuleDateOrDateTime::Date(o.1))),
    ))(input)
}

// TODO FIXME this parser is not strictly correct in all cases namely when a shorter number would suffice
pub fn digits<T: RangeBounds<U>, U: FromStr + PartialOrd>(
    range: T,
) -> impl FnMut(&str) -> IResult<&str, U> {
    move |input| {
        verify(
            map_res(
                take_while(|c: char| ('0'..='9').contains(&c) || c == '-'),
                |v: &str| v.parse::<U>(),
            ),
            |v: &U| range.contains(v),
        )(input)
    }
}

fn recur_rule_part(input: &str) -> IResult<&str, RecurRulePart> {
    alt((
        nom::combinator::map(preceded(tag("FREQ="), freq), RecurRulePart::Freq),
        nom::combinator::map(preceded(tag("UNTIL="), enddate), |v| {
            RecurRulePart::End(RecurEnd::Until(v))
        }),
        nom::combinator::map(
            preceded(
                tag("COUNT="),
                digits(std::num::NonZeroU64::new(1).unwrap()..),
            ),
            |v| RecurRulePart::End(RecurEnd::Count(v)),
        ),
        nom::combinator::map(
            preceded(
                tag("INTERVAL="),
                digits(std::num::NonZeroU64::new(1).unwrap()..),
            ),
            RecurRulePart::Interval,
        ),
        nom::combinator::map(
            preceded(tag("BYSECOND="), separated_list0(tag(","), digits(0..=60))),
            RecurRulePart::Bysecond,
        ),
        nom::combinator::map(
            preceded(tag("BYMINUTE="), separated_list0(tag(","), digits(0..=59))),
            RecurRulePart::Byminute,
        ),
        nom::combinator::map(
            preceded(tag("BYHOUR="), separated_list0(tag(","), digits(0..=23))),
            RecurRulePart::Byhour,
        ),
        nom::combinator::map(
            preceded(
                tag("BYDAY="),
                separated_list0(
                    tag(","),
                    nom::combinator::map(tuple((opt(digits((-53)..=53)), weekday)), |v| {
                        WeekdayNum {
                            ordwk: v.0,
                            weekday: v.1,
                        }
                    }),
                ),
            ),
            RecurRulePart::Byday,
        ),
        nom::combinator::map(
            preceded(
                tag("BYMONTHDAY="),
                separated_list0(
                    tag(","),
                    digits(
                        std::num::NonZeroI8::new(-31).unwrap()
                            ..=std::num::NonZeroI8::new(31).unwrap(),
                    ),
                ),
            ),
            RecurRulePart::Bymonthday,
        ),
        nom::combinator::map(
            preceded(
                tag("BYYEARDAY="),
                separated_list0(
                    tag(","),
                    digits(NonZeroI16::new(-366).unwrap()..=NonZeroI16::new(366).unwrap()),
                ),
            ),
            RecurRulePart::Byyearday,
        ),
        nom::combinator::map(
            preceded(
                tag("BYWEEKNO="),
                separated_list0(
                    tag(","),
                    digits(NonZeroI8::new(-53).unwrap()..=NonZeroI8::new(53).unwrap()),
                ),
            ),
            RecurRulePart::Byweekno,
        ),
        nom::combinator::map(
            preceded(
                tag("BYMONTH="),
                separated_list0(
                    tag(","),
                    digits(
                        std::num::NonZeroU8::new(1).unwrap()
                            ..=std::num::NonZeroU8::new(12).unwrap(),
                    ),
                ),
            ),
            RecurRulePart::Bymonth,
        ),
        nom::combinator::map(
            preceded(
                tag("BYSETPOS="),
                separated_list0(
                    tag(","),
                    digits(NonZeroI16::new(-366).unwrap()..=NonZeroI16::new(366).unwrap()),
                ),
            ),
            RecurRulePart::Bysetpos,
        ),
        nom::combinator::map(preceded(tag("WKST="), weekday), RecurRulePart::Weekstart),
    ))(input)
}

pub fn recur(input: &str) -> IResult<&str, RecurRule> {
    // frequency is required and has to be first.
    let (input, freq) = preceded(tag("FREQ="), freq)(input)?;

    let x = fold_many0(
        preceded(tag(";"), recur_rule_part),
        || RecurRule {
            freq,
            ..Default::default()
        },
        |mut acc, item| {
            // TODO FIXME detect overrides (which is currently not possible for alles fields like this)
            match item {
                RecurRulePart::Freq(_v) => acc.freq = freq,
                RecurRulePart::End(v) => acc.end = v,
                RecurRulePart::Interval(v) => acc.interval = v,
                RecurRulePart::Bysecond(v) => acc.bysecond = Some(v),
                RecurRulePart::Byminute(v) => acc.byminute = Some(v),
                RecurRulePart::Byhour(v) => acc.byhour = Some(v),
                RecurRulePart::Byday(v) => acc.byday = Some(v),
                RecurRulePart::Bymonthday(v) => acc.bymonthday = Some(v),
                RecurRulePart::Byyearday(v) => acc.byyearday = Some(v),
                RecurRulePart::Byweekno(v) => acc.byweekno = Some(v),
                RecurRulePart::Bymonth(v) => acc.bymonth = Some(v),
                RecurRulePart::Bysetpos(v) => acc.bysetpos = Some(v),
                RecurRulePart::Weekstart(v) => acc.weekstart = v,
            };
            acc
        },
    )(input);
    let _ = x;
    x
}

// but it SHOULD NOT be specified more than once. The recurrence set generated with multiple "RRULE" properties is undefined.
pub fn rrule(input: &str) -> IResult<&str, RecurRule> {
    let (input, _params) = preceded(tag("RRULE"), rrulparams)(input)?;
    // TODO FIXME don't drop params
    preceded(tag(":"), recur)(input)
}

#[cfg(test)]
mod tests {
    use std::num::{NonZeroI16, NonZeroI8, NonZeroU64, NonZeroU8};

    use chrono::{DateTime, NaiveDate, Utc};
    use nom::{error::ErrorKind, IResult};

    use crate::parser::{
        constant_rrule, date, datetime, enddate, freq, iana_param, iana_token, other_param,
        param_value, paramtext, rrule, rrulparams, Frequency, RRuleDateOrDateTime, RRuleDateTime,
        RecurEnd, RecurRule, Weekday, WeekdayNum,
    };

    #[test]
    fn it_works() {
        assert_eq!(constant_rrule("RRULE"), IResult::Ok(("", "RRULE")));
        assert_eq!(
            constant_rrule("NOTRRULE"),
            Err(nom::Err::Error(nom::error::Error {
                input: "NOTRRULE",
                code: ErrorKind::Tag
            }))
        );
        assert_eq!(
            constant_rrule("RRULEEXTENDED"),
            IResult::Ok(("EXTENDED", "RRULE"))
        );

        assert_eq!(
            iana_token(""),
            Err(nom::Err::Error(nom::error::Error {
                input: "",
                code: ErrorKind::TakeWhile1
            }))
        );
        assert_eq!(
            iana_token("_"),
            Err(nom::Err::Error(nom::error::Error {
                input: "_",
                code: ErrorKind::TakeWhile1
            }))
        );
        assert_eq!(iana_token("5"), IResult::Ok(("", "5")));
        assert_eq!(iana_token("i"), IResult::Ok(("", "i")));
        assert_eq!(
            iana_token("133THIS-IS-a-010-IAnATokn"),
            IResult::Ok(("", "133THIS-IS-a-010-IAnATokn"))
        );

        for fun in [paramtext, param_value] {
            assert_eq!(fun(""), IResult::Ok(("", "")));
            assert_eq!(fun("ä"), IResult::Ok(("", "ä")));
            assert_eq!(fun("耳ä"), IResult::Ok(("", "耳ä")));
            assert_eq!(fun("\""), IResult::Ok(("\"", "")));
        }

        for fun in [iana_param, other_param] {
            assert_eq!(fun("TEST=1"), IResult::Ok(("", ("TEST", vec!["1"]))));
            assert_eq!(
                fun("1-tEST=ädf,üsldifh"),
                IResult::Ok(("", ("1-tEST", vec!["ädf", "üsldifh"])))
            );
            assert_eq!(fun("TEST="), IResult::Ok(("", ("TEST", vec![""]))));
            assert_eq!(
                fun("TEST=,,1,,"),
                IResult::Ok(("", ("TEST", vec!["", "", "1", "", ""])))
            );
            assert_eq!(
                fun("TEST"),
                Err(nom::Err::Error(nom::error::Error {
                    input: "",
                    code: ErrorKind::Tag
                }))
            );
        }

        assert_eq!(rrulparams(""), IResult::Ok(("", vec![])));
        assert_eq!(
            rrulparams(";TEST=1"),
            IResult::Ok(("", vec![("TEST", vec!["1"])]))
        );
        assert_eq!(
            rrulparams(";TEST=1,2;TEST=3,4"),
            IResult::Ok(("", vec![("TEST", vec!["1", "2"]), ("TEST", vec!["3", "4"])]))
        );
        assert_eq!(rrulparams(";"), IResult::Ok((";", vec![])));

        assert_eq!(freq("MINUTELY"), IResult::Ok(("", Frequency::Minutely)));
        assert_eq!(freq("MINUTELYY"), IResult::Ok(("Y", Frequency::Minutely)));
        assert_eq!(
            freq("MINUTEL"),
            Err(nom::Err::Error(nom::error::Error {
                input: "MINUTEL",
                code: ErrorKind::Tag
            }))
        );

        assert_eq!(
            date("20210920"),
            IResult::Ok(("", NaiveDate::from_ymd(2021, 9, 20)))
        );
        assert_eq!(
            date("202109201"),
            IResult::Ok(("1", NaiveDate::from_ymd(2021, 9, 20)))
        );
        assert_eq!(
            date("20210a920"),
            Err(nom::Err::Error(nom::error::Error {
                input: "0",
                code: ErrorKind::Fail
            }))
        );
        assert_eq!(
            date("2021"),
            Err(nom::Err::Error(nom::error::Error {
                input: "2021",
                code: ErrorKind::Eof
            }))
        );

        assert_eq!(
            datetime("20210920T000000F"),
            IResult::Ok((
                "F",
                RRuleDateTime::Unspecified(NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0))
            ))
        );
        assert_eq!(
            datetime("20210920T000000Z"),
            IResult::Ok((
                "",
                RRuleDateTime::Utc(DateTime::from_utc(
                    NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0),
                    Utc
                ))
            ))
        );
        assert_eq!(
            datetime("20210920T00000"),
            Err(nom::Err::Error(nom::error::Error {
                input: "20210920T00000",
                code: ErrorKind::Fail
            }))
        );
        assert_eq!(
            datetime("20210920Q000000"),
            Err(nom::Err::Error(nom::error::Error {
                input: "20210920Q000000",
                code: ErrorKind::Fail
            }))
        );

        // TODO FIXME include in tests above
        assert_eq!(
            enddate(""),
            Err(nom::Err::Error(nom::error::Error {
                input: "",
                code: ErrorKind::Eof
            }))
        );
        assert_eq!(
            enddate("20210920T000000F"),
            IResult::Ok((
                "F",
                RRuleDateOrDateTime::DateTime(RRuleDateTime::Unspecified(
                    NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0)
                ))
            ))
        );
        assert_eq!(
            enddate("202109201"),
            IResult::Ok((
                "1",
                RRuleDateOrDateTime::Date(NaiveDate::from_ymd(2021, 9, 20))
            ))
        );

        // just for coverage
        let a = RRuleDateTime::Unspecified(NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0));
        let b = RRuleDateTime::Unspecified(NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 1));
        assert!(a != b);

        let c = RRuleDateOrDateTime::DateTime(RRuleDateTime::Unspecified(
            NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0),
        ));
        let d = RRuleDateOrDateTime::DateTime(RRuleDateTime::Unspecified(
            NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 1),
        ));
        assert!(c != d);

        assert!(Frequency::Minutely == Frequency::Minutely);

        // All examples assume the Eastern United States time zone.

        // Daily for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Daily,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            ..Default::default()
        };
        assert_eq!("RRULE:FREQ=DAILY;COUNT=10", rule.to_string());
        assert_eq!(("", rule), rrule("RRULE:FREQ=DAILY;COUNT=10").unwrap());

        // Daily until December 24, 1997:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Daily,
                    end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                        DateTime::from_utc(NaiveDate::from_ymd(1997, 12, 24).and_hms(0, 0, 0), Utc)
                    ))),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=DAILY;UNTIL=19971224T000000Z").unwrap()
        );

        // Every other day - forever:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Daily,
                    interval: NonZeroU64::new(2).unwrap(),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=DAILY;INTERVAL=2").unwrap()
        );

        // Every 10 days, 5 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Daily,
                    interval: NonZeroU64::new(10).unwrap(),
                    end: RecurEnd::Count(NonZeroU64::new(5).unwrap()),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=DAILY;INTERVAL=10;COUNT=5").unwrap()
        );

        // Every day in January, for 3 years:
        // DTSTART;TZID=America/New_York:19980101T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Yearly,
                    end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                        DateTime::from_utc(NaiveDate::from_ymd(2000, 1, 31).and_hms(14, 0, 0), Utc)
                    ))),
                    bymonth: Some(vec![NonZeroU8::new(1).unwrap()]),
                    byday: Some(vec![
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Sun
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Mon
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Tue
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Wed
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Thu
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Fri
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Sat
                        }
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA")
                .unwrap()
        );
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Daily,
                    bymonth: Some(vec![NonZeroU8::new(1).unwrap()]),
                    end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                        DateTime::from_utc(NaiveDate::from_ymd(2000, 1, 31).and_hms(14, 0, 0), Utc)
                    ))),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1").unwrap()
        );

        // Weekly for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Weekly,
                    end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=WEEKLY;COUNT=10").unwrap()
        );

        // Weekly until December 24, 1997:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Weekly,
                    end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                        DateTime::from_utc(NaiveDate::from_ymd(1997, 12, 24).and_hms(0, 0, 0), Utc)
                    ))),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=WEEKLY;UNTIL=19971224T000000Z").unwrap()
        );

        // Every other week - forever:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Weekly,
                    interval: NonZeroU64::new(2).unwrap(),
                    weekstart: Weekday::Sun,
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU").unwrap()
        );

        // Weekly on Tuesday and Thursday for five weeks:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Weekly,
                    end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                        DateTime::from_utc(NaiveDate::from_ymd(1997, 10, 07).and_hms(0, 0, 0), Utc)
                    ))),
                    weekstart: Weekday::Sun,
                    byday: Some(vec![
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Tue
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Thu
                        },
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH").unwrap()
        );

        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Weekly,
                    end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
                    weekstart: Weekday::Sun,
                    byday: Some(vec![
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Tue
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Thu
                        },
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH").unwrap()
        );

        // Every other week on Monday, Wednesday, and Friday until December 24, 1997, starting on Monday, September 1, 1997:
        // DTSTART;TZID=America/New_York:19970901T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Weekly,
                    interval: NonZeroU64::new(2).unwrap(),
                    end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                        DateTime::from_utc(NaiveDate::from_ymd(1997, 12, 24).and_hms(0, 0, 0), Utc)
                    ))),
                    weekstart: Weekday::Sun,
                    byday: Some(vec![
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Mon
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Wed
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Fri
                        },
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR")
                .unwrap()
        );

        // Every other week on Tuesday and Thursday, for 8 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Weekly,
                    interval: NonZeroU64::new(2).unwrap(),
                    end: RecurEnd::Count(NonZeroU64::new(8).unwrap()),
                    weekstart: Weekday::Sun,
                    byday: Some(vec![
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Tue
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Thu
                        },
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH").unwrap()
        );

        // Monthly on the first Friday for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970905T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
                    byday: Some(vec![WeekdayNum {
                        ordwk: Some(1),
                        weekday: Weekday::Fri
                    },]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR").unwrap()
        );

        // Monthly on the first Friday until December 24, 1997:
        // DTSTART;TZID=America/New_York:19970905T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                        DateTime::from_utc(NaiveDate::from_ymd(1997, 12, 24).and_hms(0, 0, 0), Utc)
                    ))),
                    byday: Some(vec![WeekdayNum {
                        ordwk: Some(1),
                        weekday: Weekday::Fri
                    },]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR").unwrap()
        );

        // Every other month on the first and last Sunday of the month for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970907T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    interval: NonZeroU64::new(2).unwrap(),
                    end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
                    byday: Some(vec![
                        WeekdayNum {
                            ordwk: Some(1),
                            weekday: Weekday::Sun
                        },
                        WeekdayNum {
                            ordwk: Some(-1),
                            weekday: Weekday::Sun
                        }
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU").unwrap()
        );

        // Monthly on the second-to-last Monday of the month for 6 months:
        // DTSTART;TZID=America/New_York:19970922T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    end: RecurEnd::Count(NonZeroU64::new(6).unwrap()),
                    byday: Some(vec![WeekdayNum {
                        ordwk: Some(-2),
                        weekday: Weekday::Mon
                    }]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO").unwrap()
        );

        // Monthly on the third-to-the-last day of the month, forever:
        // DTSTART;TZID=America/New_York:19970928T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    bymonthday: Some(vec![NonZeroI8::new(-3).unwrap()]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;BYMONTHDAY=-3").unwrap()
        );

        // Monthly on the 2nd and 15th of the month for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        // RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
                    bymonthday: Some(vec![
                        NonZeroI8::new(2).unwrap(),
                        NonZeroI8::new(15).unwrap()
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15").unwrap()
        );

        // Monthly on the first and last day of the month for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970930T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
                    bymonthday: Some(vec![
                        NonZeroI8::new(1).unwrap(),
                        NonZeroI8::new(-1).unwrap()
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1").unwrap()
        );

        // Every 18 months on the 10th thru 15th of the month for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970910T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    interval: NonZeroU64::new(18).unwrap(),
                    end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
                    bymonthday: Some(vec![
                        NonZeroI8::new(10).unwrap(),
                        NonZeroI8::new(11).unwrap(),
                        NonZeroI8::new(12).unwrap(),
                        NonZeroI8::new(13).unwrap(),
                        NonZeroI8::new(14).unwrap(),
                        NonZeroI8::new(15).unwrap(),
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15").unwrap()
        );

        // Every Tuesday, every other month:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    interval: NonZeroU64::new(2).unwrap(),
                    byday: Some(vec![WeekdayNum {
                        ordwk: None,
                        weekday: Weekday::Tue
                    }]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU").unwrap()
        );

        // Yearly in June and July for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970610T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Yearly,
                    end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
                    bymonth: Some(vec![NonZeroU8::new(6).unwrap(), NonZeroU8::new(7).unwrap(),]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7").unwrap()
        );

        // Every other year on January, February, and March for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970310T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Yearly,
                    interval: NonZeroU64::new(2).unwrap(),
                    end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
                    bymonth: Some(vec![
                        NonZeroU8::new(1).unwrap(),
                        NonZeroU8::new(2).unwrap(),
                        NonZeroU8::new(3).unwrap(),
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3").unwrap()
        );

        // Every third year on the 1st, 100th, and 200th day for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970101T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Yearly,
                    interval: NonZeroU64::new(3).unwrap(),
                    end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
                    byyearday: Some(vec![
                        NonZeroI16::new(1).unwrap(),
                        NonZeroI16::new(100).unwrap(),
                        NonZeroI16::new(200).unwrap(),
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200").unwrap()
        );

        // Every 20th Monday of the year, forever:
        // DTSTART;TZID=America/New_York:19970519T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Yearly,
                    byday: Some(vec![WeekdayNum {
                        ordwk: Some(20),
                        weekday: Weekday::Mon
                    }]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=YEARLY;BYDAY=20MO").unwrap()
        );

        // Monday of week number 20 (where the default start of the week is Monday), forever:
        // DTSTART;TZID=America/New_York:19970512T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Yearly,
                    byday: Some(vec![WeekdayNum {
                        ordwk: None,
                        weekday: Weekday::Mon
                    }]),
                    byweekno: Some(vec![NonZeroI8::new(20).unwrap(),]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO").unwrap()
        );

        // Every Thursday in March, forever:
        // DTSTART;TZID=America/New_York:19970313T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Yearly,
                    bymonth: Some(vec![NonZeroU8::new(3).unwrap(),]),
                    byday: Some(vec![WeekdayNum {
                        ordwk: None,
                        weekday: Weekday::Thu
                    }]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=TH").unwrap()
        );

        // Every Thursday, but only during June, July, and August, forever:
        // DTSTART;TZID=America/New_York:19970605T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Yearly,
                    bymonth: Some(vec![
                        NonZeroU8::new(6).unwrap(),
                        NonZeroU8::new(7).unwrap(),
                        NonZeroU8::new(8).unwrap(),
                    ]),
                    byday: Some(vec![WeekdayNum {
                        ordwk: None,
                        weekday: Weekday::Thu
                    }]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8").unwrap()
        );

        // Every Friday the 13th, forever:
        // DTSTART;TZID=America/New_York:19970902T090000
        // EXDATE;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    byday: Some(vec![WeekdayNum {
                        ordwk: None,
                        weekday: Weekday::Fri
                    }]),
                    bymonthday: Some(vec![NonZeroI8::new(13).unwrap(),]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13").unwrap()
        );

        // The first Saturday that follows the first Sunday of the month, forever:
        // DTSTART;TZID=America/New_York:19970913T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    byday: Some(vec![WeekdayNum {
                        ordwk: None,
                        weekday: Weekday::Sat
                    }]),
                    bymonthday: Some(vec![
                        NonZeroI8::new(7).unwrap(),
                        NonZeroI8::new(8).unwrap(),
                        NonZeroI8::new(9).unwrap(),
                        NonZeroI8::new(10).unwrap(),
                        NonZeroI8::new(11).unwrap(),
                        NonZeroI8::new(12).unwrap(),
                        NonZeroI8::new(13).unwrap(),
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13").unwrap()
        );

        // Every 4 years, the first Tuesday after a Monday in November, forever (U.S. Presidential Election day):
        // DTSTART;TZID=America/New_York:19961105T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Yearly,
                    interval: NonZeroU64::new(4).unwrap(),
                    bymonth: Some(vec![NonZeroU8::new(11).unwrap(),]),
                    byday: Some(vec![WeekdayNum {
                        ordwk: None,
                        weekday: Weekday::Tue
                    }]),
                    bymonthday: Some(vec![
                        NonZeroI8::new(2).unwrap(),
                        NonZeroI8::new(3).unwrap(),
                        NonZeroI8::new(4).unwrap(),
                        NonZeroI8::new(5).unwrap(),
                        NonZeroI8::new(6).unwrap(),
                        NonZeroI8::new(7).unwrap(),
                        NonZeroI8::new(8).unwrap(),
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8")
                .unwrap()
        );

        // The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months:
        // DTSTART;TZID=America/New_York:19970904T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    end: RecurEnd::Count(NonZeroU64::new(3).unwrap()),
                    byday: Some(vec![
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Tue
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Wed
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Thu
                        }
                    ]),
                    bysetpos: Some(vec![NonZeroI16::new(3).unwrap(),]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3").unwrap()
        );

        //The second-to-last weekday of the month:
        // DTSTART;TZID=America/New_York:19970929T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    byday: Some(vec![
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Mon
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Tue
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Wed
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Thu
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Fri
                        },
                    ]),
                    bysetpos: Some(vec![NonZeroI16::new(-2).unwrap(),]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2").unwrap()
        );

        // Every 3 hours from 9:00 AM to 5:00 PM on a specific day:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Hourly,
                    interval: NonZeroU64::new(3).unwrap(),
                    end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                        DateTime::from_utc(
                            NaiveDate::from_ymd(1997, 09, 02).and_hms(17, 0, 0),
                            Utc
                        )
                    ))),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z").unwrap()
        );

        // Every 15 minutes for 6 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Minutely,
                    interval: NonZeroU64::new(15).unwrap(),
                    end: RecurEnd::Count(NonZeroU64::new(6).unwrap()),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MINUTELY;INTERVAL=15;COUNT=6").unwrap()
        );

        // Every hour and a half for 4 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Minutely,
                    interval: NonZeroU64::new(90).unwrap(),
                    end: RecurEnd::Count(NonZeroU64::new(4).unwrap()),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MINUTELY;INTERVAL=90;COUNT=4").unwrap()
        );

        // Every 20 minutes from 9:00 AM to 4:40 PM every day:
        // DTSTART;TZID=America/New_York:19970902T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Daily,
                    byhour: Some(vec![9, 10, 11, 12, 13, 14, 15, 16]),
                    byminute: Some(vec![0, 20, 40]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40").unwrap()
        );

        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Minutely,
                    interval: NonZeroU64::new(20).unwrap(),
                    byhour: Some(vec![9, 10, 11, 12, 13, 14, 15, 16]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16").unwrap()
        );

        // An example where the days generated makes a difference because of WKST:
        // DTSTART;TZID=America/New_York:19970805T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Weekly,
                    interval: NonZeroU64::new(2).unwrap(),
                    end: RecurEnd::Count(NonZeroU64::new(4).unwrap()),
                    byday: Some(vec![
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Tue
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Sun
                        },
                    ]),
                    weekstart: Weekday::Mon,
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO").unwrap()
        );

        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Weekly,
                    interval: NonZeroU64::new(2).unwrap(),
                    end: RecurEnd::Count(NonZeroU64::new(4).unwrap()),
                    byday: Some(vec![
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Tue
                        },
                        WeekdayNum {
                            ordwk: None,
                            weekday: Weekday::Sun
                        },
                    ]),
                    weekstart: Weekday::Sun,
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU").unwrap()
        );

        // An example where an invalid date (i.e., February 30) is ignored.
        // DTSTART;TZID=America/New_York:20070115T090000
        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Monthly,
                    end: RecurEnd::Count(NonZeroU64::new(5).unwrap()),
                    bymonthday: Some(vec![
                        NonZeroI8::new(15).unwrap(),
                        NonZeroI8::new(30).unwrap(),
                    ]),
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5").unwrap()
        );
    }
}
