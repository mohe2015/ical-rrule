use std::{
    num::{NonZeroU64, NonZeroU8},
    ops::RangeBounds,
    str::FromStr,
};

use chrono::{DateTime, FixedOffset, NaiveDate, NaiveDateTime, TimeZone, Utc, Weekday};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till, take_while1},
    character::complete::digit1,
    combinator::{map_res, opt, verify},
    error::ErrorKind,
    multi::{fold_many0, many0, separated_list0, separated_list1},
    sequence::{preceded, tuple},
    IResult,
};

// The UNTIL or COUNT rule parts are OPTIONAL, but they MUST NOT occur in the same 'recur'.
#[derive(Copy, Clone, Debug)]
pub enum RecurEnd {
    Until(RRuleDateOrDateTime),
    Count(NonZeroU64),
    Forever,
}

#[derive(Copy, Clone, Debug)]
pub struct WeekdayNum {
    //relative: WeekdayRelative,
    pub ordwk: Option<i8>,
    pub weekday: Weekday,
}

#[derive(Clone, Debug)]
pub struct RecurRule {
    freq: Frequency,
    end: RecurEnd,
    interval: NonZeroU64,
    bysecond: Option<Vec<u8>>,
    byminute: Option<Vec<u8>>,
    byhour: Option<Vec<u8>>,
    byday: Option<Vec<WeekdayNum>>,
    bymonthday: Option<Vec<i8>>,
    byyearday: Option<Vec<i16>>,
    byweekno: Option<Vec<i8>>,
    bymonth: Option<Vec<NonZeroU8>>,
    bysetpos: Option<Vec<i16>>,
    weekstart: Weekday,
}

impl Default for RecurRule {
    fn default() -> Self {
        RecurRule {
            freq: Frequency::Daily, // TODO FIXME no default
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
    Bymonthday(Vec<i8>),
    Byyearday(Vec<i16>),
    Byweekno(Vec<i8>),
    Bymonth(Vec<NonZeroU8>),
    Bysetpos(Vec<i16>),
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
pub enum Frequency {
    Secondly,
    Minutely,
    Hourly,
    Daily,
    Weekly,
    Monthly,
    Yearly,
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
        verify(map_res(digit1, |v: &str| v.parse::<U>()), |v: &U| {
            range.contains(v)
        })(input)
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
                separated_list0(tag(","), digits(0..=23)),
            ),
            RecurRulePart::Bymonthday,
        ),
        nom::combinator::map(
            preceded(
                tag("BYYEARDAY="),
                separated_list0(tag(","), digits(0_i16..=23_i16)),
            ),
            RecurRulePart::Byyearday,
        ),
        nom::combinator::map(
            preceded(
                tag("BYWEEKNO="),
                separated_list0(tag(","), digits(0_i8..=23_i8)),
            ),
            RecurRulePart::Byweekno,
        ),
        nom::combinator::map(
            preceded(
                tag("BYMONTH="),
                separated_list0(
                    tag(","),
                    digits(
                        std::num::NonZeroU8::new(1).unwrap()..=std::num::NonZeroU8::new(1).unwrap(),
                    ),
                ),
            ),
            RecurRulePart::Bymonth,
        ),
        nom::combinator::map(
            preceded(
                tag("BYWEEKNO="),
                separated_list0(tag(","), digits(0_i16..=23_i16)),
            ),
            RecurRulePart::Bysetpos,
        ),
        nom::combinator::map(preceded(tag("WKST="), weekday), RecurRulePart::Weekstart),
    ))(input)
}

pub fn recur(input: &str) -> IResult<&str, RecurRule> {
    // the first thing needs to be frequency so do
    let (input, freq) = preceded(tag("FREQ="), freq)(input)?;

    // TODO FIXME this probably allocates unecessarily - can't use fold_many0 because we need separated? maybe separated could return iterator?
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
    println!("{:?}", x);
    x
}

//pub fn quoted_string(input: &str) -> IResult<&str, &str> {}

#[cfg(test)]
mod tests {
    use chrono::{DateTime, NaiveDate, Utc};
    use nom::{error::ErrorKind, IResult};

    use crate::parser::{
        constant_rrule, date, datetime, enddate, freq, iana_param, iana_token, other_param,
        param_value, paramtext, rrulparams, Frequency, RRuleDateOrDateTime, RRuleDateTime,
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
        print!("{:?}", a);

        let c = RRuleDateOrDateTime::DateTime(RRuleDateTime::Unspecified(
            NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0),
        ));
        let d = RRuleDateOrDateTime::DateTime(RRuleDateTime::Unspecified(
            NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 1),
        ));
        assert!(c != d);
        print!("{:?}", c);

        assert!(Frequency::Minutely == Frequency::Minutely);
        print!("{:?}", Frequency::Minutely);
    }
}
