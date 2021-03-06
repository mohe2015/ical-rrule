pub mod arbitrary_enums;
pub mod chrono_utils;
pub mod enum_utils;
pub mod frequency;
pub mod recur_rule;

use std::{ops::RangeBounds, str::FromStr};

use crate::weekday::Weekday;
use nom::{
    bytes::complete::{tag, take_till, take_while, take_while1},
    combinator::{map_res, verify},
    multi::{many0, separated_list1},
    IResult,
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct WeekdayNum {
    pub ordwk: Option<i8>,
    pub weekday: Weekday,
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

#[cfg(test)]
mod tests {
    use std::num::{NonZeroI16, NonZeroI8, NonZeroU64, NonZeroU8};

    use arbitrary::{Arbitrary, Unstructured};
    use chrono::{DateTime, NaiveDate, Utc};
    use nom::{error::ErrorKind, IResult};
    use rand::{Rng, RngCore};

    use crate::parser::{
        chrono_utils::{date, datetime, enddate, RRuleDateOrDateTime, RRuleDateTime, RecurEnd},
        constant_rrule,
        frequency::{freq, Frequency},
        iana_param, iana_token, other_param, param_value, paramtext,
        recur_rule::{rrule, RecurRule},
        rrulparams, Weekday, WeekdayNum,
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
            assert_eq!(fun("??"), IResult::Ok(("", "??")));
            assert_eq!(fun("?????"), IResult::Ok(("", "?????")));
            assert_eq!(fun("\""), IResult::Ok(("\"", "")));
        }

        for fun in [iana_param, other_param] {
            assert_eq!(fun("TEST=1"), IResult::Ok(("", ("TEST", vec!["1"]))));
            assert_eq!(
                fun("1-tEST=??df,??sldifh"),
                IResult::Ok(("", ("1-tEST", vec!["??df", "??sldifh"])))
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
                code: ErrorKind::Eof
            }))
        );
        assert_eq!(
            datetime("20210920Q000000"),
            Err(nom::Err::Error(nom::error::Error {
                input: "",
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
    }

    fn check(rule: RecurRule, to_string: &str) {
        assert_eq!(to_string, rule.to_string());
        assert_eq!(("", rule), rrule(to_string).unwrap());
    }

    #[test]
    fn examples() {
        // All examples assume the Eastern United States time zone.

        // Daily for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Daily,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=DAILY;COUNT=10");

        // Daily until December 24, 1997:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Daily,
            end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                DateTime::from_utc(NaiveDate::from_ymd(1997, 12, 24).and_hms(0, 0, 0), Utc),
            ))),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=DAILY;UNTIL=19971224T000000Z");

        // Every other day - forever:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Daily,
            interval: NonZeroU64::new(2).unwrap(),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=DAILY;INTERVAL=2");

        // Every 10 days, 5 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Daily,
            interval: NonZeroU64::new(10).unwrap(),
            end: RecurEnd::Count(NonZeroU64::new(5).unwrap()),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=DAILY;INTERVAL=10;COUNT=5");

        // Every day in January, for 3 years:
        // DTSTART;TZID=America/New_York:19980101T090000
        let rule = RecurRule {
            freq: Frequency::Yearly,
            end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                DateTime::from_utc(NaiveDate::from_ymd(2000, 1, 31).and_hms(14, 0, 0), Utc),
            ))),
            bymonth: Some(vec![NonZeroU8::new(1).unwrap()]),
            byday: Some(vec![
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Sun,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Mon,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Tue,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Wed,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Thu,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Fri,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Sat,
                },
            ]),
            ..Default::default()
        };
        check(
            rule,
            "RRULE:FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA",
        );

        let rule = RecurRule {
            freq: Frequency::Daily,
            bymonth: Some(vec![NonZeroU8::new(1).unwrap()]),
            end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                DateTime::from_utc(NaiveDate::from_ymd(2000, 1, 31).and_hms(14, 0, 0), Utc),
            ))),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1");

        // Weekly for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Weekly,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=WEEKLY;COUNT=10");

        // Weekly until December 24, 1997:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Weekly,
            end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                DateTime::from_utc(NaiveDate::from_ymd(1997, 12, 24).and_hms(0, 0, 0), Utc),
            ))),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=WEEKLY;UNTIL=19971224T000000Z");

        // Every other week - forever:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Weekly,
            interval: NonZeroU64::new(2).unwrap(),
            weekstart: Weekday::Sun,
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU");

        // Weekly on Tuesday and Thursday for five weeks:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Weekly,
            end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                DateTime::from_utc(NaiveDate::from_ymd(1997, 10, 7).and_hms(0, 0, 0), Utc),
            ))),
            weekstart: Weekday::Sun,
            byday: Some(vec![
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Tue,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Thu,
                },
            ]),
            ..Default::default()
        };
        check(
            rule,
            "RRULE:FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH",
        );

        let rule = RecurRule {
            freq: Frequency::Weekly,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            weekstart: Weekday::Sun,
            byday: Some(vec![
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Tue,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Thu,
                },
            ]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH");

        // Every other week on Monday, Wednesday, and Friday until December 24, 1997, starting on Monday, September 1, 1997:
        // DTSTART;TZID=America/New_York:19970901T090000
        let rule = RecurRule {
            freq: Frequency::Weekly,
            interval: NonZeroU64::new(2).unwrap(),
            end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                DateTime::from_utc(NaiveDate::from_ymd(1997, 12, 24).and_hms(0, 0, 0), Utc),
            ))),
            weekstart: Weekday::Sun,
            byday: Some(vec![
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Mon,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Wed,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Fri,
                },
            ]),
            ..Default::default()
        };
        check(
            rule,
            "RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR",
        );

        // Every other week on Tuesday and Thursday, for 8 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Weekly,
            interval: NonZeroU64::new(2).unwrap(),
            end: RecurEnd::Count(NonZeroU64::new(8).unwrap()),
            weekstart: Weekday::Sun,
            byday: Some(vec![
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Tue,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Thu,
                },
            ]),
            ..Default::default()
        };
        check(
            rule,
            "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH",
        );

        // Monthly on the first Friday for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970905T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            byday: Some(vec![WeekdayNum {
                ordwk: Some(1),
                weekday: Weekday::Fri,
            }]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR");

        // Monthly on the first Friday until December 24, 1997:
        // DTSTART;TZID=America/New_York:19970905T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                DateTime::from_utc(NaiveDate::from_ymd(1997, 12, 24).and_hms(0, 0, 0), Utc),
            ))),
            byday: Some(vec![WeekdayNum {
                ordwk: Some(1),
                weekday: Weekday::Fri,
            }]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR");

        // Every other month on the first and last Sunday of the month for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970907T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            interval: NonZeroU64::new(2).unwrap(),
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            byday: Some(vec![
                WeekdayNum {
                    ordwk: Some(1),
                    weekday: Weekday::Sun,
                },
                WeekdayNum {
                    ordwk: Some(-1),
                    weekday: Weekday::Sun,
                },
            ]),
            ..Default::default()
        };
        check(
            rule,
            "RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU",
        );

        // Monthly on the second-to-last Monday of the month for 6 months:
        // DTSTART;TZID=America/New_York:19970922T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            end: RecurEnd::Count(NonZeroU64::new(6).unwrap()),
            byday: Some(vec![WeekdayNum {
                ordwk: Some(-2),
                weekday: Weekday::Mon,
            }]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO");

        // Monthly on the third-to-the-last day of the month, forever:
        // DTSTART;TZID=America/New_York:19970928T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            bymonthday: Some(vec![NonZeroI8::new(-3).unwrap()]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MONTHLY;BYMONTHDAY=-3");

        // Monthly on the 2nd and 15th of the month for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        // RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15
        let rule = RecurRule {
            freq: Frequency::Monthly,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            bymonthday: Some(vec![
                NonZeroI8::new(2).unwrap(),
                NonZeroI8::new(15).unwrap(),
            ]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15");

        // Monthly on the first and last day of the month for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970930T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            bymonthday: Some(vec![
                NonZeroI8::new(1).unwrap(),
                NonZeroI8::new(-1).unwrap(),
            ]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1");

        // Every 18 months on the 10th thru 15th of the month for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970910T090000
        let rule = RecurRule {
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
        };
        check(
            rule,
            "RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15",
        );

        // Every Tuesday, every other month:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            interval: NonZeroU64::new(2).unwrap(),
            byday: Some(vec![WeekdayNum {
                ordwk: None,
                weekday: Weekday::Tue,
            }]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU");

        // Yearly in June and July for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970610T090000
        let rule = RecurRule {
            freq: Frequency::Yearly,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            bymonth: Some(vec![NonZeroU8::new(6).unwrap(), NonZeroU8::new(7).unwrap()]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7");

        // Every other year on January, February, and March for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970310T090000
        let rule = RecurRule {
            freq: Frequency::Yearly,
            interval: NonZeroU64::new(2).unwrap(),
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            bymonth: Some(vec![
                NonZeroU8::new(1).unwrap(),
                NonZeroU8::new(2).unwrap(),
                NonZeroU8::new(3).unwrap(),
            ]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3");

        // Every third year on the 1st, 100th, and 200th day for 10 occurrences:
        // DTSTART;TZID=America/New_York:19970101T090000
        let rule = RecurRule {
            freq: Frequency::Yearly,
            interval: NonZeroU64::new(3).unwrap(),
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            byyearday: Some(vec![
                NonZeroI16::new(1).unwrap(),
                NonZeroI16::new(100).unwrap(),
                NonZeroI16::new(200).unwrap(),
            ]),
            ..Default::default()
        };
        check(
            rule,
            "RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200",
        );

        // Every 20th Monday of the year, forever:
        // DTSTART;TZID=America/New_York:19970519T090000
        let rule = RecurRule {
            freq: Frequency::Yearly,
            byday: Some(vec![WeekdayNum {
                ordwk: Some(20),
                weekday: Weekday::Mon,
            }]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=YEARLY;BYDAY=20MO");

        // Monday of week number 20 (where the default start of the week is Monday), forever:
        // DTSTART;TZID=America/New_York:19970512T090000
        let rule = RecurRule {
            freq: Frequency::Yearly,
            byday: Some(vec![WeekdayNum {
                ordwk: None,
                weekday: Weekday::Mon,
            }]),
            byweekno: Some(vec![NonZeroI8::new(20).unwrap()]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO");

        // Every Thursday in March, forever:
        // DTSTART;TZID=America/New_York:19970313T090000
        let rule = RecurRule {
            freq: Frequency::Yearly,
            bymonth: Some(vec![NonZeroU8::new(3).unwrap()]),
            byday: Some(vec![WeekdayNum {
                ordwk: None,
                weekday: Weekday::Thu,
            }]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=TH");

        // Every Thursday, but only during June, July, and August, forever:
        // DTSTART;TZID=America/New_York:19970605T090000
        let rule = RecurRule {
            freq: Frequency::Yearly,
            bymonth: Some(vec![
                NonZeroU8::new(6).unwrap(),
                NonZeroU8::new(7).unwrap(),
                NonZeroU8::new(8).unwrap(),
            ]),
            byday: Some(vec![WeekdayNum {
                ordwk: None,
                weekday: Weekday::Thu,
            }]),
            ..Default::default()
        };
        assert_eq!("RRULE:FREQ=YEARLY;BYMONTH=6,7,8;BYDAY=TH", rule.to_string());
        assert_eq!(
            ("", rule),
            rrule("RRULE:FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8").unwrap()
        );

        // Every Friday the 13th, forever:
        // DTSTART;TZID=America/New_York:19970902T090000
        // EXDATE;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            byday: Some(vec![WeekdayNum {
                ordwk: None,
                weekday: Weekday::Fri,
            }]),
            bymonthday: Some(vec![NonZeroI8::new(13).unwrap()]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13");

        // The first Saturday that follows the first Sunday of the month, forever:
        // DTSTART;TZID=America/New_York:19970913T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            byday: Some(vec![WeekdayNum {
                ordwk: None,
                weekday: Weekday::Sat,
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
        };
        check(
            rule,
            "RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13",
        );

        // Every 4 years, the first Tuesday after a Monday in November, forever (U.S. Presidential Election day):
        // DTSTART;TZID=America/New_York:19961105T090000
        let rule = RecurRule {
            freq: Frequency::Yearly,
            interval: NonZeroU64::new(4).unwrap(),
            bymonth: Some(vec![NonZeroU8::new(11).unwrap()]),
            byday: Some(vec![WeekdayNum {
                ordwk: None,
                weekday: Weekday::Tue,
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
        };
        check(
            rule,
            "RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8",
        );

        // The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months:
        // DTSTART;TZID=America/New_York:19970904T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            end: RecurEnd::Count(NonZeroU64::new(3).unwrap()),
            byday: Some(vec![
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Tue,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Wed,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Thu,
                },
            ]),
            bysetpos: Some(vec![NonZeroI16::new(3).unwrap()]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3");

        //The second-to-last weekday of the month:
        // DTSTART;TZID=America/New_York:19970929T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            byday: Some(vec![
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Mon,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Tue,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Wed,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Thu,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Fri,
                },
            ]),
            bysetpos: Some(vec![NonZeroI16::new(-2).unwrap()]),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2");

        // Every 3 hours from 9:00 AM to 5:00 PM on a specific day:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Hourly,
            interval: NonZeroU64::new(3).unwrap(),
            end: RecurEnd::Until(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
                DateTime::from_utc(NaiveDate::from_ymd(1997, 9, 2).and_hms(17, 0, 0), Utc),
            ))),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z");

        // Every 15 minutes for 6 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Minutely,
            interval: NonZeroU64::new(15).unwrap(),
            end: RecurEnd::Count(NonZeroU64::new(6).unwrap()),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MINUTELY;INTERVAL=15;COUNT=6");

        // Every hour and a half for 4 occurrences:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Minutely,
            interval: NonZeroU64::new(90).unwrap(),
            end: RecurEnd::Count(NonZeroU64::new(4).unwrap()),
            ..Default::default()
        };
        check(rule, "RRULE:FREQ=MINUTELY;INTERVAL=90;COUNT=4");

        // Every 20 minutes from 9:00 AM to 4:40 PM every day:
        // DTSTART;TZID=America/New_York:19970902T090000
        let rule = RecurRule {
            freq: Frequency::Daily,
            byhour: Some(vec![9, 10, 11, 12, 13, 14, 15, 16]),
            byminute: Some(vec![0, 20, 40]),
            ..Default::default()
        };
        check(
            rule,
            "RRULE:FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40",
        );

        let rule = RecurRule {
            freq: Frequency::Minutely,
            interval: NonZeroU64::new(20).unwrap(),
            byhour: Some(vec![9, 10, 11, 12, 13, 14, 15, 16]),
            ..Default::default()
        };
        check(
            rule,
            "RRULE:FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16",
        );

        // An example where the days generated makes a difference because of WKST:
        // DTSTART;TZID=America/New_York:19970805T090000
        let rule = RecurRule {
            freq: Frequency::Weekly,
            interval: NonZeroU64::new(2).unwrap(),
            end: RecurEnd::Count(NonZeroU64::new(4).unwrap()),
            byday: Some(vec![
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Tue,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Sun,
                },
            ]),
            weekstart: Weekday::Mon,
            ..Default::default()
        };
        assert_eq!(
            "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU",
            rule.to_string()
        );
        assert_eq!(
            ("", rule),
            rrule("RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO").unwrap()
        );

        let rule = RecurRule {
            freq: Frequency::Weekly,
            interval: NonZeroU64::new(2).unwrap(),
            end: RecurEnd::Count(NonZeroU64::new(4).unwrap()),
            byday: Some(vec![
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Tue,
                },
                WeekdayNum {
                    ordwk: None,
                    weekday: Weekday::Sun,
                },
            ]),
            weekstart: Weekday::Sun,
            ..Default::default()
        };
        assert_eq!(
            "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;WKST=SU;BYDAY=TU,SU",
            rule.to_string()
        );
        assert_eq!(
            ("", rule),
            rrule("RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU").unwrap()
        );

        // An example where an invalid date (i.e., February 30) is ignored.
        // DTSTART;TZID=America/New_York:20070115T090000
        let rule = RecurRule {
            freq: Frequency::Monthly,
            end: RecurEnd::Count(NonZeroU64::new(5).unwrap()),
            bymonthday: Some(vec![
                NonZeroI8::new(15).unwrap(),
                NonZeroI8::new(30).unwrap(),
            ]),
            ..Default::default()
        };
        assert_eq!(
            "RRULE:FREQ=MONTHLY;COUNT=5;BYMONTHDAY=15,30",
            rule.to_string()
        );
        assert_eq!(
            ("", rule),
            rrule("RRULE:FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5").unwrap()
        );

        // coverage rules
        let rule1 = RecurRule {
            freq: Frequency::Secondly,
            bysecond: Some(vec![]),
            end: RecurEnd::Until(
                RRuleDateOrDateTime::DateTime(
                    RRuleDateTime::Unspecified(NaiveDate::from_ymd(1997, 12, 24).and_hms(0, 0, 0))
                        .clone(),
                )
                .clone(),
            )
            .clone(),
            byday: Some(vec![WeekdayNum {
                ordwk: None,
                weekday: Weekday::Mon,
            }
            .clone()]),
            ..Default::default()
        };
        check(
            rule1.clone(),
            "RRULE:FREQ=SECONDLY;UNTIL=19971224T000000;BYSECOND=;BYDAY=MO",
        );
        format!("{:?}", rule1);

        let rule = RecurRule {
            freq: Frequency::Secondly,
            end: RecurEnd::Until(RRuleDateOrDateTime::Date(NaiveDate::from_ymd(1997, 12, 24))),
            ..Default::default()
        };
        assert!(rule != rule1);
        check(rule, "RRULE:FREQ=SECONDLY;UNTIL=19971224");

        assert_eq!(
            (
                "",
                RecurRule {
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=YEARLY;FREQ=YEARLY").unwrap()
        );

        assert_eq!(
            (
                "",
                RecurRule {
                    freq: Frequency::Minutely,
                    ..Default::default()
                }
            ),
            rrule("RRULE:FREQ=MINUTELY;FREQ=YEARLY").unwrap()
        );

        assert!(
            WeekdayNum {
                ordwk: None,
                weekday: Weekday::Mon
            } != WeekdayNum {
                ordwk: None,
                weekday: Weekday::Tue
            }
        );

        // to get coverage in arbitary impl because coverage is stupid and doesn't ignore it when the feature is disabled
        for _ in 0..10240 {
            loop {
                let mut data = vec![0; rand::thread_rng().gen_range(0..1024)];
                rand::thread_rng().fill_bytes(&mut data);

                // Wrap it in an `Unstructured`.
                let mut unstructured = Unstructured::new(&data);

                if let Ok(recur_rule) = RecurRule::arbitrary(&mut unstructured) {
                    let val = recur_rule.to_string();
                    let parsed = rrule(&val);
                    assert_eq!(Ok(("", recur_rule)), parsed);
                    break;
                } else {
                    println!("random generation failed - trying again");
                }

                let mut unstructured = Unstructured::new(&data);
                let _ = unstructured.arbitrary_len::<Weekday>();
                let _ = unstructured.arbitrary_len::<RecurEnd>();
                let _ = unstructured.arbitrary_len::<Frequency>();
                let a = unstructured.arbitrary_take_rest_iter::<Weekday>();
                if let Ok(mut b) = a {
                    let _ = b.next();
                }
                let unstructured = Unstructured::new(&data);
                let a = unstructured.arbitrary_take_rest_iter::<RecurEnd>();
                if let Ok(mut b) = a {
                    let _ = b.next();
                }
                let unstructured = Unstructured::new(&data);
                let a = unstructured.arbitrary_take_rest_iter::<Frequency>();
                if let Ok(mut b) = a {
                    let _ = b.next();
                }
            }
        }
    }
}
