use std::num::{NonZeroI64, NonZeroI8, NonZeroU8};

use chrono::{DateTime, Datelike, Duration, NaiveDate, TimeZone, Timelike, Utc, format::Parsed};

use crate::parser::{frequency::Frequency, recur_rule::RecurRule};

#[derive(Clone)]
pub struct RRule {
    pub(crate) rrule: RecurRule,
    pub(crate) dtstart: MaybeInvalidDateTime,
}

impl RRule {
    fn bymonth(self: &RRule) -> Vec<NonZeroU8> {
        match &self.rrule.bymonth {
            Some(v) => v.clone(),
            None => vec![NonZeroU8::new((self.dtstart.month0 + 1).try_into().unwrap()).unwrap()],
        }
    }

    fn byweekno(self: &RRule) -> Vec<NonZeroI8> {
        match &self.rrule.byweekno {
            Some(v) => v.clone(),
            None => vec![],
        }
    }

    fn byhour(self: &RRule) -> Vec<u8> {
        match &self.rrule.byhour {
            Some(v) => v.clone(),
            None => vec![self.dtstart.hour as u8],
        }
    }

    fn byminute(self: &RRule) -> Vec<u8> {
        match &self.rrule.byminute {
            Some(v) => v.clone(),
            None => vec![self.dtstart.minute as u8],
        }
    }

    fn bysecond(self: &RRule) -> Vec<u8> {
        match &self.rrule.bysecond {
            Some(v) => v.clone(),
            None => vec![self.dtstart.second as u8],
        }
    }
}

#[derive(Clone, Copy)]
pub struct MaybeInvalidDateTime {
    second: u32,
    minute: u32,
    hour: u32,
    day: u32,
    month0: u32,
    year: i32,
}

impl<T: TimeZone> From<DateTime<T>> for MaybeInvalidDateTime {
    fn from(datetime: DateTime<T>) -> MaybeInvalidDateTime {
        MaybeInvalidDateTime {
            second: datetime.second(),
            minute: datetime.minute(),
            hour: datetime.hour(),
            day: datetime.day(),
            // weeks
            month0: datetime.month0(),
            year: datetime.year(),
        }
    }
}

impl From<MaybeInvalidDateTime> for DateTime<Utc> {
    fn from(datetime: MaybeInvalidDateTime) -> DateTime<Utc> {
        DateTime::from_utc(
            NaiveDate::from_ymd(datetime.year, datetime.month0 + 1, datetime.day).and_hms(
                datetime.hour,
                datetime.minute,
                datetime.second,
            ),
            Utc,
        )
    }
}

pub fn add_month(mut datetime: MaybeInvalidDateTime, interval: u32) -> MaybeInvalidDateTime {
    datetime.month0 = (datetime.month0 + interval) % 12;
    datetime.year += interval as i32 / 12_i32;
    datetime
}

pub fn add_year(mut datetime: MaybeInvalidDateTime, interval: i32) -> MaybeInvalidDateTime {
    datetime.year += interval;
    datetime
}

impl Iterator for RRule {
    type Item = MaybeInvalidDateTime;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO FIXME honor until/count
        //let dtstart = date_or_datetime_to_utc(rrule.dtstart);

        let test: NonZeroI64 = self.rrule.interval.into();
        let test2: u32 = self.rrule.interval.into();

        let new = match self.rrule.freq {
            crate::parser::frequency::Frequency::Secondly => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::seconds(test.into())).into()
            }
            crate::parser::frequency::Frequency::Minutely => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::minutes(test.into())).into()
            }
            crate::parser::frequency::Frequency::Hourly => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::hours(test.into())).into()
            }
            crate::parser::frequency::Frequency::Daily => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::days(test.into())).into()
            }
            crate::parser::frequency::Frequency::Weekly => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::weeks(test.into())).into()
            }
            crate::parser::frequency::Frequency::Monthly => {
                add_month(self.dtstart, self.rrule.interval.into())
            } // not easy maybe month doesnt have that day
            crate::parser::frequency::Frequency::Yearly => add_year(self.dtstart, test2 as i32), //dtstart + Duration::years(test.into()), // not easy february is an asshole
                                                                                                 // even worse as when by* is used this could probably create valid shit?
        };
        self.dtstart = new;

        Some(new)
    }
}

pub fn complete_implementation<'a>(
    rrule: &'a RRule,
) -> impl Iterator<Item = MaybeInvalidDateTime> + 'a {
    // BYMONTH
    let it1 = rrule
        .clone() // don't look here
        .map(|f| {
            if rrule.rrule.freq > Frequency::Monthly {
                rrule
                    .bymonth()
                    .iter()
                    .map(|s| {
                        let mut dupe = f;
                        let tmp: u8 = (*s).into();
                        dupe.month0 = tmp as u32;
                        dupe
                    })
                    .collect::<Vec<_>>()
            } else if rrule
                .bymonth()
                .contains(&NonZeroU8::new(f.month0 as u8).unwrap())
            {
                vec![f]
            } else {
                vec![]
            }
        })
        .flatten();

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYWEEKNO  |N/A     |N/A     |N/A    |N/A    |N/A   |N/A    |Expand|
    let it2 = it1.flat_map(|f| {
        if rrule.rrule.freq == Frequency::Yearly {
            rrule
                .byweekno()
                .iter()
                .map(|s| {
                    let mut parsed = Parsed::new();
                    parsed.set_year(f.year.into()).unwrap();
                    parsed.set_isoweek(i8::from(*s) as i64).unwrap();
                    parsed.to_datetime().unwrap().into()
                })
                .collect::<Vec<_>>()
        } else {
            vec![f]
        }
    });

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYYEARDAY |Limit   |Limit   |Limit  |N/A    |N/A   |N/A    |Expand|

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYMONTHDAY|Limit   |Limit   |Limit  |Limit  |N/A   |Expand |Expand|

    // TODO FIXME BYDAY

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYHOUR    |Limit   |Limit   |Limit  |Expand |Expand|Expand |Expand|
    let it3 = it2.flat_map(|f| {
        if rrule.rrule.freq > Frequency::Hourly {
            rrule
                .byhour()
                .iter()
                .map(|s| {
                    let mut dupe = f;
                    dupe.hour = *s as u32;
                    dupe
                })
                .collect::<Vec<_>>()
        } else if rrule.byhour().contains(&(f.hour as u8)) {
            vec![f]
        } else {
            vec![]
        }
    });

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYMINUTE  |Limit   |Limit   |Expand |Expand |Expand|Expand |Expand|
    let it4 = it3.flat_map(|f| {
        if rrule.rrule.freq > Frequency::Minutely {
            rrule
                .byminute()
                .iter()
                .map(|s| {
                    let mut dupe = f;
                    dupe.minute = *s as u32;
                    dupe
                })
                .collect::<Vec<_>>()
        } else if rrule.byminute().contains(&(f.minute as u8)) {
            vec![f]
        } else {
            vec![]
        }
    });

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYSECOND  |Limit   |Expand  |Expand |Expand |Expand|Expand |Expand|
    let it9 = it4.flat_map(|f| {
        if rrule.rrule.freq > Frequency::Secondly {
            rrule
                .bysecond()
                .iter()
                .map(|s| {
                    let mut dupe = f;
                    dupe.second = *s as u32;
                    dupe
                })
                .collect::<Vec<_>>()
        } else if rrule.bysecond().contains(&(f.second as u8)) {
            vec![f]
        } else {
            vec![]
        }
    });

    // TODO bysetpos

    it9
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroU64;

    use chrono::{DateTime, NaiveDate, Utc};

    use crate::parser::{
        chrono_utils::{date_or_datetime_to_utc, RRuleDateOrDateTime, RRuleDateTime, RecurEnd},
        frequency::Frequency,
        recur_rule::RecurRule,
    };

    use super::{complete_implementation, RRule};

    #[test]
    fn examples_interpreter() {
        let dtstart = date_or_datetime_to_utc(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
            DateTime::from_utc(NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0), Utc),
        )))
        .into();
        let rrule = RecurRule {
            freq: Frequency::Daily,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            ..Default::default()
        };
        let rrule = RRule { dtstart, rrule };
        for date in rrule.clone().take(10) {
            println!("{}", Into::<DateTime<Utc>>::into(date));
        }

        println!("------------------------");

        let dtstart = date_or_datetime_to_utc(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
            DateTime::from_utc(NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0), Utc),
        )))
        .into();
        let rrule = RecurRule {
            freq: Frequency::Daily,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            bysecond: Some(vec![1, 2]),
            ..Default::default()
        };
        let rrule = RRule { dtstart, rrule };
        for date in complete_implementation(&rrule).take(10) {
            println!("{}", Into::<DateTime<Utc>>::into(date));
        }

        println!("------------------------");

        let dtstart = date_or_datetime_to_utc(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
            DateTime::from_utc(NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0), Utc),
        )))
        .into();
        let rrule = RecurRule {
            freq: Frequency::Secondly,
            end: RecurEnd::Count(NonZeroU64::new(10).unwrap()),
            bysecond: Some(vec![1, 2]),
            ..Default::default()
        };
        let rrule = RRule { dtstart, rrule };
        for date in complete_implementation(&rrule).take(10) {
            println!("{}", Into::<DateTime<Utc>>::into(date));
        }
    }
}
