use chrono::{format::Parsed, DateTime, Datelike, Duration, NaiveDate, TimeZone, Timelike, Utc};

use crate::parser::{
    chrono_utils::{date_or_datetime_to_utc, RecurEnd},
    frequency::Frequency,
    recur_rule::RecurRule,
};

#[derive(Clone)]
pub struct RRule {
    pub(crate) rrule: RecurRule,
    pub(crate) dtstart: MaybeInvalidDateTime,
}

impl RRule {
    fn bymonthday(self: &RRule) -> Vec<i8> {
        match &self.rrule.bymonthday {
            Some(v) => v.clone(),
            None => vec![],
        }
    }

    fn byyearday(self: &RRule) -> Vec<i16> {
        match &self.rrule.byyearday {
            Some(v) => v.clone(),
            None => vec![],
        }
    }

    fn bymonth(self: &RRule) -> Vec<u8> {
        match &self.rrule.bymonth {
            Some(v) => v.clone(),
            None => vec![(self.dtstart.month0 + 1).try_into().unwrap()],
        }
    }
    /*
        fn byweekno(self: &RRule) -> Vec<i8> {
            match &self.rrule.byweekno {
                Some(v) => v.clone(),
                None => vec![],
            }
        }
    */
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

        let test: i64 = self.rrule.interval.into();
        let test2: u32 = self.rrule.interval;

        let new = match self.rrule.freq {
            crate::parser::frequency::Frequency::Secondly => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::seconds(test)).into()
            }
            crate::parser::frequency::Frequency::Minutely => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::minutes(test)).into()
            }
            crate::parser::frequency::Frequency::Hourly => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::hours(test)).into()
            }
            crate::parser::frequency::Frequency::Daily => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::days(test)).into()
            }
            crate::parser::frequency::Frequency::Weekly => {
                (Into::<DateTime<Utc>>::into(self.dtstart) + Duration::weeks(test)).into()
            }
            crate::parser::frequency::Frequency::Monthly => {
                add_month(self.dtstart, self.rrule.interval)
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
) -> Box<dyn Iterator<Item = DateTime<Utc>> + 'a> {
    let dtstart = rrule.dtstart;
    // TODO FIXME one of these probably removes all elements and therefore the iterator never yields an element

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYMONTH   |Limit   |Limit   |Limit  |Limit  |Limit |Limit  |Expand|
    let it_bymonth = rrule
        .clone() // don't look here
        .map(|f| {
            if rrule.rrule.bymonth.is_some() {
                if rrule.rrule.freq > Frequency::Monthly {
                    rrule
                        .bymonth()
                        .iter()
                        .map(|s| {
                            let mut dupe = f;
                            let tmp: u8 = *s;
                            dupe.month0 = tmp as u32;
                            dupe
                        })
                        .collect::<Vec<_>>()
                } else if rrule.bymonth().contains(&(f.month0 as u8)) {
                    vec![f]
                } else {
                    vec![]
                }
            } else {
                vec![f]
            }
        })
        .flatten();

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYWEEKNO  |N/A     |N/A     |N/A    |N/A    |N/A   |N/A    |Expand|
    /*
        let it2 = it1.flat_map(|f| {
            if rrule.rrule.byweekno.is_some() {
            if rrule.rrule.freq == Frequency::Yearly {
                rrule
                    .byweekno()
                    .iter()
                    .map(|s| {
                        let mut parsed = Parsed::new();
                        parsed.set_year(f.year.into()).unwrap();                    parsed.set_isoweek(i8::from(*s) as i64).unwrap();
                        parsed.to_datetime().unwrap().into()
                    })
                    .collect::<Vec<_>>()
            } else {
                vec![f]
            }
            } else {
                vec![f]
            }
        });
    */
    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYYEARDAY |Limit   |Limit   |Limit  |N/A    |N/A   |N/A    |Expand|
    let it_byyearday = it_bymonth.flat_map(|f| {
        if rrule.rrule.byyearday.is_some() {
            if rrule.rrule.freq == Frequency::Yearly {
                // expand
                rrule
                    .byyearday()
                    .iter()
                    .map(|s| {
                        let mut parsed = Parsed::new();
                        // TODO FIXME negative values
                        parsed.set_ordinal((*s).into()).unwrap();
                        // TODO FIXME skip out of range values
                        parsed.to_datetime().unwrap().into()
                    })
                    .collect::<Vec<_>>()
            } else if rrule.rrule.freq <= Frequency::Hourly
                && !rrule
                    .byyearday()
                    .contains(&(DateTime::<Utc>::from(f).ordinal().try_into().unwrap()))
            {
                // TODO FIXME what if no BYHOUR is set but freq is e.g. secondly? probably then this is wrong
                vec![]
            } else {
                vec![f]
            }
        } else {
            vec![f]
        }
    });

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYMONTHDAY|Limit   |Limit   |Limit  |Limit  |N/A   |Expand |Expand|
    let it_bymonthday = it_byyearday.flat_map(|f| {
        if rrule.rrule.bymonthday.is_some() {
            if rrule.rrule.freq > Frequency::Hourly {
                // expand
                rrule
                    .bymonthday()
                    .iter()
                    .map(|s| {
                        let mut dupe = f;
                        // TODO FIXME negative values
                        dupe.day = (*s).try_into().unwrap();
                        dupe
                    })
                    .collect::<Vec<_>>()
            } else if rrule.bymonthday().contains(&(f.day.try_into().unwrap())) {
                // TODO FIXME what if no BYHOUR is set but freq is e.g. secondly? probably then this is wrong
                vec![f]
            } else {
                vec![]
            }
        } else {
            vec![f]
        }
    });

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYDAY     |Limit   |Limit   |Limit  |Limit  |Expand|Note 1 |Note 2|

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYHOUR    |Limit   |Limit   |Limit  |Expand |Expand|Expand |Expand|
    let it_byhour = it_bymonthday.flat_map(|f| {
        if rrule.rrule.byhour.is_some() {
            if rrule.rrule.freq > Frequency::Hourly {
                // expand
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
                // TODO FIXME what if no BYHOUR is set but freq is e.g. secondly? probably then this is wrong
                vec![f]
            } else {
                vec![]
            }
        } else {
            vec![f]
        }
    });

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYMINUTE  |Limit   |Limit   |Expand |Expand |Expand|Expand |Expand|
    let it_byminute = it_byhour.flat_map(|f| {
        if rrule.rrule.byminute.is_some() {
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
        } else {
            vec![f]
        }
    });

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYSECOND  |Limit   |Expand  |Expand |Expand |Expand|Expand |Expand|

    let bysecond = it_byminute.flat_map(|f| {
        if rrule.rrule.bysecond.is_some() {
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
        } else {
            vec![f]
        }
    });

    let result = std::iter::once(dtstart)
        .chain(bysecond)
        .map(Into::<DateTime<Utc>>::into);

    let the_final: Box<dyn Iterator<Item = DateTime<Utc>> + 'a> =
        if let RecurEnd::Count(c) = rrule.rrule.end {
            Box::new(result.take(c.try_into().unwrap()))
        } else {
            Box::new(result)
        };

    let the_final_final: Box<dyn Iterator<Item = DateTime<Utc>> + 'a> =
        if let RecurEnd::Until(c) = rrule.rrule.end {
            let the_end = date_or_datetime_to_utc(c);
            Box::new(the_final.take_while(move |e| *e <= the_end))
        } else {
            Box::new(the_final)
        };

    the_final_final

    //    |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
    //    |BYSETPOS  |Limit   |Limit   |Limit  |Limit  |Limit |Limit  |Limit |
}

#[cfg(test)]
mod tests {

    #[test]
    fn examples_interpreter() {
        let dtstart = date_or_datetime_to_utc(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
            DateTime::from_utc(NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0), Utc),
        )))
        .into();
        let rrule = RecurRule {
            freq: Frequency::Daily,
            end: RecurEnd::Count(10),
            ..Default::default()
        };
        let rrule = RRule { dtstart, rrule };
        for date in rrule.take(10) {
            println!("{}", Into::<DateTime<Utc>>::into(date));
        }

        println!("------------------------");

        let dtstart = date_or_datetime_to_utc(RRuleDateOrDateTime::DateTime(RRuleDateTime::Utc(
            DateTime::from_utc(NaiveDate::from_ymd(2021, 9, 20).and_hms(0, 0, 0), Utc),
        )))
        .into();
        let rrule = RecurRule {
            freq: Frequency::Daily,
            end: RecurEnd::Count(10),
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
            end: RecurEnd::Count(10),
            bysecond: Some(vec![1, 2]),
            ..Default::default()
        };
        let rrule = RRule { dtstart, rrule };
        for date in complete_implementation(&rrule).take(10) {
            println!("{}", Into::<DateTime<Utc>>::into(date));
        }
    }
}
