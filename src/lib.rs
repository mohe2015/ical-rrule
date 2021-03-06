#![feature(const_impl_trait)]
#![feature(const_fn_trait_bound)]
pub mod parser;
pub mod weekday;

// https://github.com/Geal/nom/blob/master/doc/choosing_a_combinator.md
// https://stackoverflow.com/questions/56846090/nom-5-creating-a-combinator-using-another-parser-multiple-times

// https://datatracker.ietf.org/doc/html/rfc5545#section-3.2
// https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10
// https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5
// https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.3

// https://datatracker.ietf.org/doc/html/rfc5234

// https://stackoverflow.com/questions/46755838/when-will-a-single-icalendar-event-have-multiple-recurrence-rules
