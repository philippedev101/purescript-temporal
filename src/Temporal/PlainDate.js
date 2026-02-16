export const plainDateImpl = (just) => (nothing) => (y) => (m) => (d) => {
  try {
    return just(new Temporal.PlainDate(y, m, d));
  } catch (_) {
    return nothing;
  }
};

export const plainDateWithImpl = (just) => (nothing) => (overflow) => (y) => (m) => (d) => {
  try {
    return just(Temporal.PlainDate.from({ year: y, month: m, day: d }, { overflow }));
  } catch (_) {
    return nothing;
  }
};

export const fromStringImpl = (just) => (nothing) => (str) => {
  try {
    return just(Temporal.PlainDate.from(str));
  } catch (_) {
    return nothing;
  }
};

export const getYear = (d) => d.year;
export const getMonth = (d) => d.month;
export const getDay = (d) => d.day;
export const getDayOfWeek = (d) => d.dayOfWeek;
export const getDayOfYear = (d) => d.dayOfYear;
export const getWeekOfYear = (d) => d.weekOfYear;
export const getYearOfWeek = (d) => d.yearOfWeek;
export const getDaysInMonth = (d) => d.daysInMonth;
export const getDaysInWeek = (d) => d.daysInWeek;
export const getDaysInYear = (d) => d.daysInYear;
export const getMonthsInYear = (d) => d.monthsInYear;
export const getInLeapYear = (d) => d.inLeapYear;

export const withImpl = (just) => (nothing) => (overflow) => (fields) => (pd) => {
  try {
    return just(pd.with(fields, { overflow }));
  } catch (_) {
    return nothing;
  }
};

export const addImpl = (just) => (nothing) => (dur) => (pd) => {
  try {
    return just(pd.add(dur));
  } catch (_) {
    return nothing;
  }
};

export const subtractImpl = (just) => (nothing) => (dur) => (pd) => {
  try {
    return just(pd.subtract(dur));
  } catch (_) {
    return nothing;
  }
};

export const sinceImpl = (opts) => (self) => (other) => self.since(other, opts);
export const untilImpl = (opts) => (self) => (other) => self.until(other, opts);

export const toPlainDateTime = (time) => (date) => date.toPlainDateTime(time);
export const toPlainYearMonth = (d) => d.toPlainYearMonth();
export const toPlainMonthDay = (d) => d.toPlainMonthDay();
export const toZonedDateTime = (tz) => (time) => (date) => date.toZonedDateTime({ timeZone: tz, plainTime: time });

export const toString = (d) => d.toString();
