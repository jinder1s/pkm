;;; pkm/experiment.el -*- lexical-binding: t; -*-

(defun pkm-test (habit-schedule-hour)
  (let* ((now (ts-now))
         (current-hour (ts-hour now))
         (current-minute (ts-minute now))
         (schedule-hour (truncate habit-schedule-hour))
         (schedule-minute (truncate (* (mod habit-schedule-hour 1) 60 ) )))
    (when (and (<= current-hour schedule-hour) (< current-minute schedule-minute))
      (--> (ts-apply :hour schedule-hour :minute schedule-minute now)
           (ts-unix it)
           (truncate it)))) )

(defun pkm-test-deadline (habit-deadline-hour)
  (--> (ts-apply :hour (truncate habit-deadline-hour) :minute (truncate (* (mod habit-deadline-hour 1) 60) ) (ts-now))
       (ts-unix it)
       (truncate it)))
