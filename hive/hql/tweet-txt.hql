SELECT id, `user`.screen_name, trim(regexp_replace(text, '\r\n+|\r+|\n+|\t+|\s+]', ' '))
FROM ${hiveconf:tbl}
WHERE coldate = ${hiveconf:day}