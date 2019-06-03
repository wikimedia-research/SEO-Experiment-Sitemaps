USE wmf;
SELECT
  CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')) AS date,
  IF(project IN('id.wikipedia', 'ko.wikipedia', 'nl.wikipedia', 'nds-nl.wikipedia', 'pa.wikipedia', 'pnb.wikipedia, 'pt.wikipedia'), 'treatment', 'control') AS test_group,
  project,
  IF(access_method = 'mobile web', 'mobile', 'desktop') AS site_version,
  IF(referer_class = 'external (search engine)', 'search', referer_class) AS referrer,
  SUM(view_count) AS pageviews
FROM projectview_hourly
WHERE year = ${year} AND month = ${month} AND day = ${day}
  AND referer_class IN('none', 'internal', 'external', 'external (search engine)')
  AND agent_type = 'user'
  AND project IN(
    'id.wikipedia', 'pt.wikipedia', 'pa.wikipedia', 'pnb.wikipedia', 'nl.wikipedia',
    'nds-nl.wikipedia', 'ko.wikipedia', 'bh.wikipedia', 'chr.wikipedia',
    'kk.wikipedia', 'ca.wikipedia', 'fr.wikipedia', 'yo.wikipedia', 'xal.wikipedia'
  )
GROUP BY
  CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')),
  IF(project IN('id.wikipedia', 'ko.wikipedia', 'nl.wikipedia', 'pa.wikipedia', 'pt.wikipedia'), 'treatment', 'control'),
  project,
  IF(access_method = 'mobile web', 'mobile', 'desktop'),
  IF(referer_class = 'external (search engine)', 'search', referer_class)
ORDER BY date, test_group, project, site_version, referrer
LIMIT 100000;
