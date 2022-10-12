SET market_id = 001;  -- Masked
SET date_filter = last_28_days;
SET date_label = 'last_28_days';

SELECT
    advertiser_id
    ,advertiser_name
    ,insertion_order_id
    ,insertion_order_name
    ,${date_label} as time_period --update as needed
    ,sum(impressions) as impressions
    ,sum(spend) as spend
    ,sum(xd_actions) as xd_actions
    ,sum(xd_rev) as xd_rev
    ,sum(uu) as uu
    ,sum(xd_actions) / sum(uu) as conversion_rate
FROM (
  SELECT
    advertiser_id
    ,advertiser_name
    ,insertion_order_id
    ,insertion_order_name
    ,sum(impression) as impressions
    ,sum(cost) as spend
    ,sum(cross_device_action) as xd_actions
    ,sum(cross_device_assigned_cpa_action) as xd_rev
    ,count(distinct case when household_person_id<>-1 then household_person_id
           when derived_user_id<>-1 then derived_user_id
           else user_id end) as uu

 FROM media  -- Masked
 WHERE dates ${date_filter} --update as needed
 and market_id = ${market_id}
 and event_type in ('impression','action')
 group by 1,2,3,4
  ) A
GROUP BY 1,2,3,4,5
