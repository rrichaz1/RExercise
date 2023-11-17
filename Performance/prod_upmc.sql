select job_no, max(inserts1) from interface_run where 
program_name = 'PopulateMonAccount'
group by job_no
 ;
 
 select * from spark_job_tracker sjt, (select job_no, max(inserts1) from interface_run where 
program_name = 'PopulateMonAccount'
group by job_no) volume;


select * from interface_run where job_no = 518799777 order by start_date ;

--16:53:53

select * from spark_job_tracker where job_no_spark = 519268509;

select spark_job_no,to_char(spark_start_date, 'mm/dd/yyyy hh24:mi:ss') as spark_start_date, 
to_char(spark_end_date, 'mm/dd/yyyy hh24:mi:ss') as spark_end_date, 
to_char(max_merge_end, 'mm/dd/yyyy hh24:mi:ss') as max_merge_end, 
round((spark_end_date-spark_start_date) * 24,2) as total_spark_time, 
round((max_merge_end-spark_start_date) * 24,2) as total_spark_merge_time,
vol
from
(
select jh.start_date as spark_start_date, jh.end_date as spark_end_date, spark_job_no, vol,  max_merge_end
from job_history jh, (select job_no spark_job_no, max(inserts1) vol, max(end_date) max_merge_end
from interface_run where 
program_name = 'PopulateMonAccount'
group by job_no) volume1
where volume1.spark_job_no = jh.job_no
order by job_no) order by vol desc;

select job_day, max(vol), max(job_time) from
(
select  job_day, round((end_date-start_date)*24,2) as job_time,vol,job_no
from
(
select max(end_date) as end_date, min(start_date) as start_date, max(vol) as vol, a.job_no, min(trunc(start_date)) as job_day from interface_run a, 
(select inserts1 as vol, job_no from interface_run where upper(program_name) like 'MON_ACCOUNTS%') b where a.job_no = b.job_no
group by a.job_no) a )
group by job_day;