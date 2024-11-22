use SBIRNCSCNEW

 -----       -------------------------========= truncate tables to recreate
--drop PROCEDURE MB_GenerateDates
--drop Table DayVec
--drop Table Last_end_day_cut
--drop Table DayVec_Join
--drop Table #temp_obs

--
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE MB_GenerateDates
	@StartDate DateTime,
	@EndDate DateTime
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	Create TABLE #tmp
	(
		[Year] int,
		[Month] int,
		[Day] int,
		[Hour] int
	)

	DECLARE @I DateTime
	DECLARE @h int = 0

	Set @I = @StartDate
	WHILE @I <= @EndDate
	BEGIN
		SET @h = 0
		WHILE @h < 24
		BEGIN
			INSERT INTO #tmp SELECT Year(@I), Month(@I), Day(@I), @h
			SET @h = @h + 1
		END
		SET @I = DateAdd(day, 1, @I)
	END

	select * from #tmp order by [year], [month], [day], [hour]

	drop table #tmp
END
GO

Create TABLE DayVec
	(
		[Year] int,
		[Month] int,
		[Day] int,
		[Hour] int
	)
	insert into DayVec ( [Year],
		[Month],
		[Day],
		[Hour])
	exec MB_GenerateDates '2010/07/24', '2012/06/19' 

	--================ aggregate every hour by excluding the first and last days of the data

			select year(StartDate) as year,
               month(StartDate) as month,
               day(StartDate) as day,
               datepart(hour, StartDate) as hour, 
               count(*) as obbs INTO #temp_obs
               FROM [SBIRNCSCNEW].[dbo].[Facts] where [StartDate] >='2010-07-24' and StartDate <='2012-06-19'
               group by 
               year(StartDate) ,
               month(StartDate),
               day(StartDate),
               DATEPART(hour, StartDate)
               order by year, month, day, hour;

			  select * from #temp_obs order by year, month, day, hour;


-- =================== put the data into a table

	Create TABLE Last_end_day_cut
	(
		[Year] int,
		[Month] int,
		[Day] int,
		[Hour] int,
		[obs] int
	)
	insert into Last_end_day_cut  select * from #temp_obs order by year, month, day, hour;

	select * from Last_end_day_cut order by year, month, day, hour;
	select * from DayVec

	-- ========================= View and alter table DayVec
-- Alter table 
	ALTER TABLE dbo.DayVec
   ADD ID INT IDENTITY(1, 1) NOT NULL,
    obs int

	select * from DayVec

	-- ========================= create a table with increasing id 

create table DayVec_Join
(id int IDENTITY (1, 1) NOT NULL
	  ,[Year] int
      ,[Month] int
      ,[Day] int 
      ,[Hour] int 
      ,[obs] int)

-- ======================== insert into the table the other columns, other than id

	insert into DayVec_Join
	(
	   [Year]
      ,[Month]
      ,[Day]
      ,[Hour]
      ,[Obs]
	)

SELECT        TOP (100) PERCENT dbo.Last_end_day_cut.Year, dbo.Last_end_day_cut.Month, dbo.Last_end_day_cut.Day, dbo.Last_end_day_cut.Hour, dbo.Last_end_day_cut.obs
FROM            dbo.DayVec left outer JOIN
                dbo.Last_end_day_cut ON dbo.DayVec.Year = dbo.Last_end_day_cut.Year AND dbo.DayVec.Month = dbo.Last_end_day_cut.Month AND dbo.DayVec.Day = dbo.Last_end_day_cut.Day AND 
                dbo.DayVec.Hour = dbo.Last_end_day_cut.Hour ORDER BY dbo.DayVec.id

-- select * from DayVec_Join

---- =========== The joined table with null values is created. We need to fill the gap of the table by updating DayVec_Join

UPDATE DayVec_Join
SET dbo.DayVec_Join.Year = dbo.DayVec.Year,
	dbo.DayVec_Join.Month = dbo.DayVec.Month,
	dbo.DayVec_Join.Day = dbo.DayVec.Day,
	dbo.DayVec_Join.Hour = dbo.DayVec.Hour
FROM dbo.DayVec where dbo.DayVec_Join.id = dbo.DayVec.id

-- select * from DayVec_Join

--------------========= We need to fill the gap of the obs null value of the DayVec_Join column by 0

UPDATE DayVec_Join
SET dbo.DayVec_Join.Obs = 0
FROM dbo.DayVec where dbo.DayVec_Join.id = dbo.DayVec.id and dbo.DayVec_Join.Obs is null

-- select * from DayVec_Join

--------------=================  Aggregate every day

		select year,
                month,
               day,
               sum(Obs) as obbs INTO Per_day
               FROM DayVec_Join
               group by 
               year,
               month,
               day
        order by year, month, day;

--select * from Per_day order by year, month, day;

------------- ================== per hour with probability

select * into Per_hour from DayVec_Join order by year, month, day, hour;

--- select * from Per_hour
Alter table Per_hour
add [Prob] float

--- select * from Per_hour order by id
---- select * from Per_day
---- -------------------------============ set prob = per_hour/per day when obs is not 0

update Per_hour
set Prob=CAST(Per_hour.obs as float)/cast(Per_day.obbs as float) from Per_hour, Per_day 
where Per_hour.[Year]=Per_day.[Year] and Per_hour.[Month]=Per_day.[Month] and Per_hour.[Day]=Per_day.[Day] and not(Per_day.obbs = 0)

----- ------============== Set Prob = 0 when count =0

update Per_hour
set Prob=0 where Per_hour.obs=0;

--- select * from Per_hour order by id

----- ==================== We now have to calculate the outliers based on 3 times standard deviation from the mean

select * into outliers_3std from Per_hour where Per_hour.Prob > (select ((select avg(Per_hour.Prob) from Per_hour)+ (select 3*(stdev(Per_hour.prob)) from Per_hour)))

-- select * from outliers_3std

------=========== All data with outliers labeled using 3 times standard deviation from the mean

select * into Per_Hour_with_labeles from Per_hour

Alter table Per_Hour_with_labeles
add label_3std int

---- select * from Per_Hour_with_labeles order by id
 ---============= update Per_Hour_with_labeles set label_3std= NULL

update Per_Hour_with_labeles
set label_3std=1 
where Per_Hour_with_labeles.Prob > (select ((select avg(Per_Hour_with_labeles.Prob) from Per_Hour_with_labeles)+ (select 3*(stdev(Per_Hour_with_labeles.prob)) from Per_Hour_with_labeles)))

update Per_Hour_with_labeles
set label_3std=0
where label_3std is null;

---- select * from Per_Hour_with_labeles order by id    -----  alter table Per_Hour_with_labeles drop column id_prob

------ ======================================== calculate the median of the prob

create table Perhor_asc_prob
(
id_prob_asc int identity(1,1),
year int, month int, day int, hour int, obs int, prob float, label_3std int
)

insert into Perhor_asc_prob (year,month, day, hour, obs, Prob, label_3std) 
select year,month, day, hour, obs, Prob, label_3std  from Per_Hour_with_labeles order by Per_Hour_with_labeles.Prob 

---  select * from Perhor_asc_prob

--- ========== R_code result 
----======== > median(x, na.rm = FALSE)
-----========= [1] 0.02629644  --------- ===========================

declare @median1 float
declare @median2 float
declare @median float

create table mid_points
(
mid1 float,
mid2 float,
med float
)

if (select count(id_prob_asc) % 2 from Perhor_asc_prob)=0
begin
set @median1=(select Perhor_asc_prob.prob as ab from Perhor_asc_prob where id_prob_asc =(select count(id_prob_asc)/2 from Perhor_asc_prob));
set @median2=(select Perhor_asc_prob.prob as cd from Perhor_asc_prob where id_prob_asc =(select count(id_prob_asc)/2 +1 from Perhor_asc_prob));
set @median= (@median1 + @median2)/2;
insert into mid_points select @median1, @median2, @median
end
else
begin
set @median=(select Perhor_asc_prob.prob as cd from Perhor_asc_prob where id_prob_asc =(select count(id_prob_asc)/2 +1 from Perhor_asc_prob));
insert into mid_points select @median, @median, @median
end

-----  select * from mid_points
------ ======================= Calculate the median absolute deviation
create table MADs
(
id int identity(1,1),
MADs float
)
insert into MADs
select abs(xi.prob-b.med) from Perhor_asc_prob xi, mid_points b order by abs(xi.prob-b.med)
-------------------  MAD - Median Absolute deviation
declare @MAD1 float
declare @MAD2 float
declare @MAD float

create table MAD_v
(
MAD1 float,
MAD2 float,
MAD float
)

if (select count(id_prob_asc) % 2 from Perhor_asc_prob)=0
begin
set @MAD1=(select MADs from MADs where id=(select count(id)/2 from MADs));
set @MAD2=(select MADs from MADs where id=(select count(id)/2 +1 from MADs));
set @MAD= (@MAD1 + @MAD2)/2;
insert into MAD_v select @MAD1, @MAD2, @MAD
end
else
begin
set @MAD= (select MADs from MADs where id=(select count(id)/2 +1 from MADs));

insert into MAD_v select @MAD, @MAD, @MAD
end

select * from MAD_v
----------------- ========================== apply Modified Z-score to label the data
---------------====  R code:  M_i=0.6745*(Attack_per_hr_relatv[,6]-x_med)/(MAD_relv); Outl_MAD<-Attack_per_hr_relatv[which(abs(M_i)>3.5),]; 

Alter table Per_Hour_with_labeles
add M_i_v float

---     select* from Per_Hour_with_labeles

update Per_Hour_with_labeles
set M_i_v=Cast(0.6745*(xi.prob-x.med) as float)/cast(y.MAD as float) from Per_Hour_with_labeles xi, mid_points x, MAD_v y

Alter table Per_Hour_with_labeles
add M_i_Label float

--- ================= make the outliers 1

update Per_Hour_with_labeles
set M_i_Label=1 where abs(Per_Hour_with_labeles.M_i_v)>3.5

-------- ================ make the rest 0

update Per_Hour_with_labeles
set M_i_Label=0 where not(abs(Per_Hour_with_labeles.M_i_v))>3.5

----- ================    This is the last table with labeled data as normal and anomaly =============

select* from Per_Hour_with_labeles order by year, month, day, hour

------------- ================== end of 3std method and Modified Z-Score
