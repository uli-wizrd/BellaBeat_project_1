/*
The following SQL script presents the steps taken in the creation of some of the tables
 used to obtain insights for the BellaBeat project.
*/

-- All of the tables created have had extra spaces in their values removed
-- We'll start by looking at our daily activity table.
-- First we'll see if our table has all the records we need for each column of interest.

SELECT 
   COUNT(Id) AS users,
   COUNT(ActivityDate) AS activity_date,
   COUNT(TotalDistance) AS total_distance,
   COUNT(VeryActiveMinutes) AS vam,
   COUNT(FairlyActiveMinutes) AS fam,
   COUNT(LightlyActiveMinutes) AS lam,
   COUNT(SedentaryMinutes) AS sm,
   COUNT(Calories) AS calories
FROM 
	bellabeat.dailyactivity_merged

-- Now that we know that the amount of data matches our expected value.
-- We'll see what the max and minimum values are for each column of interest.
-- Lets start with max values.

SELECT 
    Id,
    MAX(TotalSteps) AS total_steps,
	ROUND(MAX(TotalDistance), 2) AS total_distance,
    MAX(VeryActiveMinutes) AS vam,
    MAX(FairlyActiveMinutes) AS fam,
    MAX(LightlyActiveMinutes) AS lam,
    MAX(SedentaryMinutes) AS sm,
    MAX(Calories) AS calories
FROM 
	bellabeat.dailyactivity_merged
GROUP BY
	Id
ORDER BY 
	calories

-- Now we look at the minimum values.

SELECT 
    Id,
    MIN(TotalSteps) AS total_steps,
	ROUND(MIN(TotalDistance), 2)  AS total_distance,
    MIN(VeryActiveMinutes)AS vam,
    MIN(FairlyActiveMinutes) AS fam,
    MIN(LightlyActiveMinutes) AS lam,
    MIN(SedentaryMinutes) AS sm,
    MIN(Calories) AS calories
FROM 
	bellabeat.dailyactivity_merged
GROUP BY
	Id
ORDER BY
	calories
    
-- Now lets make a query to prepare the data for analysis.
-- We'll create a table with average values of interest from our daily activity table.
-- Since in this table we can have repeated values we don't need to check that
-- We take the average for our values in order to summarize  each users performance

CREATE TABLE daily_average_data AS (
	SELECT 
		TRIM(Id) AS user_id,
		TRIM(COUNT(DISTINCT(ActivityDate))) AS day_count,
		TRIM(ROUND(AVG(TotalSteps), 0)) AS avg_total_steps,
		TRIM(ROUND(AVG(TotalDistance), 0)) AS total_distance,
		TRIM(ROUND(AVG(VeryActiveMinutes), 0)) AS avg_vam,
		TRIM(ROUND(AVG(FairlyActiveMinutes), 0)) AS avg_fam,
		TRIM(ROUND(AVG(LightlyActiveMinutes), 0)) AS avg_lam,
		TRIM(ROUND(AVG(SedentaryMinutes), 0)) AS avg_sm,
		TRIM(ROUND(AVG(Calories), 0)) AS avg_calories
	FROM 
		bellabeat.dailyactivity_merged
	GROUP BY
		Id
	ORDER BY
		avg_calories
)


-- We'll also take a look at data that was collected in the same amount of time (31 days).

CREATE TABLE daily_average_activity AS (
	SELECT
		*
	FROM
		bellabeat.daily_average_data
	WHERE
		day_count >= 31
        )
        
-- We move to the next table we'll look at.
-- This time we'll look at the daily calories of our users.

SELECT 
	*
FROM 
	bellabeat.dailycalories_merged
LIMIT
	10
    
-- First We'll check that each user doesn't have any repeated dates registered
-- We'll pay special attention to users that registered data during the same amount of days

SELECT
	Id AS user_id,
    COUNT(DISTINCT (ActivityDay)) AS unique_dates,
    COUNT(ActivityDay) AS dates
FROM bellabeat.dailycalories_merged
GROUP BY
	user_id;

-- Since we don't have repeated values and the most common amount of days registered was 31
-- We'll create a table with this information.

CREATE TABLE daily_calorie_summary AS (
SELECT 
	TRIM(Id) AS user_id,
    TRIM(SUM(Calories)) AS Total_calories_registered,
    TRIM(COUNT(ActivityDay)) AS days_registered
FROM 
	bellabeat.dailycalories_merged
GROUP BY
	user_id
)

-- Now we'll look at our steps data.
-- We'll follow the same process previously described

SELECT 
	*
FROM 
	bellabeat.dailysteps_merged
LIMIT
	10
    
-- We look for repeated values and how many values each user has for the step variable

SELECT
	Id AS user_id,
    COUNT(DISTINCT (ActivityDay)) AS unique_dates,
    COUNT(ActivityDay) AS dates
FROM bellabeat.dailysteps_merged
GROUP BY
	user_id;
    
-- We'll take the same approach we took with our calorie data.
-- We know that we can filter in R and Python so we won't do it right now.

CREATE TABLE daily_steps_summary AS (
SELECT 
	TRIM(Id) AS user_id,
    TRIM(SUM(StepTotal)) AS total_steps,
    TRIM(COUNT(ActivityDay)) AS days_registered
FROM 
	bellabeat.dailysteps_merged
GROUP BY 
	user_id
ORDER BY
	total_steps
)

# Now we'll look at our hourly calorie data.

SELECT 
	*
FROM 
	bellabeat.hourlycalories_merged
LIMIT
	10

# Now we'll look at how many records each user has and if it has repeated values.

SELECT 
	Id AS user_id,
    COUNT(DISTINCT(ActivityHour)) AS unique_hours,
    COUNT(ActivityHour) AS hours_registered,
    SUM(Calories) AS total_calories
FROM 
	bellabeat.hourlycalories_merged
GROUP BY
	user_id

# We'll create a table with the previous total values.

CREATE TABLE hourly_calories_totals AS (
	SELECT 
		TRIM(Id) AS user_id,
		TRIM(COUNT(ActivityHour)) AS hours_registered,
		TRIM(SUM(Calories)) AS calories
	FROM 
		bellabeat.hourlycalories_merged
	GROUP BY
		user_id
	ORDER BY
		calories
)

-- We'll create the same table for our intensities to look a total values.
-- But first we'll look for duplicate entries and how much data for each variable we have, like before.

SELECT 
	Id AS user_id,
    COUNT(DISTINCT(ActivityHour)) AS unique_hours,
    COUNT(ActivityHour) AS hours_registered,
    SUM(TotalIntensity) AS total_intensity
FROM 
	bellabeat.hourlyintensities_merged
GROUP BY
	user_id

-- Now we can create our table with confidence.

CREATE TABLE hourly_intensity_totals AS (
	SELECT 
		TRIM(Id) AS user_id,
		TRIM(COUNT(ActivityHour)) AS hours_registered,
		TRIM(SUM(TotalIntensity)) AS total_intensity
	FROM 
		bellabeat.hourlyintensities_merged
	GROUP BY
		user_id
	ORDER BY
		total_intensity
)

-- Up next we'll look at the values of the calorie and intensity variables at different times of the day.
-- We'll start with calories.
-- Then we'll do intensities.

SELECT
	TRIM(ActivityHour) AS registered_activity_hour,
    TRIM(SUM(Calories)) AS total_calories,
    TRIM(AVG(Calories)) AS average_calories
FROM
	bellabeat.hourlycalories_merged
GROUP BY
	registered_activity_hour
LIMIT
	10;

-- Looking at the results from the previous query we must change our approach.
-- We need to do this because our Activity Hour variable is in the date-time format.
-- First we need to prepare the previously mentioned variable to summarize the values for each time.
-- Theb we can create the table we need.

CREATE TABLE hourly_calories2_summary AS (
	SELECT
		TRIM(SUM(Calories)) AS total_calories,
		TRIM(AVG(Calories)) AS average_calories,
		TRIM(SUBSTRING(ActivityHour,10,LENGTH(ActivityHour))) AS hour_registered
	FROM
		bellabeat.hourlycalories_merged
	GROUP BY
		hour_registered
)

-- We repeat this process for our intensities data.

CREATE TABLE hourly_intensities2_summary AS (
	SELECT
		TRIM(SUM(TotalIntensity)) AS total_intensity,
		TRIM(AVG(TotalIntensity)) AS average_intensity,
		TRIM(SUBSTRING(ActivityHour,10,LENGTH(ActivityHour))) AS hour_registered
	FROM
		bellabeat.hourlyintensities_merged
	GROUP BY
		hour_registered
)

-- We'll look at two other tables: weight log info and minute intensities.
-- For this first one we'll just make sure we have no extra spaces.
-- The second table we'll clean and analyze with R and Python.

CREATE TABLE clean_weight_log_info AS (
	
    SELECT
		TRIM(Id) AS user_id,
        TRIM(Date) AS date_registered,
        TRIM(WeightKg) AS weight_kg,
        TRIM(WeightPounds) AS weight_pounds,
        TRIM(Fat) AS fat,
        TRIM(BMI) AS body_mass_index,
        TRIM(IsManualReport) AS is_manual_report,
        TRIM(LogId) AS log_id
	FROM 
		bellabeat.weightloginfo_merged
	ORDER BY
		user_id
)