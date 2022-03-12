Select * From [dbo].['owid-covid-data$']
order by 3,4

Select Location, date, total_cases, new_cases, total_deaths, population
From [dbo].['owid-covid-data$']
order by 1,2


--Looking at Total cases V/s Total Deaths

Select Location, date, total_cases, total_deaths,  (total_deaths/total_cases)*100 as DeathPercentage
From [dbo].['owid-covid-data$']
--where Location like '%africa%'
order by 1,2 

Select Location, date, MAX(total_cases) as HighestInfectionCount,  MAX((total_deaths/population))*100 as DeathHighestPercentage
From [dbo].['owid-covid-data$']
--where Location like '%africa%'
Group by Location, Population
order by 1,2 

--Showing countries with Highest Death Count per population

Select Location, MAX(Total_deaths) as TotalDeath
From [dbo].['owid-covid-data$']
Group by Location
order by TotalDeath desc

Select Location, MAX(cast(total_deaths as int)) as TotaldeathCount
From [dbo].['owid-covid-data$']
where continent is not null
Group by Location 
order by TotaldeathCount desc

--Showing continents with highest death count per population


Select continent, MAX(cast(Total_deaths as int)) as Totaldeath
From [dbo].['owid-covid-data$']
where continent is not null
Group by continent 
order by TotalDeath desc

----
Select * 
From [dbo].['owid-covid-data$'] dea
Join [dbo].['owid-covid-data$'] vac
     On dea.location = vac.location
	 and dea.date = vac.date
where dea.continent is not null
order by 1,2,3

