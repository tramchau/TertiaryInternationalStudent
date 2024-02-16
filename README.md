# Tertiary International Student Dashboard

The overall trend of number of International student (from 2003 to 2022) as well as the distributions of international students by different fields of study and by the region in New Zealand
Visualiazation by Shiny for the tertiary internation student in NZ - trend, simple forcast, segment by faculties/providers, regions, and regions comparison.

Dashboard's link access: https://tramchau.shinyapps.io/TertiaryInternationStudentNZ/

## Purpose

The project turn the tabular data published by the Ministry of Education into the visualizations with the interaction from the users to filter in a specific interest of area. As such, the user can grasp the whole picture and the trend over years in a few minutes instead of reading number-based data from excel file. The application allows users to easily compare the trends between fields or between regions. Moreover, the application provides the forecasting number in future based on the history data using auto.arima.

## Data

Data source is excel file which is downloaded from website https://catalogue.data.govt.nz/dataset/international-students-studying-in-new-zealand

- Data is updated annual by Ministry of Education. The years available are from 2013 to 2022, the yearly data records different metrics (number of students and tuition fee income) by different dimensions (region, field of study, provider types, citizenship).
- The excel file includes multiple sheets. Two sheets are selected for analysing for this application. They are the enrolment by regions, and the enrolment by field of study. There is one main metric – number of students enrolled.
  

## Understanding the data

 **Data granularity**: there are 2 levels of data in the application:
- Total level
- Segment level (either field of study or region)
  
**Rounding number**: Data have been rounded by the source to the nearest 5 to protect the privacy of individuals, so the sum of individual counts may not add to the total.

**Timestamp**: Data is yearly and ranges from 2003 to 2022 for total level. This wide year range supports the forecast feature. Segment data is available from 2013 to 2022.

**Field of study**: the predominant field(s) of study of students enrolled at tertiary education providers 

**Region**: region is based on the delivery site of the courses that they were enrolled in for that qualification Students studying from offshore locations are included in the Extramural.

**Education sectors & Provider types**: The application provides data for the Tertiary sector. Note: In New Zealand, there are 2 main education sectors including School sector and Tertiary sector. The School sector include Primary and Secondary. The Tertiary includes university, Private Training establishment, Te Pūkenga, and Wānanga.
