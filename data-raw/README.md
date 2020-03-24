
From now on, the data will come in this form. 

Please refer to columns by name rather than number when constructing the matricies for your scripts. More columns may be added in the future and the order may change, but the names will not.

I have created a smaller spreadsheet of the total spreadsheet for Hubei, because some columns are missing entirely.

## All the column names are (for the complete data):

* Country.Region: Corresponds to "Country.Region" in the Johns-Hopkins data
* Province.State: Corresponds to "Province.State" in the Johns-Hopkins data
* date: date
* confirmed: Cumulative count of total number of people to ever have the disease
* recovered:
* deaths:
* active: number of people currently with nCov-19
* suspected: Suspected cases, as reported by the National Health * Commission in China
* hospitalized_intensive: In the Italian data this is terapia_intensiva
* hospitalized_symptoms: In the Italian data this is ricoverati_con_sintomi
* isolated_at_home: In the Italian data this is isolamento_domiciliare
* tested: number of people tested
* close_observation: In the NHC data, this was reported as "under close medical observation", originally we thought this was like isolated_at_home, but it is greater than confirmed so we are unsure.
* susc_not_ill: The number of people yet to be infected. This is starting population in 2020 minus confirmed cases.

## Formulas

Confirmed = Active + Recovered + Deaths

Active = Hospitalized_intensive + hospitalized_symptoms + isolated_at_home

Susc_not_ill = Population2020 - Confirmed

Thank you

Anthony
