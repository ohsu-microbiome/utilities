observational_variables = 
  
list(
  
  CaseString = list(
    covariate_of_interest='CaseString',
    control='Control',
    case='AMD',
    labels=list(reference='Control', comparison='AMD')
  ),
  
  ARMS2rs10490924 = list(
    covariate_of_interest='ARMS2rs10490924',
    control='GG',
    case='TT',
    labels=list(reference='Non-Risk', comparison='Risk')
  ),
  
  CFHrs1061170 = list(covariate_of_interest='CFHrs1061170',
    control='TT',
    case='CC',
    labels=list(reference='Non-Risk', comparison='Risk')
  ),
  
  CFHrs10737680 = list(
    covariate_of_interest='CFHrs10737680',
    control='CC',
    case='AA',
    labels=list(reference='Non-Risk', comparison='Risk')
  ),
  
  SKIV2Lrs429608 = list(
    covariate_of_interest='SKIV2Lrs429608',
    control='AA',
    case='GG',
    labels=list(reference='Non-Risk', comparison='Risk')
  ),
  
  GA_No_CNV_Either_Eye = list(
    covariate_of_interest='GA_No_CNV_Either_Eye',
    control=0,
    case=1,
    labels=list(reference='No GA', comparison='GA')
  ),
  
  CNV_Either_Eye = list(
    covariate_of_interest='CNV_Either_Eye',
    control=0,
    case=1,
    labels=list(reference='No CNV', comparison='CNV')
  ),
  
  Age = list(
    covariate_of_interest='Age',
    control='',
    case=''
  ),
  
  Gender = list(
    covariate_of_interest='Gender',
    control='F',
    case='M',
    labels=list(reference='F', comparison='M')
  ),
  
  AREDS = list(
    covariate_of_interest='AREDS',
    control='N',
    case='Y',
    labels=list(reference='N', comparison='Y')
  )
  
)