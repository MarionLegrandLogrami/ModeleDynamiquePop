--Requête surf_inf_dev par année et cours d'eau

select sum(toto.surf)
from (
select facies,case when facies like 'PLA' THEN (sum(surface) /5) ELSE sum(surface) END	as surf						
from Dev13_Allier_Lb2e
where (facies like 'RAD'  or  facies like 'RAP' or   facies like 'RAB' or  facies like 'CLO' or  facies like 'PLA')
group by facies
) toto;

select toto.cours_eau,toto.annee,sum(toto.surf)
from (
select annee,cours_eau,facies,case when facies like 'PLA' THEN (sum(surface) /5) ELSE sum(surface) END	as surf						
from surf_inf_dev
where (facies like 'RAD'  or  facies like 'RAP' or   facies like 'RAB' or  facies like 'CLO' or  facies like 'PLA')
group by annee,cours_eau,facies
) toto
group by toto.cours_eau,toto.annee
order by toto.annee,toto.cours_eau;

-- Requête pour avoir les déversements par année

select * 
from deversement
where (dev_date>'2004-12-31' and dev_date<'2006-01-01')  and substr(dev_stade,1,2)='al' 
order by dev_date