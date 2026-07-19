--pb de coordonnées : nouvelles lignes insérées ne sont pas dans la même projection que les anciennes
select * from carto_model.surf_inf_dev where annee>2016 --1268

begin;
update carto_model.surf_inf_dev set geom=ST_TRANSFORM(ST_SetSRID(geom,2154),27572) where annee>2016
commit;

--Requête surf_inf_dev par année, cours d'eau et ref_couche_carto


select toto.cours_eau,toto.annee,toto.ref_couche_carto,round(sum(toto.surf))
from (
select annee,cours_eau,ref_couche_carto,facies,case when facies like 'PLA' THEN (sum(surface) /5) ELSE sum(surface) END	as surf						
from carto_model.surf_inf_dev
where (facies like 'RAD'  or  facies like 'RAP' or   facies like 'RAB' or  facies like 'PLA') --les clo ne sont plus considérés : or  facies like 'CLO' 
group by annee,cours_eau,ref_couche_carto,facies
) toto
group by toto.cours_eau,toto.annee, toto.ref_couche_carto
order by toto.ref_couche_carto,toto.annee,toto.cours_eau;

select toto.cours_eau,toto.annee,toto.ref_couche_carto,round(sum(toto.surf))
from (
select annee,cours_eau,ref_couche_carto,facies,case when facies like 'PLA' THEN (sum(surface) /5) ELSE sum(surface) END	as surf						
from carto_model.surf_inf_dev
where (facies like 'RAD'  or  facies like 'RAP' or   facies like 'RAB' or  facies like 'PLA') --les clo ne sont plus considérés : or  facies like 'CLO' 
group by annee,cours_eau,ref_couche_carto,facies
) toto
group by toto.cours_eau,toto.annee, toto.ref_couche_carto
order by toto.cours_eau, toto.annee;

--#################################################################################
--Requête pour MAJ modèle S_juv_JP_dev
--On croise avec la couche des secteurs pour calculer les surfaces par secteur
--#################################################################################
--Attention à partir de 2008 plus aucun alevin déversé en S3 et S4 (il doit y avoir un souci peut-être à cause du buffer 
--qui du coup donne des surface en S3 alors qu'il faut les remettre sur S4)

WITH surf_sec AS(
	--select surf.*,sec.*
	select surf.annee,surf.cours_eau,surf.ref_couche_carto,surf.facies,case when surf.facies like 'PLA' THEN (sum(surf.surface) /5) ELSE sum(surf.surface) END	as surf, sec.secteur_mo
	from carto_model.surf_inf_dev surf
	left join carto_model.secteur sec on ST_Intersects(ST_BUFFER(sec.geom,1000),surf.geom) and sec.cours_eau=surf.cours_eau --12343
	where (facies like 'RAD'  or  facies like 'RAP' or   facies like 'RAB' or  facies like 'PLA')
	group by surf.annee,surf.cours_eau,surf.ref_couche_carto,surf.facies,sec.secteur_mo
	),
	sum_surf_sec AS (
		SELECT cours_eau,annee,ref_couche_carto,secteur_mo,round(sum(surf)) as surf
		FROM surf_sec
		GROUP BY ref_couche_carto,cours_eau,annee,secteur_mo
	),
	sum_surf_sec_tot AS (
		select case when cours_eau='Dore' THEN 1 WHEN cours_eau='Alagnon' THEN 2 ELSE secteur_mo END as secteur_def,annee,ref_couche_carto, sum(surf) as surf_def
		from sum_surf_sec
		GROUP BY secteur_def,annee,ref_couche_carto
		ORDER BY ref_couche_carto,annee,secteur_def
	)
	select CASE WHEN (annee>=2008) AND (secteur_def=3) THEN 1 ELSE secteur_def END as secteur,annee,ref_couche_carto, sum(surf_def) as surf_def
	from sum_surf_sec_tot
	where ref_couche_carto~*'.*2021.*' 
	group by 1,2,3
	order by annee,secteur

----------------------------------------------------------------------------------------------------------
--OLD (sans ref_couche_carto) On croise avec la couche des secteurs pour calculer les surfaces par secteur

WITH surf_sec AS(
	--select surf.*,sec.*
	select surf.annee,surf.cours_eau,surf.facies,case when surf.facies like 'PLA' THEN (sum(surf.surface) /5) ELSE sum(surf.surface) END	as surf, sec.secteur_mo
	from carto_model.surf_inf_dev surf
	left join carto_model.secteur sec on ST_Intersects(ST_BUFFER(sec.geom,1000),surf.geom) --12343
	where (facies like 'RAD'  or  facies like 'RAP' or   facies like 'RAB' or  facies like 'CLO' or  facies like 'PLA')
	group by surf.annee,surf.cours_eau,surf.facies,sec.secteur_mo
	)
SELECT cours_eau,annee,secteur_mo,round(sum(surf))
FROM surf_sec
GROUP BY cours_eau,annee,secteur_mo
ORDER BY annee, cours_eau,secteur_mo

-- Requête pour avoir les déversements par année

select * 
from deversement
where (dev_date>'2004-12-31' and dev_date<'2006-01-01')  and substr(dev_stade,1,2)='al' 
order by dev_date


select pec_sta_id,pec_id,pec_date
from peche p
join carto_model.surf_inf_dev s on (ST_Intersects(st_buffer(s.geom,100),p.geom) and s.annee=extract(year from p.pec_date))
where extract(year from pec_date)=2017 and extract (month from pec_date)>7 and p.pec_methodepeche='protocole Prévost et Baglinière'
group by 1,2,3