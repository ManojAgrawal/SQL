/*Data preparation for QlikView prototype

Due to the RAM limitation, we will use the data for 100K patients for the prototype. For this purpose we will create new tables in SQL server and populate with the data related to only these patients.
Table Creations in SQL: */


CREATE TABLE member_qlik(
	[patid] [varchar](12) NOT NULL,
	[eligeff] [date] NOT NULL,
	[eligend] [date] NOT NULL,
	[gdr_cd] [varchar](1) NULL,
	[state] [varchar](2) NULL,
	[yrdob] [int] NULL,
	[bus] [varchar](3) NULL,
	[product] [varchar](3) NULL,
      [risk_score]DECIMAL(6,2) NULL
	primary key(patid, eligeff, eligend));

CREATE TABLE confinement_qlik(
	[ADMIT_DATE] [date] NULL,
	[DISCH_DATE] [date] NULL,
	[DIAG_BEGIN] [varchar](6) NULL,
	[DIAG_END] [varchar](6) NULL,
	[DIAG1] [varchar](6) NULL,
	[DIAG2] [varchar](6) NULL,
	[DIAG3] [varchar](6) NULL,
	[DIAG4] [varchar](6) NULL,
	[DIAG5] [varchar](6) NULL,
	[PROC_BEGIN] [varchar](6) NULL,
	[PROC_END] [varchar](6) NULL,
	[PROC1] [varchar](6) NULL,
	[PROC2] [varchar](6) NULL,
	[PROC3] [varchar](6) NULL,
	[PROC4] [varchar](6) NULL,
	[PROC5] [varchar](6) NULL,
	[DRG] [varchar](6) NULL,
	[CHARGE] [money] NULL,
	[PATID] [varchar](12) NOT NULL,
	[CONF_ID] [varchar](50) NOT NULL,
	[eligeff] [date] NOT NULL,
	[eligend] [date] NOT NULL
	PRIMARY KEY (PATID,CONF_ID), 
FOREIGN KEY (PATID,eligeff,eligend) REFERENCES member_qlik(patid,eligeff,eligend));

CREATE TABLE [dbo].[lab_qlik](
	[patid] [varchar](12) NOT NULL,
	[fst_dt] [date] NOT NULL,
	[source] [varchar](50) NULL,
	[anlytseq] [varchar](50) NULL,
	[hi_nrml] [varchar](50) NULL,
	[loinc_cd] [varchar](50) NULL,
	[low_nrml] [varchar](50) NULL,
	[abnl_cd] [varchar](5) NULL,
	[rslt_txt] [varchar](50) NULL,
	[rslt_nbr] [varchar](50) NULL,
	[tst_desc] [varchar](50) NULL,
	[tst_nbr] [varchar](50) NULL,
	[labclmid] [varchar](50) NULL,
	[rslt_unit_nm] [varchar](50) NULL,
	[eligeff] [date] NOT NULL,
	[eligend] [date] NOT NULL,
FOREIGN KEY (patid,eligeff,eligend) REFERENCES member_qlik(patid,eligeff,eligend));

CREATE INDEX lab_patid ON lab_qlik(patid);


CREATE TABLE [dbo].[medical_qlik](
	[clmseq] [varchar](6) NOT NULL,
	[fst_dt] [date] NULL,
	[lst_dt] [date] NOT NULL,
	[paid_dt] [date] NULL,
	[diag1] [varchar](6) NULL,
	[diag2] [varchar](6) NULL,
	[diag3] [varchar](6) NULL,
	[diag4] [varchar](6) NULL,
	[diag5] [varchar](6) NULL,
	[proc1] [varchar](6) NULL,
	[proc2] [varchar](6) NULL,
	[proc3] [varchar](6) NULL,
	[drg] [varchar](6) NULL,
	[proc_cd] [varchar](6) NULL,
	[charge] [money] NULL,
	[patid] [varchar](12) NOT NULL,
	[conf_id] [varchar](20),
	[clmid] [varchar](20) NOT NULL,
	[eligeff] [date] NOT NULL,
	[eligend] [date] NOT NULL
	PRIMARY KEY (patid,clmid,clmseq,lst_dt), 
FOREIGN KEY (PATID,eligeff,eligend) REFERENCES member_qlik(patid,eligeff,eligend));


CREATE TABLE [dbo].[rx_qlik](
	[patid] [varchar](12) NOT NULL,
	[chk_dt] [date] NULL,
	[fill_dt] [date] NOT NULL,
	[days_sup] [int] NULL,
	[ndc] [varchar](50) NOT NULL,
	[quantity] [varchar](50) NULL,
	[rfl_nbr] [varchar](12) NULL,
	[charge] [money] NULL,
	[brnd_nm] [varchar](50) NULL,
	[strength] [varchar](50) NULL,
	[gnrc_ind] [varchar](2) NULL,
	[clmid] [varchar](15) NOT NULL,
	[fst_fill] [varchar](50) NULL,
	[eligeff] [date] NOT NULL,
	[eligend] [date] NOT NULL
	PRIMARY KEY (patid,clmid,fill_dt,ndc), 
FOREIGN KEY (PATID,eligeff,eligend) REFERENCES member_qlik(patid,eligeff,eligend));


CREATE TABLE patient_conditions_qlik (
	[patid] [varchar](12) NOT NULL,
	lst_dt DATE NOT NULL,
	[Condition] [varchar](6) NOT NULL,
	[cond_class] [varchar](10) NULL,
	[eligeff] [date] NOT NULL,
	[eligend] [date] NOT NULL
	PRIMARY KEY (patid,lst_dt,Condition), 
    FOREIGN KEY (PATID,eligeff,eligend) REFERENCES member_qlik(patid,eligeff,eligend));

CREATE TABLE patient_procedures_qlik (
	[patid] [varchar](12) NOT NULL,
	lst_dt DATE NOT NULL,
	[Procedure] [varchar](6) NOT NULL,
   [eligeff] [date] NOT NULL,
	[eligend] [date] NOT NULL
	PRIMARY KEY (patid,lst_dt,[Procedure]), 
    FOREIGN KEY (patid,eligeff,eligend) REFERENCES member_qlik(patid,eligeff,eligend));
    

CREATE TABLE [dbo].[patient_cond_ct](
	[patid] [varchar](12) NOT NULL,
	[ct_acmin] [smallint] NULL,
	[ct_acmaj] [smallint] NULL,
	[ct_recur] [smallint] NULL,
	[ct_aller] [smallint] NULL,
	[ct_crst] [smallint] NULL,
	[ct_crunst] [smallint] NULL,
	[ct_crspst] [smallint] NULL,
	[ct_crspun] [smallint] NULL,
	[ct_psymin] [smallint] NULL,
	[ct_psyrecst] [smallint] NULL,
	[ct_psyrecun] [smallint] NULL,
	[ct_pregnmin] [smallint] NULL,
	[ct_pregnmaj] [smallint] NULL,
	[ct_malig] [smallint] NULL,
	[ct_eyedent] [smallint] NULL ,
    [eligeff] [date] NOT NULL,
	[eligend] [date] NOT NULL
	PRIMARY KEY (patid,eligeff, eligend), 
    FOREIGN KEY (patid,eligeff,eligend) REFERENCES member_qlik(patid,eligeff,eligend));

/* Insert data commands */

INSERT INTO member_qlik (patid,eligeff, eligend, gdr_cd,state, yrdob, bus, product)

SELECT TOP 95000 * from member
UNION 
SELECT * from member where yrdob >= 1996 ORDER BY patid DESC;
Note: youth data is limited in the original optum files hence they are inserted explicitly using ‘union’ command
INSERT INTO confinement_qlik

SELECT confinement.[ADMIT_DATE]
      ,confinement.[DISCH_DATE]
      ,confinement.[DIAG_BEGIN]
      ,confinement.[DIAG_END]
      ,confinement.[DIAG1]
      ,confinement.[DIAG2]
      ,confinement.[DIAG3]
      ,confinement.[DIAG4]
      ,confinement.[DIAG5]
      ,confinement.[PROC_BEGIN]
      ,confinement.[PROC_END]
      ,confinement.[PROC1]
      ,confinement.[PROC2]
      ,confinement.[PROC3]
      ,confinement.[PROC4]
      ,confinement.[PROC5]
      ,confinement.[DRG]
      ,confinement.[CHARGE]
      ,member_qlik.patid
      ,confinement.[CONF_ID]
      ,member_qlik.eligeff
      ,member_qlik.eligend
  FROM member_qlik INNER JOIN confinement on member_qlik.patid = confinement.PATID
  WHERE confinement.ADMIT_DATE >= '2012-01-01'AND confinement.DISCH_DATE <= '2012-12-31' 
AND confinement.ADMIT_DATE BETWEEN  member_qlik.eligeff and member_qlik.eligend;

INSERT INTO medical_qlik

SELECT medical.[clmseq]
      ,medical.[fst_dt]
      ,medical.[lst_dt]
      ,medical.[paid_dt]
      ,medical.[diag1]
      ,medical.[diag2]
      ,medical.[diag3]
      ,medical.[diag4]
      ,medical.[diag5]
      ,medical.[proc1]
      ,medical.[proc2]
      ,medical.[proc3]
      ,medical.[drg]
      ,medical.[proc_cd]
      ,medical.[charge]
      ,member_qlik.[patid]
      ,medical.[conf_id]
      ,medical.[clmid]
      ,member_qlik.[eligeff]
      ,member_qlik.[eligend]
  FROM member_qlik INNER JOIN medical on member_qlik.patid = medical.patid
  WHERE medical.lst_dt BETWEEN  '2012-01-01'AND '2012-12-31' 
AND medical.lst_dt BETWEEN  member_qlik.eligeff and member_qlik.eligend;

INSERT INTO lab_qlik


SELECT member_qlik.[patid]
      ,lab.[fst_dt]
      ,lab.[source]
      ,lab.[anlytseq]
      ,lab.[hi_nrml]
      ,lab.[loinc_cd]
      ,lab.[low_nrml]
      ,lab.[abnl_cd]
      ,lab.[rslt_txt]
      ,lab.[rslt_nbr]
      ,lab.[tst_desc]
      ,lab.[tst_nbr]
      ,lab.[labclmid]
      ,lab.[rslt_unit_nm]
      ,member_qlik.[eligeff]
      ,member_qlik.[eligend]
  FROM member_qlik INNER JOIN lab on member_qlik.patid = lab.patid
  WHERE lab.fst_dt BETWEEN  '2012-01-01'AND '2012-12-31' 
AND lab.fst_dt BETWEEN  member_qlik.eligeff and member_qlik.eligend;

UPDATE lab_qlik

SET abnl_cd = NULL where abnl_cd = '';

INSERT INTO rx_qlik

SELECT member_qlik.[patid]
      ,rx.[chk_dt]
      ,rx.[fill_dt]
      ,rx.[days_sup]
      ,rx.[ndc]
      ,rx.[quantity]
      ,rx.[rfl_nbr]
      ,rx.[charge]
      ,rx.[brnd_nm]
      ,rx.[strength]
      ,rx.[gnrc_ind]
      ,rx.[clmid]
      ,rx.[fst_fill]
      ,member_qlik.[eligeff]
      ,member_qlik.[eligend]
  FROM member_qlik JOIN rx ON member_qlik.patid = rx.patid
  WHERE rx.fill_dt BETWEEN  '2012-01-01'AND '2012-12-31' 
AND rx.fill_dt BETWEEN  member_qlik.eligeff and member_qlik.eligend;

INSERT INTO patient_conditions_qlik (patid, lst_dt, Condition, eligeff, eligend)

select patid, lst_dt, diag1, eligeff, eligend from  medical_qlik

UNION

select patid, lst_dt, diag2, eligeff, eligend  from medical_qlik where diag2 <> ''

UNION

select patid, lst_dt, diag3, eligeff, eligend  from medical_qlik where diag3 <> ''

UNION

select patid, lst_dt, diag4, eligeff, eligend  from medical_qlik where diag4 <> ''

UNION

select patid, lst_dt, diag5, eligeff, eligend from medical_qlik where diag5 <> '';

INSERT INTO [advancedhealth].[dbo].[patient_procedures_qlik]
           ([patid]
           ,[lst_dt]
           ,[Procedure]
           ,eligeff
           ,eligend)
          
select patid, lst_dt, proc1,eligeff,eligend from medical_qlik where proc1 <> '' and proc1 <> 0 

UNION

select patid, lst_dt, proc2,eligeff,eligend from medical_qlik where proc2 <> '' and proc1 <> 0 

UNION

select patid, lst_dt, proc3,eligeff,eligend from medical_qlik where proc3 <> '' and proc1 <> 0 

delete from patient_procedures_qlik where [Procedure] = '00000'

INSERT INTO patient_cond_ct (patid, eligeff, eligend)

select patid, eligeff, eligend from member_qlik;


update patient_cond_ct
SET patient_cond_ct.ct_acmin = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'ACMIN' AND 
patient_conditions_qlik.lst_dt >= '2012-11-15' group by patid);


update patient_cond_ct
SET patient_cond_ct.ct_acmaj = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
patient_conditions_qlik.lst_dt >= '2012-11-15' AND 
ICD9_codes_Class.Class = 'ACMAJ' group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_recur = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'RECUR' group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_aller = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'ALLER' group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_crst = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'CRST' group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_crunst = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'CRUNST' group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_crspst = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'CRSPST' group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_crspun = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'CRSPUN' group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_psymin = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'PSYMIN' group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_psyrecst = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'PSYRECST' group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_psyrecun = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'PSYRECUN' group by patid);

/** If not delivered, count pregnancy minor conditions*/
update patient_cond_ct
SET patient_cond_ct.ct_pregnmin = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
                                            ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
                                         where patid = patient_cond_ct.patid AND 
                                         eligeff = patient_cond_ct.eligeff AND
                                         eligend = patient_cond_ct.eligend AND
                                               ICD9_codes_Class.Class = 'PREGNMIN' AND 
                                               ICD9_codes_Class.[Condition Category] = 'Pregnancy' AND 
                                               patient_cond_ct.patid not in 
                                               (select DISTINCT patient_conditions_qlik.patid from patient_conditions_qlik join ICD9_codes_Class
													   on patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
													   WHERE ICD9_codes_Class.[Condition Category] = 'Delivered') group by patid);

/** If not delivered, count pregnancy major conditions*/
update patient_cond_ct
SET patient_cond_ct.ct_pregnmin = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
                                            ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
                                         where patid = patient_cond_ct.patid AND 
                                         eligeff = patient_cond_ct.eligeff AND
                                         eligend = patient_cond_ct.eligend AND
                                               ICD9_codes_Class.Class = 'PREGNMAJ' AND 
                                               ICD9_codes_Class.[Condition Category] = 'Pregnancy' AND 
                                               patient_cond_ct.patid not in 
                                               (select DISTINCT patient_conditions_qlik.patid from patient_conditions_qlik join ICD9_codes_Class
													   on patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
													   WHERE ICD9_codes_Class.[Condition Category] = 'Delivered') group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_malig = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'MALIG' group by patid);

update patient_cond_ct
SET patient_cond_ct.ct_eyedent = (select COUNT(DISTINCT patient_conditions_qlik.Condition) from patient_conditions_qlik JOIN ICD9_codes_Class
ON patient_conditions_qlik.Condition = ICD9_codes_Class.[DIAGNOSIS CODE]
where patid = patient_cond_ct.patid AND 
eligeff = patient_cond_ct.eligeff AND
eligend = patient_cond_ct.eligend AND
ICD9_codes_Class.Class = 'EYEDENT' group by patid);

update patient_cond_ct
SET ct_acmin = 0 where ct_acmin IS NULL;

update patient_cond_ct
SET ct_acmaj = 0 where ct_acmaj IS NULL;

update patient_cond_ct
SET ct_recur = 0 where ct_recur IS NULL;

update patient_cond_ct
SET ct_aller = 0 where ct_aller IS NULL;

update patient_cond_ct
SET ct_crst = 0 where ct_crst IS NULL;

update patient_cond_ct
SET ct_crunst = 0 where ct_crunst IS NULL;

update patient_cond_ct
SET ct_crspst = 0 where ct_crspst IS NULL;

update patient_cond_ct
SET ct_crspun = 0 where ct_crspun IS NULL;

update patient_cond_ct
SET ct_psymin = 0 where ct_psymin IS NULL;

update patient_cond_ct
SET ct_psyrecst = 0 where ct_psyrecst IS NULL;

update patient_cond_ct
SET ct_psyrecun = 0 where ct_psyrecun IS NULL;

update patient_cond_ct
SET ct_pregnmin = 0 where ct_pregnmin IS NULL;

update patient_cond_ct
SET ct_pregnmaj = 0 where ct_pregnmaj IS NULL;

update patient_cond_ct
SET ct_malig = 0 where ct_malig IS NULL;

update patient_cond_ct
SET ct_eyedent = 0 where ct_eyedent IS NULL;



UPDATE member_qlik

SET member_qlik.risk_score = (patient_cond_ct.ct_acmin * cluster_weights.wt_acmin + 
                              patient_cond_ct.ct_acmaj * cluster_weights.wt_acmaj +
                              patient_cond_ct.ct_recur * cluster_weights.wt_recur +
                              patient_cond_ct.ct_aller * cluster_weights.wt_aller +
                              patient_cond_ct.ct_crst * cluster_weights.wt_crst +
                              patient_cond_ct.ct_crunst * cluster_weights.wt_crunst +
                              patient_cond_ct.ct_crspst * cluster_weights.wt_crspst +
                              patient_cond_ct.ct_crspun * cluster_weights.wt_crspun +
                              patient_cond_ct.ct_psymin * cluster_weights.wt_psymin + 
                              patient_cond_ct.ct_psyrecst * cluster_weights.wt_psyrecst +
                              patient_cond_ct.ct_psyrecun * cluster_weights.wt_psyrecun +
                              patient_cond_ct.ct_pregnmin * cluster_weights.wt_pregnmin +
                              patient_cond_ct.ct_pregnmaj * cluster_weights.wt_pregnmaj +
                              patient_cond_ct.ct_malig * cluster_weights.wt_malig +
                              patient_cond_ct.ct_eyedent * cluster_weights.wt_eyedent) 
                              from member_qlik JOIN patient_cond_ct ON 
                              member_qlik.patid = patient_cond_ct.patid AND 
                          member_qlik.eligeff = patient_cond_ct.eligeff AND
                              member_qlik.eligend = patient_cond_ct.eligend  JOIN
                            cluster_weights ON (2012-member_qlik.yrdob) between cluster_weights.AGE1 and cluster_weights.AGE2 ;

UPDATE member_qlik

SET member_qlik.risk_score = 100 where member_qlik.risk_score > 100 ;

CREATE TABLE patient_condition_category_qlik (patid VARCHAR(12), condition_category VARCHAR(40));

CREATE INDEX ix_cond_patid ON patient_condition_category_qlik(patid);

INSERT INTO patient_condition_category_qlik 

SELECT DISTINCT patient_conditions_qlik.patid,
       ICD9_codes_Class.[Condition Category] 
       FROM patient_conditions_qlik JOIN ICD9_codes_Class
       ON patient_conditions_qlik.condition = ICD9_codes_Class.[DIAGNOSIS CODE];
