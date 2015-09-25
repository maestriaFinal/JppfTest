-- M:\APPS\Solicitudes\Tracker_5124_5274\BD\pkg_mb621000pr.pkb
--
-- Generated for Oracle 9i on Thu Jun 16  10:58:54 2005 by Server Generator 6.5.93.2.10
 

PROMPT Creating Package Body 'PKG_MB621000PR'
CREATE OR REPLACE PACKAGE BODY PKG_MB621000PR IS
 
-- Sub-Program Unit Declarations

FUNCTION F_GET_BDT_CHARGE
 (P_EFF_DATE IN DATE
 ,P_RQST_BK_DATE IN DATE
 ,P_BDR_RQST_DATE IN DATE
 )
 RETURN RATE_RECORD_TYPE
 IS

vd_3828_4493 DATE;
RATE_RECORD PKG_MB621000PR.RATE_RECORD_TYPE;
vn_days number;

-- PL/SQL Block
BEGIN
    BEGIN
    SELECT TO_DATE(VAL,'DD-MON-YYYY')
    INTO vd_3828_4493
    FROM parameters
    WHERE typ_cd = 'BILLING'
    AND descr = '3828_4493';
    EXCEPTION WHEN NO_DATA_FOUND THEN
        vd_3828_4493 := TO_DATE('01-JAN-2005','DD-MON-YYYY');
    END;
	rate_record.eff_date	:= p_eff_date;
	rate_record.UNIT 		:= null;
	rate_record.QTY 		:= null;
	rate_record.RATE 		:= null;
    IF TRUNC(P_EFF_DATE) >= vd_3828_4493 THEN
	    vn_days := TRUNC(p_rqst_bk_date) - TRUNC(p_bdr_rqst_date);  
        IF vn_days >= TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN
       	    rate_record.ITEM_NO := '1050.0247';
        ELSIF vn_days < TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN
       	    rate_record.ITEM_NO := '1050.0248';
	    END IF;
	    rate_record.AMOUNT := SF_GET_EFF_RATE(rate_record.ITEM_NO,rate_record.EFF_DATE);
   	    rate_record.DESCRIPTION := 'SURCHARGE FOR DAYLIGHT TRANSIT RESERVATION - '||ROUND(vn_days)||' IN ADVANCE';
	ELSE
		rate_record := NULL;
	END IF;
RETURN rate_record;
END;
FUNCTION F_GET_CANCEL_BDT
 (P_EFF_DATE IN DATE
 ,P_RQST_BK_DATE IN DATE
 ,P_BDR_CANCL_DATE IN DATE
 ,P_BDR_RQST_DATE IN DATE
 ,P_BDR_STAT IN BK_DLT_RQSTS.STAT%TYPE
 ,P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_MODE IN VARCHAR2
 ,P_PCUMS IN NUMBER := null
 ,P_PC_GROSS IN NUMBER := null
 ,P_ON_DECK IN NUMBER := null
 ,P_GF_IND IN VARCHAR2 := 'null'
 ,P_HS_IND IN VARCHAR2 := 'null'
 ,P_BK_STAT IN VARCHAR2 := 'null'
 ,P_REQ_ARR IN VARCHAR2 := 'null'
 )
 RETURN RATE_RECORD_TYPE
 IS

vd_3828_4493 DATE;
RATE_RECORD PKG_MB621000PR.RATE_RECORD_TYPE;
CHARGE_TAB PKG_MB620000PR.CHARGE_TABLE_TYPE;
vn_days number(8,2);
vn_days_req number(8,2);
vn_hours NUMBER(8,2);
vd_bk_req_date DATE;
vn_bk_amt  CHARGES.CHRG_AMT%TYPE;
vd_date DATE;

--Tracker 5124
vd_canc_eff_date DATE;
vv_msg_3 VARCHAR2(2000);
vv_msg_4 VARCHAR2(2000);
BEGIN
    BEGIN
    SELECT TO_DATE(VAL,'DD-MON-YYYY')
    INTO vd_3828_4493
    FROM parameters
    WHERE typ_cd = 'BILLING'
    AND descr = '3828_4493';
    EXCEPTION WHEN NO_DATA_FOUND THEN
        vd_3828_4493 := TO_DATE('01-JAN-2005','DD-MON-YYYY');
    END; 
    --TRACKER 5124
    vd_canc_eff_date := PKG_MB620000PR.F_GET_BK_CANC_CHG(vv_msg_3,vv_msg_4);
    IF TRUNC(P_EFF_DATE) >= vd_3828_4493 THEN        
        charge_tab := PKG_MB620000PR.F_GET_BOOK_CHARGES(P_BR_SEQ,
                                                        'BF',
                                                        'O',
                                                        P_MODE, 
                                                        P_EFF_DATE, 
                                                        P_PCUMS, 
                                                        P_PC_GROSS, 
                                                        P_ON_DECK,
                                                        P_GF_IND, 
                                                        P_HS_IND, 
                                                        P_BK_STAT, 
                                                        P_RQST_BK_DATE, 
                                                        NULL, 
                                                        P_REQ_ARR);
        vn_bk_amt  := charge_tab(1).amount;
	    IF vn_bk_amt IS NOT NULL THEN      
	        vd_bk_req_date := TO_DATE(TO_CHAR(p_rqst_bk_date,'DD-MON-YYYY ')||sf_get_param_val(16,'BOOKING'),'DD-MON-YYYY HH24MI');
	        vn_days := ABS(vd_bk_req_date - p_bdr_cancl_date);
            vn_hours := ABS(vn_days)*24;
            rate_record := null;
  	        rate_record.eff_date	:= p_eff_date;
	        rate_record.UNIT 		:= null;
	        rate_record.QTY 		:= null;
	        rate_record.RATE 		:= null;
	        rate_record.DESCRIPTION := 'DAYLIGHT TRANSIT CANCELLATION CHARGE';
            IF TRUNC(p_rqst_bk_date) - TRUNC(p_bdr_rqst_date)  >= TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN
                IF vd_date < vd_canc_eff_date THEN
                    IF vn_days >= TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN
                        rate_Record.ITEM_NO := '1050.0249'; --tarifa sin cargo      
                    ELSIF vn_days BETWEEN 31 AND 59.99999 THEN
                        rate_Record.ITEM_NO := '1050.0251'; --tarifa de 10%      
                    ELSIF vn_days BETWEEN 22 AND 30.99999 THEN
                        rate_Record.ITEM_NO :=  '1050.0252';--tarifa de 40%
                    ELSIF vn_days BETWEEN 4 AND 21.99999 THEN
                        rate_Record.ITEM_NO :=  '1050.0253';--taarifa de 60%
                    ELSIF vn_days <= 3.99999 THEN
                        IF vn_hours > 36 THEN
                            rate_Record.ITEM_NO :=  '1050.0254';--tarifa de 80%
                        ELSIF vn_hours <= 36 THEN
                            rate_Record.ITEM_NO :=  '1050.0261';--tarifa de 100%            
         	            END IF;
                    END IF;
                ELSIF vd_date >= vd_canc_eff_date THEN  
                    IF vn_hours >= 1440 THEN
                        rate_Record.ITEM_NO := '1050.0249'; --tarifa sin cargo      
                    ELSIF vn_hours > 720 and vn_hours <= 1440 THEN
                        rate_Record.ITEM_NO := '1050.0251'; --tarifa de 10%      
                    ELSIF vn_hours > 504 and vn_hours <=  720 THEN
                        rate_Record.ITEM_NO :=  '1050.0252';--tarifa de 40%
                    ELSIF vn_hours >  72 and vn_hours <=  504 THEN
                        rate_Record.ITEM_NO :=  '1050.0253';--taarifa de 60%
                    ELSIF vn_hours >= 36 and vn_hours <=   72 THEN
                        rate_Record.ITEM_NO :=  '1050.0254';--tarifa de 80%
                    ELSIF vn_hours  < 36 THEN
                        rate_Record.ITEM_NO :=  '1050.0261';--tarifa de 100%            
         	        END IF;                        
                END IF;         
            ELSIF TRUNC(p_rqst_bk_date) - TRUNC(p_bdr_rqst_date) < TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN
                IF NVL(P_BDR_STAT,' ') = 'DTGUAR' THEN
                    IF vn_days < 2 THEN
                         rate_Record.ITEM_NO :=  '1050.0261';--tarifa de 100%
                    END IF;
                END IF;
            END IF;
       	    rate_record.AMOUNT := vn_bk_amt * SF_GET_EFF_RATE(rate_record.ITEM_NO,rate_record.EFF_DATE)/100;
        ELSE
           rate_record := null;
        END IF;
	END IF;		
RETURN rate_record;
END;
FUNCTION F_GET_CAN_PREV_STAT
 (P_SEQ IN BK_DLT_RQSTS.SEQ%TYPE
 ,P_STAT IN BK_DLT_RQSTS.STAT%TYPE
 )
 RETURN VARCHAR2
 IS
-- Private Declarations
VV_STAT BK_DLT_RQSTS.STAT%TYPE;
BEGIN
    SELECT STAT
          INTO VV_STAT
          FROM BK_DLT_STATS
         WHERE HIST_SEQ =(SELECT MAX(STATUS1.HIST_SEQ)
                            FROM BK_DLT_STATS STATUS1
                           WHERE STATUS1.BDR_SEQ = P_SEQ
                              AND STATUS1.HIST_SEQ IN (SELECT MAX(STATUS.HIST_SEQ)
                                                       FROM BK_DLT_STATS STATUS
                                                       WHERE STATUS.BDR_SEQ = P_SEQ 
														AND STATUS.STAT <> P_STAT));
    RETURN (vv_stat);
EXCEPTION WHEN OTHERS THEN
    RETURN (NULL);
END;

END PKG_MB621000PR;
/
SHOW ERROR

