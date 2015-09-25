create or replace PACKAGE BODY       PKG_BOOKING IS

PROCEDURE P_INS_BK_RQST_DETS
 (P_REC vw_bk_rqst_dets%ROWTYPE
 );
FUNCTION F_GET_ACT_BP_ID
 (PV_SLOT_TYP IN BK_SIZE_TYPS.TYP%TYPE
 ,PV_CLASS IN BK_SIZE_TYPS.CLASS%TYPE
 ,PD_RQST_BK_DATE IN BK_RQSTS.RQST_BK_DATE%TYPE
 ,PN_BP_ID IN BK_RQSTS.BP_ID%TYPE
 )
 RETURN NUMBER;

PROCEDURE P_INS_BK_RQST_DETS
 (P_REC vw_bk_rqst_dets%ROWTYPE
 )
 IS

--============================================================================
-- DESCRIPCION: Este procecimiento se encarga de grabar registros en la tabla
 -- de BK_RQSTS_DETS.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha       Cambios realizados
-- ------- -------------  ----------  -----------------------------------------
-- S61519   VJaen         15-Jan-2008 Version inicial.
--=============================================================================
BEGIN
      pkg_plog.sec_implementation ('PKG_BOOKING.P_INS_BK_RQST_DETS',
                                      'br_seq='
                                   || p_rec.br_seq
                                   || ' bs_seq='
                                   || p_rec.bs_seq
                                   || ' trns_type='
                                   || p_rec.trns_type
                                   || ' br_seq_dest='
                                   || p_rec.br_seq_dest
                                   || ' bs_seq_dest='
                                   || p_rec.bs_seq_dest
                                   || ' fix_amt='
                                   || p_rec.fix_amt
                                   || ' amount='
                                   || p_rec.amount,
                                   'INFO'
                                  );

      --
      --
      INSERT INTO vw_bk_rqst_dets
                  (br_seq, bs_seq, trns_type,
                   br_seq_dest, bs_seq_dest, fix_amt,
                   amount, deleted
                  )
           VALUES (p_rec.br_seq, p_rec.bs_seq, p_rec.trns_type,
                   p_rec.br_seq_dest, p_rec.bs_seq_dest, p_rec.fix_amt,
                   p_rec.amount, 'N'
                  );
   EXCEPTION
      WHEN OTHERS
      THEN
         pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);
   END p_ins_bk_rqst_dets;
PROCEDURE P_PROCESS_BOOKING
 (P_VSL_TYPE IN SHIP_ID_NO.VTY_CD%TYPE
 ,P_CSN_SEQ IN CUST_SCHED_NEEDS.SEQ%TYPE
 ,P_BS_SEQ IN BK_RQSTS.BS_SEQ%TYPE
 ,P_ITI_SEQ IN ITIN_ITEMS.SEQ%TYPE
 ,P_CUST_CD IN CONTACTS.SHORT_NAME%TYPE
 ,P_REAL_CUST_CD IN CONTACTS.SHORT_NAME%TYPE
 ,P_DATE_CREATED IN BK_RQSTS.RQST_DATE%TYPE
 ,P_BK_DATE IN BK_RQSTS.RQST_BK_DATE%TYPE
 ,P_TRN_DIR IN ITIN_ITEMS.TII_TRN_DIR%TYPE
 ,P_ITI_TRN_DIR IN ITIN_ITEMS.TII_TRN_DIR%TYPE
 ,P_EXTM_BEAM IN CUST_VSL_CHARS.EXTM_BEAM%TYPE
 ,P_BEAM IN CUST_VSL_CHARS.EXTM_BEAM%TYPE
 ,P_HML IN CUST_VSL_CHARS.LVC_HML_QUALIFIED%TYPE
 ,P_REST IN REST_USR_CDS.CD%TYPE
 ,P_GAS_FREE_IND IN VARCHAR2
 ,P_PDE_CD IN ITIN_ITEMS.PDE_CD%TYPE
 ,P_DTU_INT IN WEB_BK_REQUESTS.DTU_INT%TYPE
 ,P_AGENT IN CONTACTS.SHORT_NAME%TYPE
 ,P_DTU_SEQ IN ITIN_ITEMS.DTU_GROUP_BY_SEQ%TYPE
 ,P_TUG_NO IN SHIP_ID_NO.SHIP_NO%TYPE
 ,P_BARGE_NO IN SHIP_ID_NO.SHIP_NO%TYPE
 ,P_BR_SEQ OUT BK_RQSTS.SEQ%TYPE
 ,P_BK_STAT OUT BK_RQSTS.STAT%TYPE
 ,P_BK_COND OUT BK_CONDS.LEV%TYPE
 ,P_ERROR_CODE OUT VARCHAR2
 ,P_SUCCESS OUT VARCHAR2
 ,P_NO_FRFT_CD IN BK_RQSTS.NO_FRFT_CD%TYPE := NULL
 ,P_LOA IN CUST_VSL_CHARS.LEN_OVERALL%TYPE := NULL
 ,P_SPEC_BOOK_IND IN VARCHAR2 := NULL
 ,P_AMOUNT IN BK_RQST_DETS.AMOUNT%TYPE := NULL
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de procesar booking requests.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador         Fecha              Cambios realizados
-- -----------     --------------------        ----------             -----------------------------------------
-- R3430        GVillarreal               21-oct-2003    Version Inicial transportada desde el
--                                                                                   procedimiento P_PROCESS_BOOKING
--                                                                                   de la pantalla VI5060FM (Process
--                                                                                   Booking  Request
--                                                       11-nov-2003     Desactivar cálculo de booking fee, period, arrival
--                                                                                   time para calcularlo en forma externa en el
--                                                                                   procedimiento P_RTV_BOOK_FEE
--                                                       12-nov-2003     Inclusión de llamado de procedimiento para
--                                                                                   manejo de errores
--                                                       24-nov-2003     Inclusión de uso de vistas para booking requests
--                                                                                   como parte del proceso de booking se requiere
--                                                                                   regresar la información de pc_ums
--                                                        11-dec-2003   Inclusión de parámetros de salida P_BR_SEQ ,
--                                                                                   P_BK_STAT  y  P_BK_COND
--                                                        30-dec-2003   Cambio de status rmk a comentarios de
--                                                                                   varchar2(50) temporalmente mientras se arregla
--                                                                                  el tamaño en la tabla WEB_BK_REQUESTS y
--                                                                                  WEB_BK_RQST_TRANS
--                                                        05-jan-2004   Cambios a comentarios normales de status con
--                                                                                  largo de 250 caracteres
--                                                         07-jan-2004   Inclusión de cambio de fecha para procesamiento
--                                                                                 de año y evaluacion de periodo de reservacion
--                                                                                 para buques de pasajeros
--                                                        09-jan-2004   Inclusión de cambios con respecto al manejo de
--                                                                                 reservaciones para DTUs
--                                                        06-feb-2004  Cambios a comentarios de REJ (stat_rmk) de
--                                                                                 250 caracteres a 50 caracteres para EDCS Web
--                                                        17-feb-2004   Desactivar validación de gas free indicator y beam
--                                                       16-mar-2004  Cambiar validación de booking date contra 2 días
--                                                                                  antes en lugar de system date es request date
-- R4104        GVillarreal              18-may-2004   Cambiar validación de hora de competencia para
--                                                                                   incluir parametros de hora de inicio y fin de la
--                                                                                   competencia  y validación de la hora del sistema para
--                                                                                   competencia
--S4332         Jdam                        6-mar-2006     Subasta de booking
--
-- S9127       Jadiaz                     25-may-2006   Adicion de indicadores TYP y BP_ID para aquellos
--                                                                                 casos  en que   la reservacion de cupo es rechazada
--                                                                                 (REJECT).
-- S15142     GVillarreal             26-sep-2006     Inclusión de SUBSTR para envío de código USR en
--                                                                                 caso   de error desde trigger de BD (Particularmente
--                                                                                 error en   BR_B_I_R_RQST_DATE
-- S61519      GVillarreal            08-jan-2007      S59588/S61519: Nueva versión del proceso de booking
--                                                                                 con cambios por nuevos periodos (Passengers, Best
--                                                                                 Offer) y nuevas estructuras
--=============================================================================
--
--Cursor de Periodos Válidos
CURSOR C_Valid_Periods (p_rqst_bk_date       IN DATE,
                                                   p_rqst_date              IN DATE,
                                                   p_slot_typ                 IN BK_SIZE_TYPS.TYP%TYPE)
IS
SELECT bps.bp_id,
                bps.start_day,
                bps.end_day,
                bsps.seq  bsps_seq,
                bsps.bps_seq,
                bsps.bstp_seq,
                bstp.typ,
                bstp.class
    FROM BK_SIZE_PER_SEGS bsps,
                BK_PER_SEGS            bps,
                BK_SIZE_TYPS            bstp
 WHERE bsps.bps_seq  = bps.seq
       AND bsps.bstp_seq = bstp.seq
       AND bsps.active       = 'Y'
       AND bstp.typ             = p_slot_typ
       AND TRUNC(p_rqst_bk_date) - TRUNC(p_rqst_date) BETWEEN start_day and end_day;


--Cursor de Condiciones
CURSOR C_Num_Cond  (p_bd_seq  IN  BK_DAYS.seq%TYPE,
                                              p_date        IN  BK_COND_TYPS.DATE_FROM%TYPE,
                                              p_slot_typ IN  BK_SIZE_TYPS.TYP%TYPE,
                                              p_class      IN  BK_SIZE_TYPS.CLASS%TYPE ) IS
SELECT bctp.bco_lev,
                no_max_slots
   FROM  BK_COND_TYPS              bctp,
                BK_SIZE_TYPS                 bstp,
                BK_CONDS                        cond,
                BK_DAY_COND_ASSGS bdca,
                BK_DAYS                           days
 WHERE bctp.bco_lev    = cond.lev
       AND bctp.bstp_seq = bstp.seq
       AND cond.lev   = bdca.bco_lev
       AND bdca.seq = days.bdca_seq
       AND TRUNC(bctp.date_from) <= TRUNC(p_date)
       AND TRUNC(bctp.date_to)       >= TRUNC(p_date)
       AND days.seq = p_bd_seq
       AND bstp.typ   = p_slot_typ
       AND bstp.class = p_class;



-- Variables para insertar en la tabla de Bk_Rqsts
vn_seq                              BK_RQSTS.seq%TYPE;
vv_srce                             BK_RQSTS.srce%TYPE            := 'AGENT';
vv_stat                              BK_RQSTS.stat%TYPE;
vd_stat_date                   BK_RQSTS.stat_date%TYPE;
vv_stat_rmk                    BK_RQSTS.stat_rmk%TYPE;
vv_stat_rmk_tmp          WEB_BK_REQUESTS.reason_for_rej%TYPE;
vn_bs_seq                      BK_RQSTS.bs_seq%TYPE          := p_bs_seq;
vv_rule_apply_ind         BK_RQSTS.rule_apply_ind%TYPE  := 'Y';
vn_crra_seq                   BK_RQSTS.crra_seq%TYPE        := F_FIND_AGENT_SEQ(p_agent);
--
-- Added by JaDiaz on September 23, 2008 S83474
vd_rqst_date                  BK_RQSTS.rqst_date%TYPE       := p_date_created;
vd_org_rqst_date              BK_RQSTS.rqst_date%TYPE       := p_date_created;
--
vd_bk_rqst_date           BK_RQSTS.rqst_bk_date%TYPE    := p_bk_date;
vv_rest_when_bk         BK_RQSTS.rest_when_bk%TYPE;
vn_bd_seq                      BK_RQSTS.bd_seq%TYPE;
vv_bp_id                          BK_RQSTS.bp_id%TYPE;
vv_typ                               BK_RQSTS.typ%TYPE;
vv_spc_slot_assg         BK_RQSTS.spc_slot_assg%TYPE   := 'N';
--
vn_per                               BK_PERS.id%TYPE;
vn_beam                          CUST_VSL_CHARS.extm_beam%TYPE := p_extm_beam;
vv_slot_typ                      VARCHAR2(5);
vn_direction                    ITIN_ITEMS.tii_trn_dir%TYPE   := p_trn_dir;
vn_param_valid             NUMBER;
vn_exist_vessel            NUMBER;
vn_per_date_from        NUMBER;
vd_pass_date_from     DATE;
vd_pass_date_to          DATE;
vv_open                           VARCHAR2(1);

e_null_slots                    EXCEPTION;
e_large_cond                 EXCEPTION;
e_small_cond                EXCEPTION;
e_no_pd_code               EXCEPTION;
e_dtu                                EXCEPTION;
e_already_bk                 EXCEPTION;
e_for_compet                EXCEPTION;
e_invalid_dir                   EXCEPTION;
e_invalid_bk_day          EXCEPTION;
e_invalid_cust_cd        EXCEPTION;
e_invalid_beam             EXCEPTION;
e_gas_free_is_missing           EXCEPTION;
--4484
e_small_dir	                  EXCEPTION;
e_large_dir	                  EXCEPTION;
e_large_dir_rest                        EXCEPTION;
e_small_dir_rest                       EXCEPTION;
--R.5554
e_max_large_cond                   EXCEPTION;

vn_rest_vess                              NUMBER := 0;
vn_large_vess                            BK_CONDS.no_large_vsl%TYPE;
vn_small_vess                           BK_CONDS.no_small_vsl%TYPE;
vn_cond                                       BK_CONDS.lev%TYPE;
vv_vessel_type                          VSL_TYPS.cd%TYPE              := p_vsl_type;
vn_per_date_to                          BK_PERS.date_to%TYPE;
--
--
--R3746: Added on January 09, 2004--GNV
vd_barge_stat_date                 DATE;
vv_barge_rest_when_bk        BK_RQSTS.rest_when_bk%TYPE;
vn_barge_bd_seq                     BK_RQSTS.bd_seq%TYPE;
vn_barge_bp_id                         BK_RQSTS.bp_id%TYPE;
vv_barge_typ                              BK_RQSTS.typ%TYPE;
e_no_barge_info                       EXCEPTION;

--R4104: Added on May 18, 2004 -- GNV
vn_begin_compet                     NUMBER    := TO_NUMBER(Sf_Get_Param_Val(210,'BOOKING'));
vn_end_compet                        NUMBER    := TO_NUMBER(Sf_Get_Param_Val(220,'BOOKING'));
--
--R.4591
vn_between_para_month        NUMBER(3);
--R.4484
vv_message                                VARCHAR2(16);
--

--S15142: Added on September 26, 2006 -- GNV
vv_sqlerrm                                   VARCHAR2(2000);

--S22195:Added on Diciembre 14, 2006 --LCedeno
e_max_small_cond	EXCEPTION;

--S59588/S61519: Added on January 10, 2007 -- GNV
vn_br_seq                       BK_RQSTS.seq%TYPE;
vv_br_stat                       BK_RQSTS.stat%TYPE;
vn_bk_cond                    BK_CONDS.lev%TYPE;
vn_fix_amount               BK_RQSTS.fix_amt%TYPE;
vv_error_code               VARCHAR2(30);
vv_success                    VARCHAR2(5);
vn_max_slots                BK_COND_TYPS.no_max_slots%TYPE;
vv_predef_bo                 BK_SIZE_TYPS.class%TYPE;
vv_predef_xpiece         BK_SIZE_TYPS.class%TYPE;
vn_bsps_seq                 BK_SIZE_PER_SEGS.seq%TYPE;
vn_bps_seq                   BK_PER_SEGS.seq%TYPE;
vn_bstp_seq                  BK_SIZE_TYPS.seq%TYPE;
vv_bstp_typ                    BK_SIZE_TYPS.typ%TYPE;
vv_bstp_class               BK_SIZE_TYPS.class%TYPE;
vn_brd_seq                    BK_RQST_DETS.seq%TYPE;
vv_ins_bof_amount        VARCHAR2(1) := 'N';
vv_book_xpiece               VARCHAR2(1) := 'N';
vn_barge_bsps_seq    BK_SIZE_PER_SEGS.seq%TYPE;

e_invalid_amount                       EXCEPTION;
e_invalid_beam_bo                   EXCEPTION;
e_period_closed                        EXCEPTION;
--Addition ends
BEGIN

--S59588/S61519: Added on January 08, 2007 -- GNV
--New version of booking process valid from 01-FEB-2008
--Old methodology of booking process will be still valid
--until January 31,2008. It will be handled thru
--procedure P_PROCESS_BK_BEF_V1

 Pkg_Plog.sec_implementation ('PKG_BOOKING.P_PROCESS_BOOKING',
                                 SUBSTR(
                                   'P_Vsl_Type= '
                                 || p_vsl_type
                                 || CHR (10)
                                 ||'P_Csn_seq= '
                                 || TO_CHAR(p_csn_seq)
                                 || CHR (10)
                                 ||'p_bs_seq= '
                                 || TO_CHAR(p_bs_seq)
                                 || CHR (10)
                                 ||'p_iti_seq= '
                                 || TO_CHAR(p_iti_seq)
                                 || CHR (10)
                                 ||'p_cust_cd= '
                                 || p_cust_cd
                                 || CHR (10)
                                 ||'p_real_cust_cd= '
                                 || p_real_cust_cd
                                 || CHR (10)
                                 ||'p_date_created= '
                                 || TO_CHAR(p_date_created,'DD-MON-YYYY HH24MI')
                                 || CHR (10)
                                 ||'p_bk_date= '
                                 || TO_CHAR(p_bk_date,'DD-MON-YYYY HH24MI')
                                 || CHR (10)
								 ||'p_trn_dir= '
                                 || p_trn_dir
                                 || CHR (10)
								 ||'p_iti_trn_dir= '
                                 || p_iti_trn_dir
                                 || CHR (10)
								 ||'p_extm_beam= '
								 || TO_CHAR(p_extm_beam)
								 || CHR (10)
								 ||'p_beam= '
								 || TO_CHAR(p_beam)
								 || CHR (10)
								 ||'p_hml= '
								 || p_hml
								 || CHR (10)
								 ||'p_rest= '
								 || p_rest
								 || CHR (10)
								 ||'p_gas_free_ind= '
								 || p_gas_free_ind
								 || CHR (10)
								 ||'p_pde_cd= '
								 || p_pde_cd
								 || CHR (10)
								 ||'p_dtu_int= '
								 || p_dtu_int
								 || CHR (10)
								 ||'p_agent= '
								 || p_agent
								 || CHR (10)
								 ||'p_dtu_seq= '
								 || TO_CHAR(p_dtu_seq)
								 || CHR (10)
								 ||'p_tug_no= '
								 || TO_CHAR(p_tug_no)
								 || CHR (10)
								 ||'p_barge_no= '
								 || TO_CHAR(p_barge_no)
								 || CHR (10)
								 ||'p_br_seq= '
								 || TO_CHAR(p_br_seq)
								 || CHR (10)
								 ||'p_bk_stat= '
								 || p_bk_stat
								 || CHR (10)
								 ||'p_bk_cond= '
								 || TO_CHAR(p_bk_cond)
								 || CHR (10)
								 ||'p_error_code= '
								 || p_error_code
								 || CHR (10)
								 ||'p_success= '
								 || p_success
								 || CHR (10)
								 ||'p_no_frft_cd= '
								 || p_no_frft_cd
								 || CHR (10)
								 ||'p_loa= '
								 || TO_CHAR(p_loa)
								 || CHR (10)
								 ||'p_spec_book_ind= '
								 || p_spec_book_ind
								 || CHR (10)
								 ||'p_amount= '
								 || TO_CHAR(p_amount),1,1950),
                                   'INFO');

IF p_date_created >= PKG_BOOK_DATADICT.f_bk_proc_eff_date_v1 THEN
   --
   -- Verifica si el customer code de la solicitud es igual al del itinerario a reservar.
   IF NVL(p_real_cust_cd, '.') <> NVL(p_cust_cd, '@') THEN

      RAISE e_invalid_cust_cd;

   END IF;--IF NVL(p_real_cust_cd, '.') <> NVL(p_cust_cd, '@') THEN
   --

   --S59588/S61519: Added on January 08, 2007 -- GNV
   IF p_spec_book_ind IN (PKG_BOOK_DATADICT.f_bso_bo,PKG_BOOK_DATADICT.f_bso_au) AND
      NVL(p_amount,0) <= 0 THEN

      RAISE e_invalid_amount;

   END IF;--IF p_spec_book_ind IN (PKG_BOOK_DATADICT.f_bso_bo,PKG_BOOK_DATADICT.f_bso_au) AND

   --S4332 16-MAR-2006
   --IF NVL(P_FIX_AMT,0) = 0 THEN
   --S59588/S61519: Changed on January 08, 2007 -- GNV
   IF NVL(P_SPEC_BOOK_IND,' ') <> PKG_BOOK_DATADICT.f_bso_au THEN

      --Verifica si la solicitud fue recibida durante la recoleccion para el periodo de competencia
      --R4104: Regla para validar la hora de competencia para booking
      --1. Hora del sistema se encuentra entre hora de inicio y fin del periodo de competencia
      --   a. Fecha de Request del Booking es igual a la fecha del sistema
      --      a.1 Hora de la fecha de request del booking está entre la hora de inicio y fin de la
      --          competencia
      --          Se dispara exception de que no se puede reservar hasta que termine la competencia
      --      a.2 Hora no está entre el periodo de competencia
      --          Se realiza el proceso de reservación
      --   b. Fecha del Request del booking no es igual a la fecha del sistema
      --      Se realiza el proceso de reservación
      --2. Hora del sistema no está entre el periodo de competencia
      --   Se realiza la reservacion
      --
      IF TO_NUMBER(TO_CHAR(Sf_Get_Sys_Date(),'HH24MI')) BETWEEN vn_begin_compet AND vn_end_compet THEN

         IF TRUNC(p_date_created) = TRUNC(Sf_Get_Sys_Date) THEN

            IF TO_NUMBER(TO_CHAR(p_date_created, 'HH24MI'))   BETWEEN vn_begin_compet AND vn_end_compet THEN

               RAISE e_for_compet;

            END IF;--IF TO_NUMBER(TO_CHAR(p_date_created, 'HH24MI'))   BETWEEN vn_begin_compet AND vn_end_compet THEN

         END IF;--IF TRUNC(p_date_created) = TRUNC(Sf_Get_Sys_Date) THEN

      END IF;--IF TO_NUMBER(TO_CHAR(Sf_Get_Sys_Date(),'HH24MI')) BETWEEN vn_begin_compet AND vn_end_compet THEN

   END IF;-- S4332 --IF NVL(P_SPEC_BOOK_IND,' ') <> PKG_BOOK_DATADICT.f_bso_au THEN

   --
   -- Verifica si la nave ya está booked
   IF Sf_Get_Bk_Existence(p_iti_seq) THEN

      RAISE e_already_bk;

   END IF;--IF Sf_Get_Bk_Existence(p_iti_seq) THEN


   -- Verifica si la direccion de transito de la reservacion no coincide con la del itinerario
   IF p_trn_dir <> p_iti_trn_dir THEN

      RAISE e_invalid_dir;

   END IF;
   --

   -- S4332 16-MAR-2006
   --IF NVL(P_FIX_AMT,0) = 0 THEN
   --S59588/S61519: Changed on January 08, 2007 -- GNV
   IF NVL(P_SPEC_BOOK_IND,' ') <> PKG_BOOK_DATADICT.f_bso_au THEN

      -- Verifica si la fecha del booking es valida
      --R3746: Changed on March 16, 2004 -- GNV

      IF TRUNC(p_bk_date) - TRUNC(vd_rqst_date) < 2 THEN

         RAISE e_invalid_bk_day;

      END IF;--IF TRUNC(p_bk_date) - TRUNC(vd_rqst_date) < 2 THEN

   END IF; -- S4332 16-MAR-2006 --IF NVL(P_SPEC_BOOK_IND,' ') <> PKG_BOOK_DATADICT.f_bso_au THEN


   --Select the day sequence for apply the booking
   --
   BEGIN

      SELECT seq
        INTO vn_bd_seq
        FROM BK_DAYS
       WHERE TRUNC(DATE_FROM) = TRUNC(vd_bk_rqst_date);

   EXCEPTION
      WHEN OTHERS THEN

          vn_bd_seq := NULL;

   END;
   --
   --S59588/S61519: Changed on January 08, 2007 -- GNV
   --               using the SF_GET_BK_TYP function
   -- Establish the vessel type with beam
   vv_slot_typ := sf_get_bk_typ(vn_beam);

   --Validations for best offer booking
   IF p_spec_book_ind =  PKG_BOOK_DATADICT.f_bso_bo THEN

      IF NVL(vv_slot_typ,' ') <> PKG_BOOK_DATADICT.f_bk_typ_large  THEN

         RAISE e_invalid_beam_bo;

      --Get Best Offer Slot Type Classification Pre-defined
      ELSE

         vv_predef_bo := PKG_BOOK_DATADICT.f_bvtc_bestoff;

      END IF;--IF vv_slot_typ     <> PKG_BOOK_DATADICT.f_bk_typ_large  THEN

   END IF;--IF p_spec_book_ind =  PKG_BOOK_DATADICT.f_bso_bo        AND

   --Validations for setting X-PIECE classification
   --Set X-PIECE class for a Vessel
   --with LOA <= 300 in the case the
   --SMALL/REGULAR slot is not available
   IF NVL(p_spec_book_ind,' ')  <>  PKG_BOOK_DATADICT.f_bso_bo  THEN

      IF vv_slot_typ =  PKG_BOOK_DATADICT.f_bk_typ_small THEN

         IF NVL(p_loa,0) > 0 AND
            NVL(p_loa,0) <= PKG_BOOK_DATADICT.f_bk_loa_xpiece THEN

            vv_predef_xpiece := PKG_BOOK_DATADICT.f_bvtc_xpiece;

         END IF;--IF NVL(p_loa,0) > 0 AND

      END IF; --IF vv_slot_typ =  PKG_BOOK_DATADICT.f_bk_typ_small THEN

   END IF;--IF NVL(p_spec_book_ind,' ')  <>  PKG_BOOK_DATADICT.f_bso_bo       THEN

   --S59588/S61519: Changed on January 08, 2007 -- GNV
   --               Read the maximum quantity of slots according to
   --               the slot type classification. This cursor is no
   --               longer valid
   /*
   OPEN NUM_COND(vn_bd_seq);
      FETCH NUM_COND INTO vn_large_vess,vn_small_vess,vn_cond;
   CLOSE NUM_COND;
   */

   -- S4332
   --IF NVL(P_FIX_AMT,0) = 0 THEN
   --S59588/S61519: Changed on January 08, 2007 -- GNV
   IF NVL(P_SPEC_BOOK_IND,' ') <> PKG_BOOK_DATADICT.f_bso_au THEN

      --
      --    Inicio de la evaluacion del booking
      BEGIN
        --
        -- R1889: Valores de HML para el Booking y mensaje de warning
        -- Se solicita confirmacion, cuando se esta intentdo bookear una nave tipo '04' (Tanker),
        -- y el PD Code es 'H' o '#'.
        -- FIN.
        --Select the period for the actual request date by beam
        --
        IF p_no_frft_cd IS NULL THEN     --Adicionado para R. 4887
           --
           IF TO_CHAR(p_date_created,'HH24MI') < '0900' THEN -- if/then added by mrxp-tnv apr/10/2000
              vd_rqst_date := vd_rqst_date - 1 ;
           END IF;
           --
           --S59588/S61519: Changed on January 08, 2007 -- GNV
           /*
           BEGIN
             SELECT id,
                    date_from,
                    date_to
             INTO   vn_per,
                    vn_per_date_from,
                    vn_per_date_to
             FROM   BK_PERS
             WHERE  TRUNC(vd_bk_rqst_date)-TRUNC(vd_rqst_date) BETWEEN date_from AND date_to;
           EXCEPTION
             WHEN OTHERS THEN
               vn_per := NULL;
               vn_per_date_from := NULL;
               vn_per_date_to   := NULL;
           END;
           */

           --Read Valid Period Segments according to slot type
           FOR x IN C_Valid_Periods(vd_bk_rqst_date,
                                    vd_rqst_date,
                                    vv_slot_typ)
           LOOP

              --If Booking Indicator is Best Offer
              --locate the valid period for Best Offer
              IF p_spec_book_ind = PKG_BOOK_DATADICT.f_bso_bo THEN

                 IF vv_predef_bo = x.class THEN

                    vn_per           := x.bp_id;
                    vn_per_date_from := x.start_day;
                    vn_per_date_to   := x.end_day;
                    vn_bsps_seq      := x.bsps_seq;
                    vn_bps_seq       := x.bps_seq;
                    vn_bstp_seq      := x.bstp_seq;
                    vv_bstp_typ      := x.typ;
                    vv_bstp_class    := x.class;

                 END IF;--IF vv_predef_bo = x.class THEN

              ELSE

                 --Set the valid period for Passenger vessels
                 IF p_vsl_type = '11' THEN

                    IF  x.class IN (PKG_BOOK_DATADICT.f_bvtc_pax,
                                    PKG_BOOK_DATADICT.f_bvtc_super,
                                    PKG_BOOK_DATADICT.f_bvtc_reg)


                    THEN

                       vn_per           := x.bp_id;
                       vn_per_date_from := x.start_day;
                       vn_per_date_to   := x.end_day;
                       vn_bsps_seq      := x.bsps_seq;
                       vn_bps_seq       := x.bps_seq;
                       vn_bstp_seq      := x.bstp_seq;
                       vv_bstp_typ      := x.typ;
                       vv_bstp_class    := x.class;

                    END IF;--IF  x.class IN (PKG_BOOK_DATADICT.f_bvtc_pax,

                 ELSE

                    --Set the valid normal periods only if Valid Periods
                    --are not Best Offer and XPiece
                    IF x.class NOT IN (PKG_BOOK_DATADICT.f_bvtc_bestoff,
                                       PKG_BOOK_DATADICT.f_bvtc_xpiece,
                                       PKG_BOOK_DATADICT.f_bvtc_pax)
                    THEN

                       vn_per           := x.bp_id;
                       vn_per_date_from := x.start_day;
                       vn_per_date_to   := x.end_day;
                       vn_bsps_seq      := x.bsps_seq;
                       vn_bps_seq       := x.bps_seq;
                       vn_bstp_seq      := x.bstp_seq;
                       vv_bstp_typ      := x.typ;
                       vv_bstp_class    := x.class;

                    END IF; --IF x.class NOT IN (PKG_BOOK_DATADICT.f_bvtc_bo,

                 END IF; --IF p_vsl_type = '11' THEN

              END IF; --IF p_spec_book_ind = PKG_BOOK_DATADICT.f_bso_bo THEN

           END LOOP;--FOR x IN C_Valid_Periods(vd_bk_rqst_date,

           --If period was not open for corresponding booking
           --raise an error
           IF vn_per IS NULL THEN

              RAISE e_period_closed;

           END IF; --IF vn_per IS NULL THEN

           --Get the maximum number of slots
           OPEN c_num_cond(vn_bd_seq,
                           vd_rqst_date,
                           vv_slot_typ,
                           vv_bstp_class);
           IF c_num_cond%ISOPEN THEN

              FETCH c_num_cond INTO vn_cond, vn_max_slots;

              IF c_num_cond%NOTFOUND THEN

                 vn_cond      := NULL;
                 vn_max_slots := NULL;

              END IF; --IF c_num_cond%NOTFOUND THEN

              CLOSE c_num_cond;

           END IF;--IF c_num_cond%ISOPEN THEN

		   IF NVL(vv_bstp_class,' ') IN (Pkg_Book_Datadict.f_bvtc_super,
                                         Pkg_Book_Datadict.f_bvtc_reg)
		   THEN

			  IF vn_cond IS NOT NULL THEN

                 vn_max_slots := PKG_BOOK_REFTAB_UTIL.F_GET_TOT_SLOT_COND(vv_slot_typ,vn_cond,NULL);

			  END IF; --IF vn_cond IS NOT NULL THEN

		   END IF; --IF vv_bstp_clss IN (Pkg_Book_Datadict.f_bvtc_super,

           --S59588/S61519: Desactivated on January 08, 2007 -- GNV
           --This variable VV_OPEN is not used in any place of the program
           -- Verify the open period condition
           /*
           IF (TRUNC(vd_bk_rqst_date)-TRUNC(vd_rqst_date)) = vn_per_date_to THEN
               BEGIN
                 SELECT 'Y'
                 INTO   vv_open
                 FROM   dual
                 WHERE  Sf_Get_Param_Val(10,'BOOKING') <= TO_CHAR(vd_rqst_date,'HH24MI');
               EXCEPTION
                 WHEN OTHERS THEN
                   vv_open := NULL;
               END;
           END IF;
           */
           --
        END IF;   --Adicionado para R. 4887
        --

        --S59588/S61519: Changed on January 08, 2007 -- GNV
        --               Moved before the NUM_CONDS cursor
        --               using the SF_GET_BK_TYP function
        -- Establish the vessel type with beam
        /*
        IF vn_beam >= Sf_Get_Param_Val(56,'BOOKING') THEN
           vv_slot_typ := 'LARGE';
        ELSE
           vv_slot_typ := 'SMALL';
        END IF;
        */
        --
        --  avoid the evaluation of the itegrated barge units
        IF (NVL(p_dtu_int,' ') = 'N' AND p_dtu_seq IS NULL)OR
           (p_dtu_seq IS NOT NULL    AND
            NVL(p_dtu_int,' ') = 'Y' AND
            NVL(p_vsl_type,' ') IN ('13','14','15','23','24','25','26')) THEN

            BEGIN

              -- Pending check for the restricted vessel
              -- Checking for direction rules
              --S59588/S61519: Changed on January 08, 2007 -- GNV
              --IF vv_slot_typ ='LARGE' THEN
              IF vv_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large THEN

                 --
                 -- Select the number of existing records with a give condition
                 /*CONDITIONS:
                   1.  When the day transit is equal to day transit of request ship
                   2.  When have a slot assigns
                   3.  When the transit direction is equal of the request vessel
                   4.  When the vessel not have status of Cancell,Rejected or Void
                 */

                 -- For validate the number of vessel allowed by the actual condition
                 --S59588/S61519: Changed on January 08, 2007 -- GNV
                 --IF NVL(Sf_Get_Vessel_By_Dir(3,vn_bd_seq,'',vv_slot_typ),0) >= NVL(vn_large_vess,0) THEN

                 --Evaluate number of allowed slots by class

                 IF NVL(vv_bstp_class,' ') IN  (PKG_BOOK_DATADICT.f_bvtc_bestoff, PKG_BOOK_DATADICT.f_bvtc_pax) THEN

                    IF NVL(Sf_Get_Vessel_By_Dir(6,vn_bd_seq,'',vv_slot_typ,vv_bstp_class),0) >= NVL(vn_max_slots,0) THEN

                       RAISE e_large_cond;

                    END IF;--IF NVL(Sf_Get_Vessel_By_Dir(6,vn_bd_seq,'',vv_slot_typ,vv_bstp_class),0) >= NVL(vn_max_slots,0) THEN

                  --Evaluate number of allowed slots by type (for total of slots) for SUPERs
                 ELSIF  NVL(vv_bstp_class,' ') IN (Pkg_Book_Datadict.f_bvtc_super) THEN

                    IF NVL(Sf_Get_Vessel_By_Dir(9,vn_bd_seq,'',vv_slot_typ,vv_bstp_class),0) >= NVL(vn_max_slots,0) THEN

                       RAISE e_large_cond;

                    END IF;--IF NVL(Sf_Get_Vessel_By_Dir(9,vn_bd_seq,'',vv_slot_typ,vv_bstp_class),0) >= NVL(vn_max_slots,0) THEN

                 END IF; --IF NVL(vv_bstp_class,' ') IN  (PKG_BOOK_DATADICT.f_bvtc_bestoff, PKG_BOOK_DATADICT.f_bvtc_pax) THEN

                 -- R.4484 Cambio de programacion por el llamado a un nuevo procedimiento que encapsula los cambios solicitados en R.4374
                 -- S59588/S61519: Changed on January 08, 2007 -- GNV
                 -- Include slot type classification parameter VV_BSTP_CLASS
                 --Pkg_Booking.P_MESSAGE_PARAM(vd_bk_rqst_date,vd_rqst_date, vn_cond, vv_slot_typ, vn_bd_seq, p_hml, p_rest, p_trn_dir, vv_message);

                 Pkg_Booking.P_MESSAGE_PARAM(vd_bk_rqst_date,vd_rqst_date, vn_cond, vv_slot_typ, vn_bd_seq, p_hml, p_rest, p_trn_dir, vv_bstp_class,vv_message);

              --S59588/S61519: Changed on January 08, 2007 -- GNV
              --ELSIF vv_slot_typ = 'SMALL' THEN
              ELSIF vv_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_small THEN

                 --
                 -- For validate the number of vessel allowed by the actual condition
                 --S59588/S61519: Changed on January 08, 2007 -- GNV
                 --IF NVL(Sf_Get_Vessel_By_Dir(3,vn_bd_seq,'',vv_slot_typ),0) >= NVL(vn_small_vess,0) THEN

                 IF ( NVL(vv_bstp_class,' ') IN  (PKG_BOOK_DATADICT.f_bvtc_pax)                                      AND
                      NVL(Sf_Get_Vessel_By_Dir(6,vn_bd_seq,'',vv_slot_typ,vv_bstp_class),0) >= NVL(vn_max_slots,0) ) OR
                    ( NVL(vv_bstp_class,' ') IN  (PKG_BOOK_DATADICT.f_bvtc_reg)                                      AND
                      NVL(Sf_Get_Vessel_By_Dir(9,vn_bd_seq,'',vv_slot_typ,vv_bstp_class),0) >= NVL(vn_max_slots,0) )
                 THEN

                    IF vv_predef_xpiece IS NOT NULL AND
                       NVL(vv_bstp_class,' ') = PKG_BOOK_DATADICT.f_bvtc_reg
                    THEN

                       vv_book_xpiece := 'Y';

                    ELSE

                       RAISE e_small_cond;

                    END IF;--IF vv_predef_xpiece IS NOT NULL THEN

                 END IF;--IF ( NVL(vv_bstp_class,' ') IN  (PKG_BOOK_DATADICT.f_bvtc_pax)                                      AND

                 IF vv_book_xpiece = 'N' THEN

                    -- R.4484 Cambio de programacion por llamado al procedimiento
                    -- S59588/S61519: Changed on January 08, 2007 -- GNV
                    -- Include slot type classification parameter VV_BSTP_CLASS
                    --Pkg_Booking.P_MESSAGE_PARAM(vd_bk_rqst_date,vd_rqst_date, vn_cond, vv_slot_typ, vn_bd_seq, p_hml, p_rest, P_TRN_DIR, vv_message);

                    Pkg_Booking.P_MESSAGE_PARAM(vd_bk_rqst_date,vd_rqst_date, vn_cond, vv_slot_typ, vn_bd_seq, p_hml, p_rest, p_trn_dir, vv_bstp_class, vv_message);

                    IF vv_message       IS NOT NULL AND
                       vv_predef_xpiece IS NOT NULL AND
                       NVL(vv_bstp_class,' ') = PKG_BOOK_DATADICT.f_bvtc_reg
                    THEN

                       vv_book_xpiece := 'Y';

                    END IF;--IF vv_message       IS NOT NULL AND

                 END IF;--IF vv_book_xpiece = 'N' THEN

                 --Reservación de buque SMALL/REGULAR a X-PIECE
                 BEGIN

                    IF vv_book_xpiece = 'Y' THEN

                       vn_per            := NULL;
                       vn_per_date_from  := NULL;
                       vn_per_date_to    := NULL;
                       vn_bsps_seq       := NULL;
                       vn_bps_seq        := NULL;
                       vn_bstp_seq       := NULL;
                       vv_bstp_typ       := NULL;
                       vv_bstp_class     := NULL;

                       --Data for Booking of SMALL/X-PIECE
                       --If Slots for SMALL/REGULAR are not available
                       --try to book as an SMALL/X-PIECE
                       FOR x IN C_Valid_Periods(vd_bk_rqst_date,
                                                vd_rqst_date,
                                                vv_slot_typ)
                       LOOP

                          --Set the valid normal periods only if Valid Periods
                          --are not Best Offer and XPiece
                          IF x.class = PKG_BOOK_DATADICT.f_bvtc_xpiece THEN

                             vn_per            := x.bp_id;
                             vn_per_date_from  := x.start_day;
                             vn_per_date_to    := x.end_day;
                             vn_bsps_seq       := x.bsps_seq;
                             vn_bps_seq        := x.bps_seq;
                             vn_bstp_seq       := x.bstp_seq;
                             vv_bstp_typ       := x.typ;
                             vv_bstp_class     := x.class;

                          END IF;--IF x.class = PKG_BOOK_DATADICT.f_bvtc_xpiece THEN

                       END LOOP;--FOR x IN C_Valid_Periods(vd_bk_rqst_date,

                       IF vn_per IS NULL THEN

                          RAISE e_period_closed;

                       END IF;--IF vn_per IS NULL THEN

                       --Get the maximum number of slots
                       OPEN c_num_cond(vn_bd_seq,
                                       vd_rqst_date,
                                       vv_slot_typ,
                                       vv_bstp_class);

                       IF c_num_cond%ISOPEN THEN

                          FETCH c_num_cond INTO vn_cond, vn_max_slots;

                          IF c_num_cond%NOTFOUND THEN

                             vn_cond       := NULL;
                             vn_max_slots  := NULL;

                          END IF; --IF c_num_cond%NOTFOUND THEN

                          CLOSE c_num_cond;

                       END IF;--IF c_num_cond%ISOPEN THEN

                       --Validate conditions for the maximum number of slots for X-PIECE vessels
                       IF NVL(Sf_Get_Vessel_By_Dir(6,vn_bd_seq,'',vv_slot_typ,vv_bstp_class),0) >= NVL(vn_max_slots,0) THEN

                          RAISE e_small_cond;

                       END IF;--IF NVL(Sf_Get_Vessel_By_Dir(6,vn_bd_seq,'',vv_slot_typ,vv_bstp_class),0) >= NVL(vn_max_slots_xpiece,0) THEN

                       --Validate restrictions by directions for X-PIECE
                       Pkg_Booking.P_MESSAGE_PARAM(vd_bk_rqst_date,vd_rqst_date, vn_cond, vv_slot_typ, vn_bd_seq, p_hml, p_rest, p_trn_dir, vv_bstp_class,vv_message);

                    END IF; --Fin de Reservación X-PIECE

                 END;

              END IF;--IF vv_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large THEN

              --S22195: Cambio de programacion para envio de exception
              IF vv_message IS NOT NULL THEN

                 vv_stat      := 'REJ';
                 vd_stat_date := Sf_Get_Sys_Date;
                 -- S9127
                 vv_bp_id     := vn_per;
                 vv_typ       := vv_slot_typ;

                 IF vv_message = 'e_max_large_cond' THEN

                    RAISE e_max_large_cond;

                 ELSIF vv_message = 'e_max_small_cond' THEN

                    RAISE e_max_small_cond;

                 ELSIF vv_message = 'e_small_dir' THEN

                    RAISE e_small_dir;

                 ELSIF vv_message = 'e_large_dir' THEN

                    RAISE e_large_dir;

                 ELSIF vv_message = 'e_large_dir_rest' THEN

                    RAISE e_large_dir_rest;

                 ELSIF vv_message = 'e_small_dir_rest' THEN

                    RAISE e_small_dir_rest;

                 END IF;--IF vv_message = 'e_max_large_cond' THEN

              END IF;--IF vv_message IS NOT NULL THEN
            --
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                NULL;
            END;
        END IF;    --avoid the evaluation of the itegrated barge units
        --
        BEGIN
          -- IF p_dtu_seq IS NULL THEN --:br10.INTEGR_BOOK IS NULL THEN --For avoid the evaluation of the itegrated barge units
          -- R3746: Changed on January 09, 2004 -- GNV
          -- para procesar reservaciones de barge integrated units
          -- IF p_dtu_int = 'N' THEN
          --
          IF (NVL(p_dtu_int,' ') = 'N'        AND  -- Naves Normales
              p_dtu_seq IS NULL)               OR
             (p_dtu_seq IS NOT NULL           AND  -- Barge Integrated
              NVL(p_dtu_int,' ') = 'Y'        AND
              NVL(p_vsl_type,' ') IN ('13','14','15','23','24','25','26')) THEN


              --S59588/S61519: Desactivated on January 08, 2007 -- GNV
              --               Now It is the same code for p_no_frft_cd IS NULL AND NOT NULL
              --IF  p_no_frft_cd IS NULL THEN       --Adicionado para R.4887
                  --
                  ----S59588/S61519: Changed on January 10, 2007 -- GNV
                  --IF Sf_Exist_Slot(vn_per,vn_bd_seq,vv_slot_typ,vd_rqst_date) THEN

                  IF PKG_BOOKING.f_exist_slot(vn_per,vn_bd_seq,vv_slot_typ,vd_rqst_date,vv_bstp_class) THEN
                     --
                     vv_bp_id     := vn_per;
                     vv_typ       := vv_slot_typ;
                     vv_stat      := 'BKD';
                     vd_stat_date :=TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
                     vv_stat_rmk  := 'Vessel Booked';

                     --
                  ELSE

                     --S59588/S61519: Desactivated on January 08, 2007 -- GNV
                     --These rules are not longer valid from 01-FEB-2008
                     /*
                     --
                     -- For commercial passenger vessel allocations.
                     vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY');
                     -- R.4591
                     vd_pass_date_to   := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY');
                     --
                     SELECT TO_NUMBER(TO_CHAR(TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY'),'MM')) -
                            TO_NUMBER(TO_CHAR(TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY'),'MM'))
                     INTO vn_between_para_month
                     FROM dual;
                     --
                     IF vn_between_para_month >= 0 THEN
                        --
                        /*R.4591 Evaluacion de fecha de reservacion vs periodo de gracia para cruceros
                        -- PERIODO DE EVALUACION: inicia en el año en curso y termina el mismo año
                        -- Fecha de reservacion inferior al periodo a terminarse en el año en curso
                        -- Fecha de reservacion entre periodo del año en curso
                        -- Fecha de reservacion superior al periodo en curso
                        */

                        /*
                        IF     TRUNC(vd_bk_rqst_date) < TRUNC(vd_pass_date_to) THEN
                             --
                               vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY');
                               vd_pass_date_to   := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY');
                               --
                        ELSIF  TRUNC(vd_bk_rqst_date) < TRUNC(vd_pass_date_to) + 365 THEN
                               --
                               vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE + 365,'YYYY'),'DD-MON-YYYY');
                               vd_pass_date_to   := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE + 365,'YYYY'),'DD-MON-YYYY');
                               --
                        ELSE
                               --
                               vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE + 730,'YYYY'),'DD-MON-YYYY');
                               vd_pass_date_to   := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE + 730,'YYYY'),'DD-MON-YYYY');
                               --
                        END IF;
                        */
                        --
                     /*
                     ELSE

                        --
			            /*R.4591 Evaluacion de fecha de reservacion vs periodo de gracia para cruceros
                        -- PERIODO DE EVALUACION: inicia en el año en curso y termina el siguiente año
                        -- Fecha de reservacion inferior al periodo a iniciarse en el año en curso eg. bk-date 01-sep-04 today 01-ago-04
                        -- Fecha de reservacion entre periodo del año en curso pero inferior al siguiente periodo eg. bk-date 01-dic-04 today 01-abr-04
                        -- Fecha de reservacion superior al siguiente periodo eg. bk-date 01-dic-05 today 21-dic-04
                        */
                        --
                     /*
                        IF TRUNC(vd_bk_rqst_date) < TRUNC(vd_pass_date_from) THEN
                           --
                           vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE - 365,'YYYY'),'DD-MON-YYYY');
                           --
                        ELSIF  TRUNC(vd_bk_rqst_date) < TRUNC(vd_pass_date_from) + 365 THEN
                           --
                           vd_pass_date_to   := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE + 365,'YYYY'),'DD-MON-YYYY');
                           --
                        ELSE
                           --
                           vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE + 365,'YYYY'),'DD-MON-YYYY');
                           vd_pass_date_to := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE + 730,'YYYY'),'DD-MON-YYYY');
                           --
                        END IF;
                        --
			         END IF;
			         */
                     --
                     /*
                     IF vn_per = 1                                                       AND
                        vv_vessel_type = '11'                                            AND
                        Sf_Count_Passg_Vess(vn_bd_seq) < Sf_Get_Param_Val(50,'BOOKING')  AND
                        TRUNC(vd_bk_rqst_date) BETWEEN TRUNC(vd_pass_date_from)
                                                   AND TRUNC(vd_pass_date_to)            AND
                        TRUNC(vd_bk_rqst_date)-TRUNC(vd_rqst_date) >= (vn_per_date_to - Sf_Get_Param_Val(15,'BOOKING')) THEN --R.4591
                        --
                        IF Sf_Exist_Slot(vn_per,vn_bd_seq,'LARGE',vd_rqst_date) AND
                           vv_slot_typ = 'SMALL'                                THEN
                           --
                           vv_bp_id         := vn_per;
                           vv_typ           := 'SMALL';
                           vv_spc_slot_assg := 'Y';
                           vv_stat          := 'BKD';
                           vd_stat_date     := TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
                           vv_stat_rmk      :=  'Booked in large slot. Commercial Passenger Vessel';
                           --
                        ELSIF Sf_Exist_Slot(vn_per,vn_bd_seq,'SMALL',vd_rqst_date) AND
                           vv_slot_typ = 'LARGE'                                   THEN
                           --
                           vv_bp_id         := vn_per;
					       vv_typ           := 'LARGE';
					       vv_spc_slot_assg := 'Y';
					       vv_stat          := 'BKD';
					       vd_stat_date     := TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
					       vv_stat_rmk      := 'Booked in small slot. Commercial passenger vessel';
                           --
                        ELSE
                           RAISE e_null_slots;
                        END IF;
                     ELSE
                        RAISE e_null_slots;
                     END IF;
                     */

                     --S59588/S61519: Added on January 11, 2007 -- GNV
                     --Raise no Slots Avalailable
                     --For SMALL/REGULARS with LOA <= 300 try to book
                     --for X-PIECE slots
                     IF vv_predef_xpiece IS NOT NULL                           AND
                        NVL(vv_bstp_class,' ') = PKG_BOOK_DATADICT.f_bvtc_reg
                     THEN

                        IF vv_book_xpiece = 'N' THEN

                           vv_book_xpiece := 'Y';

                        END IF;--IF vv_book_xpiece = 'N' THEN

                     END IF;--IF vv_predef_xpiece IS NOT NULL THEN

                     IF vv_book_xpiece = 'N' THEN

                        RAISE e_null_slots;

                     ELSE

                        --Reservación de X-PIECE
                        vn_per            := NULL;
                        vn_per_date_from  := NULL;
                        vn_per_date_to    := NULL;
                        vn_bsps_seq       := NULL;
                        vn_bps_seq        := NULL;
                        vn_bstp_seq       := NULL;
                        vv_bstp_typ       := NULL;
                        vv_bstp_class     := NULL;

                        --Data for Booking of SMALL/X-PIECE
                        --If Slots for SMALL/REGULAR are not available
                        --try to book as an SMALL/X-PIECE
                        FOR x IN C_Valid_Periods(vd_bk_rqst_date,
                                                 vd_rqst_date,
                                                 vv_slot_typ)
                        LOOP

                           --Set the valid normal periods only if Valid Periods
                           --are not Best Offer and XPiece
                           IF x.class = PKG_BOOK_DATADICT.f_bvtc_xpiece THEN

                              vn_per            := x.bp_id;
                              vn_per_date_from  := x.start_day;
                              vn_per_date_to    := x.end_day;
                              vn_bsps_seq       := x.bsps_seq;
                              vn_bps_seq        := x.bps_seq;
                              vn_bstp_seq       := x.bstp_seq;
                              vv_bstp_typ       := x.typ;
                              vv_bstp_class     := x.class;

                           END IF;--IF x.class = PKG_BOOK_DATADICT.f_bvtc_xpiece THEN

                        END LOOP;--FOR x IN C_Valid_Periods(vd_bk_rqst_date,

                        IF vn_per IS NULL THEN

                           RAISE e_period_closed;

                        END IF;--IF vn_per IS NULL THEN

                        --Get the maximum number of slots
                        OPEN c_num_cond(vn_bd_seq,
                                        vd_rqst_date,
                                        vv_slot_typ,
                                        vv_bstp_class);

                        IF c_num_cond%ISOPEN THEN

                           FETCH c_num_cond INTO vn_cond, vn_max_slots;

                           IF c_num_cond%NOTFOUND THEN

                              vn_cond       := NULL;
                              vn_max_slots  := NULL;

                           END IF; --IF c_num_cond%NOTFOUND THEN

                           CLOSE c_num_cond;

                        END IF;--IF c_num_cond%ISOPEN THEN

                        --Validate conditions for the maximum number of slots for X-PIECE vessels
                        IF NVL(Sf_Get_Vessel_By_Dir(6,vn_bd_seq,'',vv_slot_typ,vv_bstp_class),0) >= NVL(vn_max_slots,0) THEN

                           RAISE e_small_cond;

                        END IF;--IF NVL(Sf_Get_Vessel_By_Dir(6,vn_bd_seq,'',vv_slot_typ,vv_bstp_class),0) >= NVL(vn_max_slots_xpiece,0) THEN

                        --Validate restrictions by directions for X-PIECE
                        Pkg_Booking.P_MESSAGE_PARAM(vd_bk_rqst_date,vd_rqst_date, vn_cond, vv_slot_typ, vn_bd_seq, p_hml, p_rest, p_trn_dir, vv_bstp_class,vv_message);

                        IF vv_message IS NOT NULL THEN

                           vv_stat      := 'REJ';
                           vd_stat_date := Sf_Get_Sys_Date;
                           -- S9127
                           vv_bp_id     := vn_per;
                           vv_typ       := vv_slot_typ;

                           IF vv_message = 'e_max_large_cond' THEN

                              RAISE e_max_large_cond;

                           ELSIF vv_message = 'e_max_small_cond' THEN

                              RAISE e_max_small_cond;

                           ELSIF vv_message = 'e_small_dir' THEN

                              RAISE e_small_dir;

                           ELSIF vv_message = 'e_large_dir' THEN

                              RAISE e_large_dir;

                           ELSIF vv_message = 'e_large_dir_rest' THEN

                              RAISE e_large_dir_rest;

                           ELSIF vv_message = 'e_small_dir_rest' THEN

                              RAISE e_small_dir_rest;

                           END IF;--IF vv_message = 'e_max_large_cond' THEN

                        END IF;--IF vv_message IS NOT NULL THEN

                        --Get slots availables for X-PIECE
                        IF PKG_BOOKING.f_exist_slot(vn_per,vn_bd_seq,vv_slot_typ,vd_rqst_date,vv_bstp_class) THEN
                           --
                           vv_bp_id     := vn_per;
                           vv_typ       := vv_slot_typ;
                           vv_stat      := 'BKD';
                           vd_stat_date :=TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
                           vv_stat_rmk  := 'Vessel Booked';

                        --If they are not Slots Availables for X-PIECE
                        --raise the exception
                        ELSE

                           RAISE e_null_slots;

                        END IF;--IF PKG_BOOKING.f_exist_slot(vn_per,vn_bd_seq,vv_slot_typ,vd_rqst_date,vv_bstp_class) THEN

                     END IF; --IF vv_book_xpiece = 'N' THEN

                  END IF;--IF Sf_Exist_Slot(vn_per,vn_bd_seq,vv_slot_typ,vd_rqst_date) THEN
                  --
                  --
              --S59588/S61519: Desactivated on January 08, 2007 -- GNV
              --               Now It is the same code for p_no_frft_cd IS NULL AND NOT NULL
              /*
              ELSE -- p_no_frft_cd.  Adicionado para R.4887
                  --
                  IF Sf_Exist_Slot(vn_per,vn_bd_seq,vv_slot_typ,vd_rqst_date) THEN
                     --
                     vv_bp_id     := vn_per;
                     vv_typ       := vv_slot_typ;
                     vv_stat      := 'BKD';
                     vd_stat_date := TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
                     vv_stat_rmk  := 'Vessel Booked';
                     --
                  END IF;
                  --
              END IF;
              */

          END IF; -- For avoid the evaluation of the integrated barge units

        EXCEPTION

          WHEN NO_DATA_FOUND THEN
	        vv_stat     := 'REJ';
	        vv_stat_rmk := 'No slots available';
            --
            -- S9127
            vv_bp_id     := vn_per;
            vv_typ       := vv_slot_typ;
            --
            vd_stat_date := Sf_Get_Sys_Date;

        END;

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          --
          vv_stat      := 'REJ';
          vd_stat_date := Sf_Get_Sys_Date;
          vv_stat_rmk  := 'Period is closed for the request date';
          vn_bd_seq    := NULL;
          vv_bp_id     := NULL;
          vv_typ       := NULL;
          --
        WHEN e_null_slots THEN
          --
          vv_stat      := 'REJ';
          vd_stat_date := Sf_Get_Sys_Date;
          vv_stat_rmk  := 'No slots available';
          --
          -- S9127
          vv_bp_id     := vn_per;
          vv_typ       := vv_slot_typ;
          --
        WHEN e_small_cond THEN
          --
          vv_stat      := 'REJ';
          vd_stat_date := Sf_Get_Sys_Date;
          vv_stat_rmk  := 'No. small vessels exceeded for the condition'||vn_cond;
          --
          -- S9127
          vv_bp_id     := vn_per;
          vv_typ       := vv_slot_typ;
          --
        WHEN e_large_cond THEN
          --
          vv_stat      := 'REJ';
          vd_stat_date := Sf_Get_Sys_Date;
          vv_stat_rmk  := 'No. large vessels exceeded for the condition'||vn_cond;
          --
          -- S9127
          vv_bp_id     := vn_per;
          vv_typ       := vv_slot_typ;
        --
        --S22195: Cambio de posicion de excepciones
        WHEN e_max_large_cond THEN

              vv_stat_rmk  := 'Maximum restricted large vessels exceeded for the condition'||vn_cond;

         WHEN e_max_small_cond THEN

              vv_stat_rmk  := 'Maximum restricted small vessels exceeded for the condition'||vn_cond;

         WHEN e_small_dir THEN

              vv_stat_rmk  := 'Number of small vessels exceeded in same direction';

         WHEN e_large_dir THEN

              vv_stat_rmk  := 'Number of large vessels exceeded in same direction';

         WHEN e_large_dir_rest THEN

              vv_stat_rmk  := 'No. large vessels w/rest. exceeded in same dir.';

         WHEN e_small_dir_rest THEN

              vv_stat_rmk  := 'No. small vessels w/rest. exceeded in same dir.';

         --S59588/S61519: Added on January 10, 2007 -- GNV
         WHEN e_period_closed THEN

              vv_stat      := 'REJ';
              vd_stat_date := Sf_Get_Sys_Date;
              vv_stat_rmk  := 'Period is closed for the request date';
              vn_bd_seq    := NULL;
              vv_bp_id     := NULL;
              vv_typ       := NULL;

      END; -- FIN de la evaluacion del booking
      --

   --ELSIF NVL(P_FIX_AMT,0) > 0 THEN -- S4332
   --S59588/S61519: Changed on January 09, 2007 -- GNV
   ELSIF NVL(P_AMOUNT,0) > 0 THEN
      --
      /*S59588/S61519: Changed on January 09, 2007 -- GNV
      BEGIN
        SELECT id
        INTO   vv_bp_id
        FROM   BK_PERS
        WHERE  TRUNC(vd_bk_rqst_date)-TRUNC(vd_rqst_date) BETWEEN date_from AND date_to;
      EXCEPTION
        WHEN OTHERS THEN
          vv_bp_id := NULL;
      END;
      */

      /*S59588/S61519: Changed on January 09, 2007 -- GNV
                       Used with SF_GET_BK_TYP
      IF vn_beam >= Sf_Get_Param_Val(56,'BOOKING') THEN
         vv_typ :='LARGE';
      ELSE
         vv_typ :='SMALL';
      END IF;
      */

      IF vv_slot_typ =  PKG_BOOK_DATADICT.f_bk_typ_large THEN

         vv_bstp_class := PKG_BOOK_DATADICT.f_bvtc_super;

      ELSIF vv_slot_typ =  PKG_BOOK_DATADICT.f_bk_typ_small THEN

         vv_bstp_class := PKG_BOOK_DATADICT.f_bvtc_reg;

      END IF;--IF vv_typ =  PKG_BOOK_DATADICT.f_bk_typ_large THEN

      --Get the valid Period
      FOR x IN C_Valid_Periods(vd_bk_rqst_date,
                               vd_rqst_date,
                               vv_slot_typ)
      LOOP

         --Set the valid normal periods only if Valid Periods
         --are not Best Offer and XPiece
         IF x.class NOT IN (PKG_BOOK_DATADICT.f_bvtc_bestoff,
                            PKG_BOOK_DATADICT.f_bvtc_xpiece,
                            PKG_BOOK_DATADICT.f_bvtc_pax)
         THEN

             vv_bp_id          := x.bp_id;
             vn_per_date_from  := x.start_day;
             vn_per_date_to    := x.end_day;
             vn_bsps_seq       := x.bsps_seq;
             vn_bps_seq        := x.bps_seq;
             vn_bstp_seq       := x.bstp_seq;
             vv_bstp_typ       := x.typ;
             vv_bstp_class     := x.class;

         END IF; --IF x.class NOT IN (PKG_BOOK_DATADICT.f_bvtc_bo,

      END LOOP;--FOR x IN C_Valid_Periods(vd_bk_rqst_date,

      BEGIN

        vv_stat := 'BKD';
        vv_typ  := vv_slot_typ;
        vd_stat_date :=TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
        vv_stat_rmk := 'Vessel Booked';

      END;

   END IF; -- S4332 FIN DE BUSQUEDA DE INFORMACION PARA SUBASTA
   --

   IF p_extm_beam >= Sf_Get_Param_Val(56,'BOOKING') THEN

     -- IF p_hml IN ('M','D', 'N','O') THEN
      IF INSTR(PKG_BOOK_DATADICT.f_hml_rest_list, p_hml) > 0 THEN

         vv_rest_when_bk :='Y';

      ELSE

         vv_rest_when_bk :='N';

      END IF;--IF p_hml IN ('M','D', 'N','O') THEN

   ELSE

      IF p_rest IS NOT NULL THEN

         vv_rest_when_bk :='Y';

      ELSE

         vv_rest_when_bk :='N';

      END IF;--IF p_rest IS NOT NULL THEN

   END IF;--IF p_extm_beam >= Sf_Get_Param_Val(56,'BOOKING') THEN
   --
   SELECT br_seq.NEXTVAL
   INTO   vn_seq
   FROM   dual;
   --

   --Assign special variable for fix amount
   IF P_SPEC_BOOK_IND = PKG_BOOK_DATADICT.f_bso_au THEN

      vn_fix_amount := p_amount;

   END IF;--IF P_SPEC_BOOK_IND = PKG_BOOK_DATADICT.f_bso_au THEN

   --
   IF p_dtu_seq IS NOT NULL        AND  -- Tug Integrated Booking Process
      NVL(p_dtu_int,' ') = 'Y'     AND
      NVL(p_vsl_type,' ') = '18'   THEN

      BEGIN
        SELECT bk.typ,
               bk.rest_when_bk,
               bk.bp_id,
               bk.bd_seq,
               bk.stat_date,
               bk.bsps_seq
        INTO   vv_barge_typ,
               vv_barge_rest_when_bk,
               vn_barge_bp_id,
               vn_barge_bd_seq,
               vd_barge_stat_date,
               vn_barge_bsps_seq
        FROM   CUST_SCHED_NEEDS cust,
               ITIN_ITEMS       iti,
               BK_RQSTS         bk,
               SHIP_ID_NO       SIN
        WHERE cust.seq             = iti.csn_seq
        AND iti.bs_seq             = bk.bs_seq
        AND iti.seq                = Sf_Get_Iti_Book(bk.bs_seq)
        AND SIN.seq                = cust.sin_seq
        AND SIN.ship_no            = p_barge_no
        AND TRUNC(bk.rqst_bk_date) = TRUNC(vd_bk_rqst_date)
        AND bk.stat                = 'BKD';
      EXCEPTION
        WHEN OTHERS THEN
          RAISE e_no_barge_info;
      END;
      --
      INSERT INTO vw_booking_rqsts(  BR_SEQ
                                   , SRCE
                                   , STAT
                                   , STAT_RMK
                                   , BS_SEQ
                                   , RULE_APPLY_IND
                                   , CRRA_SEQ
                                   , RQST_DATE
                                   , STAT_DATE
                                   , REST_WHEN_BK
                                   , RQST_BK_DATE
                                   , BD_SEQ
                                   , BP_ID
                                   , TYP
                                   , SPC_SLOT_ASSG
                                   , PENALTY_IND
                                   , DTU_SEQ
                                   , FIX_AMT    -- S4332
                                   , BSPS_SEQ ) -- S59588/S61519
      VALUES(  vn_seq
             , vv_srce
             , 'BKD'
             , 'Vessel Booked integrated w/Barge SIN='||TO_CHAR(p_barge_no)
             , vn_bs_seq
             , vv_rule_apply_ind
             , vn_crra_seq
             --
             -- Comment by JaDiaz on September 23, 2008 S83474
             -- , vd_rqst_date
             , vd_org_rqst_date
             --
             , vd_barge_stat_date
             , vv_barge_rest_when_bk
             , vd_bk_rqst_date
             , vn_barge_bd_seq
             , vn_barge_bp_id
             , vv_barge_typ
             , vv_spc_slot_assg
             , 'N'
             , p_dtu_seq
             --S59588/S61519: Changed on January 11, 2007 -- GNV
             --,p_fix_amt      -- S4332
             , vn_fix_amount
             , vn_barge_bsps_seq ); -- S59588/S61519
      --
      IF SQL%FOUND THEN

         p_br_seq  := vn_seq;
         p_success := 'TRUE';
         p_bk_stat := 'BKD';
         p_bk_cond := vn_cond;

      END IF;
      --
   ELSE
      --
      INSERT INTO vw_booking_rqsts(  BR_SEQ
                                   , SRCE
                                   , STAT
                                   , STAT_RMK
                                   , BS_SEQ
                                   , RULE_APPLY_IND
                                   , CRRA_SEQ
                                   , RQST_DATE
                                   , STAT_DATE
                                   , REST_WHEN_BK
                                   , RQST_BK_DATE
                                   , BD_SEQ
                                   , BP_ID
                                   , TYP
                                   , SPC_SLOT_ASSG
                                   , PENALTY_IND
                                   , DTU_SEQ
                                   , FIX_AMT -- S4332
                                   , BSPS_SEQ ) -- S59588/S61519
      VALUES(  vn_seq
             , vv_srce
             , vv_stat
             , vv_stat_rmk
             , vn_bs_seq
             , vv_rule_apply_ind
             , vn_crra_seq
             --
             -- Comment by JaDiaz on September 23, 2008 S83474
             -- , vd_rqst_date
             , vd_org_rqst_date
             --
             , vd_stat_date
             , vv_rest_when_bk
             , vd_bk_rqst_date
             , vn_bd_seq
             , vv_bp_id
             , vv_typ
             , vv_spc_slot_assg
             ,'N'
             , p_dtu_seq
             --S59588/S61519: Changed on January 11, 2007 -- GNV
             --,p_fix_amt      -- S4332
             , vn_fix_amount
             , vn_bsps_seq ); -- S59588/S61519
      --
      IF SQL%FOUND THEN

         --S59588/S61519: Added on January 09, 2007 -- GNV
         --               Insert Best Offer Amount if indicator
         --               is active and the booking request
         --               record has been inserted.
         IF p_spec_book_ind = PKG_BOOK_DATADICT.f_bso_bo THEN

            SELECT brd_seq.NEXTVAL
              INTO vn_brd_seq
              FROM dual;

            INSERT INTO vw_bk_rqst_dets( seq
                                       , br_seq
                                       , bs_seq
                                       , trns_type
                                       , bs_seq_dest
                                       , br_seq_dest
                                       , fix_amt
                                       , amount
                                       , deleted)
            VALUES(  vn_brd_seq
                   , vn_seq
                   , vn_bs_seq
                   , PKG_BOOK_DATADICT.f_bkp_bofr
                   , NULL
                   , NULL
                   , NULL
                   , p_amount
                   , 'N');

            IF SQL%FOUND THEN

               vv_ins_bof_amount := 'Y';

            END IF;--IF SQL%FOUND THEN

         END IF;--IF p_spec_book_ind = PKG_BOOK_DATADICT.f_bso_bo THEN

         IF (NVL(p_spec_book_ind,' ') = PKG_BOOK_DATADICT.f_bso_bo AND vv_ins_bof_amount = 'Y') OR
            (NVL(p_spec_book_ind,' ') <> PKG_BOOK_DATADICT.f_bso_bo)
         THEN

            p_br_seq  := vn_seq;
            p_success := 'TRUE';
            p_bk_stat := vv_stat;
            p_bk_cond := vn_cond;

         END IF;--IF (p_spec_book_ind = PKG_BOOK_DATADICT.f_bso_bo AND vv_ins_bof_amount = 'Y') OR

      END IF;--IF SQL%FOUND THEN

      --
   END IF;--IF p_dtu_seq IS NOT NULL        AND

--S59588/S61519: Added on January 09, 2007 -- GNV
--Added call to procedure that includes the old version
--of booking process
ELSE

   PKG_BOOKING.p_process_bk_bef_v1(p_vsl_type
                                  ,p_csn_seq
                                  ,p_bs_seq
                                  ,p_iti_seq
                                  ,p_cust_cd
                                  ,p_real_cust_cd
                                  ,p_date_created
                                  ,p_bk_date
                                  ,p_trn_dir
                                  ,p_iti_trn_dir
                                  ,p_extm_beam
                                  ,p_beam
                                  ,p_hml
                                  ,p_rest
                                  ,p_gas_free_ind
                                  ,p_pde_cd
                                  ,p_dtu_int
                                  ,p_agent
                                  ,p_dtu_seq
                                  ,p_tug_no
                                  ,p_barge_no
                                  ,vn_br_seq
                                  ,vv_br_stat
                                  ,vn_bk_cond
                                  ,vv_error_code
                                  ,vv_success
                                  ,p_no_frft_cd
                                  ,p_loa
                                  ,p_spec_book_ind
                                  ,p_amount);

   P_BR_SEQ     := vn_br_seq;
   P_BK_STAT    := vv_br_stat;
   P_BK_COND    := vn_bk_cond;
   P_ERROR_CODE := vv_error_code;
   P_SUCCESS    := vv_success;

END IF; --IF p_date_created >= PKG_BOOK_DATADICT.f_bk_proc_eff_date_v1 THEN

EXCEPTION
--
WHEN NO_DATA_FOUND THEN
  --
  IF vd_bk_rqst_date IS NULL THEN
     --
     p_success    := 'FALSE';
     p_error_code := 'USR-00114';
     Pkg_Evtms_Db_Util.p_error('USR-00114','The request booking date is null');
     --
  ELSE
     --
     p_success    := 'FALSE';
     p_error_code := 'USR-00115';
     Pkg_Evtms_Db_Util.p_error('USR-00115','Not exist booking date in BK_DAYS table');
     --
  END IF;
  --
WHEN e_already_bk THEN
  --
  p_success    := 'FALSE';
  p_error_code := 'USR-30106';
  Pkg_Evtms_Db_Util.p_error('USR-30106','This vessel was already booked');
  --
WHEN e_for_compet THEN
  --
  p_success    := 'FALSE';
  p_error_code := 'USR-01866';
  Pkg_Evtms_Db_Util.p_error('USR-01866','Booking Competition Period has not finished yet. Please try later ...');
  --
WHEN e_invalid_dir THEN
  --
  p_success    := 'FALSE';
  p_error_code := 'USR-30108';
  Pkg_Evtms_Db_Util.p_error('USR-30108','Booking request with itinerary of different transit direction');
  --
WHEN e_invalid_bk_day THEN
  --
  p_success    := 'FALSE';
  p_error_code := 'USR-30109';
  Pkg_Evtms_Db_Util.p_error('USR-30109','There is not an open period for this booking date');
  --
WHEN e_invalid_cust_cd THEN
  --
  p_success    := 'FALSE';
  p_error_code := 'USR-30110';
  Pkg_Evtms_Db_Util.p_error('USR-30110','Differences in Customer Codes');
  --
WHEN e_invalid_beam THEN
  --
  p_success    := 'FALSE';
  p_error_code := 'USR-30111';
  Pkg_Evtms_Db_Util.p_error('USR-30111','Differences in Vessel Beams');
  --
WHEN e_gas_free_is_missing THEN
  --
  p_success    := 'FALSE';
  p_error_code := 'USR-30117';
  Pkg_Evtms_Db_Util.p_error('USR-30117','Gas Free can not be null for Tankers or Dry/Bulk Liquid Carrie');
  --
WHEN e_no_barge_info THEN
  --
  p_success    := 'FALSE';
  p_error_code := 'USR-30058';
  Pkg_Evtms_Db_Util.p_error('USR-30058','This tug cannot be booked because it was integrated to a barge. Need to book barge first');
  --
WHEN e_invalid_amount THEN
  --
  p_success    := 'FALSE';
  p_error_code := 'USR-30698';
  Pkg_Evtms_Db_Util.p_error('USR-30698','Invalid Amount for Auction or Best Offer Booking Requests');

WHEN e_invalid_beam_bo THEN
  --
  p_success    := 'FALSE';
  p_error_code := 'USR-30699';
  Pkg_Evtms_Db_Util.p_error('USR-30699','Best Offer Booking is only for SUPERS');

WHEN OTHERS THEN
  --
  p_success  := 'FALSE';
  vv_sqlerrm := SQLERRM;

  IF INSTR(vv_SQLERRM, 'USR-') > 0 THEN

     p_success    := 'FALSE';
     p_error_code := SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9);
     pkg_evtms_db_util.p_error(SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9),'Error');

  ELSE

     IF INSTR(vv_SQLERRM, 'USR-') = 0 THEN

        p_success := 'FALSE';
        p_error_code   := SQLCODE;
        pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

     ELSE

        p_success := 'FALSE';
        p_error_code   := SQLCODE;
        pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

     END IF;--IF INSTR(vv_SQLERRM, 'USR-') = 0 THEN

  END IF;--IF INSTR(vv_SQLERRM, 'USR-') > 0 THEN

  Pkg_Plog.sec_implementation ('PKG_BOOKING.P_PROCESS_BOOKING',
                                 SUBSTR(
                                   'P_Vsl_Type= '
                                 || p_vsl_type
                                 || CHR (10)
                                 ||'P_Csn_seq= '
                                 || TO_CHAR(p_csn_seq)
                                 || CHR (10)
                                 ||'p_bs_seq= '
                                 || TO_CHAR(p_bs_seq)
                                 || CHR (10)
                                 ||'p_iti_seq= '
                                 || TO_CHAR(p_iti_seq)
                                 || CHR (10)
                                 ||'p_cust_cd= '
                                 || p_cust_cd
                                 || CHR (10)
                                 ||'p_real_cust_cd= '
                                 || p_real_cust_cd
                                 || CHR (10)
                                 ||'p_date_created= '
                                 || TO_CHAR(p_date_created,'DD-MON-YYYY HH24MI')
                                 || CHR (10)
                                 ||'p_bk_date= '
                                 || TO_CHAR(p_bk_date,'DD-MON-YYYY HH24MI')
                                 || CHR (10)
								 ||'p_trn_dir= '
                                 || p_trn_dir
                                 || CHR (10)
								 ||'p_iti_trn_dir= '
                                 || p_iti_trn_dir
                                 || CHR (10)
								 ||'p_extm_beam= '
								 || TO_CHAR(p_extm_beam)
								 || CHR (10)
								 ||'p_beam= '
								 || TO_CHAR(p_beam)
								 || CHR (10)
								 ||'p_hml= '
								 || p_hml
								 || CHR (10)
								 ||'p_rest= '
								 || p_rest
								 || CHR (10)
								 ||'p_gas_free_ind= '
								 || p_gas_free_ind
								 || CHR (10)
								 ||'p_pde_cd= '
								 || p_pde_cd
								 || CHR (10)
								 ||'p_dtu_int= '
								 || p_dtu_int
								 || CHR (10)
								 ||'p_agent= '
								 || p_agent
								 || CHR (10)
								 ||'p_dtu_seq= '
								 || TO_CHAR(p_dtu_seq)
								 || CHR (10)
								 ||'p_tug_no= '
								 || TO_CHAR(p_tug_no)
								 || CHR (10)
								 ||'p_barge_no= '
								 || TO_CHAR(p_barge_no)
								 || CHR (10)
								 ||'p_br_seq= '
								 || TO_CHAR(p_br_seq)
								 || CHR (10)
								 ||'p_bk_stat= '
								 || p_bk_stat
								 || CHR (10)
								 ||'p_bk_cond= '
								 || TO_CHAR(p_bk_cond)
								 || CHR (10)
								 ||'p_error_code= '
								 || p_error_code
								 || CHR (10)
								 ||'p_success= '
								 || p_success
								 || CHR (10)
								 ||'p_no_frft_cd= '
								 || p_no_frft_cd
								 || CHR (10)
								 ||'p_loa= '
								 || TO_CHAR(p_loa)
								 || CHR (10)
								 ||'p_spec_book_ind= '
								 || p_spec_book_ind
								 || CHR (10)
								 ||'p_amount= '
								 || TO_CHAR(p_amount)
								 || CHR (10)
								 ||'SQLERRM= '
								 || SQLERRM,1,1950),
                                   'INFO');


END;
PROCEDURE P_PROCESS_BK_BEF_V1
 (P_VSL_TYPE IN SHIP_ID_NO.VTY_CD%TYPE
 ,P_CSN_SEQ IN CUST_SCHED_NEEDS.SEQ%TYPE
 ,P_BS_SEQ IN BK_RQSTS.BS_SEQ%TYPE
 ,P_ITI_SEQ IN ITIN_ITEMS.SEQ%TYPE
 ,P_CUST_CD IN CONTACTS.SHORT_NAME%TYPE
 ,P_REAL_CUST_CD IN CONTACTS.SHORT_NAME%TYPE
 ,P_DATE_CREATED IN BK_RQSTS.RQST_DATE%TYPE
 ,P_BK_DATE IN BK_RQSTS.RQST_BK_DATE%TYPE
 ,P_TRN_DIR IN ITIN_ITEMS.TII_TRN_DIR%TYPE
 ,P_ITI_TRN_DIR IN ITIN_ITEMS.TII_TRN_DIR%TYPE
 ,P_EXTM_BEAM IN CUST_VSL_CHARS.EXTM_BEAM%TYPE
 ,P_BEAM IN CUST_VSL_CHARS.EXTM_BEAM%TYPE
 ,P_HML IN CUST_VSL_CHARS.LVC_HML_QUALIFIED%TYPE
 ,P_REST IN REST_USR_CDS.CD%TYPE
 ,P_GAS_FREE_IND IN VARCHAR2
 ,P_PDE_CD IN ITIN_ITEMS.PDE_CD%TYPE
 ,P_DTU_INT IN WEB_BK_REQUESTS.DTU_INT%TYPE
 ,P_AGENT IN CONTACTS.SHORT_NAME%TYPE
 ,P_DTU_SEQ IN ITIN_ITEMS.DTU_GROUP_BY_SEQ%TYPE
 ,P_TUG_NO IN SHIP_ID_NO.SHIP_NO%TYPE
 ,P_BARGE_NO IN SHIP_ID_NO.SHIP_NO%TYPE
 ,P_BR_SEQ OUT BK_RQSTS.SEQ%TYPE
 ,P_BK_STAT OUT BK_RQSTS.STAT%TYPE
 ,P_BK_COND OUT BK_CONDS.LEV%TYPE
 ,P_ERROR_CODE OUT VARCHAR2
 ,P_SUCCESS OUT VARCHAR2
 ,P_NO_FRFT_CD IN BK_RQSTS.NO_FRFT_CD%TYPE := NULL
 ,P_LOA IN CUST_VSL_CHARS.LEN_OVERALL%TYPE := NULL
 ,P_SPEC_BOOK_IND IN VARCHAR2 := NULL
 ,P_AMOUNT IN BK_RQSTS.FIX_AMT%TYPE := NULL
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de procesar booking requests.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador         Fecha         Cambios realizados
-- ----------- --------------------  ----------    -----------------------------------------
-- R3430       GVillarreal           21-oct-2003   Version Inicial transportada desde el
--                                                 procedimiento P_PROCESS_BOOKING
--                                                 de la pantalla VI5060FM (Process
--                                                 Booking  Request
--                                   11-nov-2003   Desactivar cálculo de booking fee, period, arrival
--                                                 time para calcularlo en forma externa en el
--                                                 procedimiento P_RTV_BOOK_FEE
--                                   12-nov-2003   Inclusión de llamado de procedimiento para
--                                                 manejo de errores
--                                   24-nov-2003   Inclusión de uso de vistas para booking requests
--                                                 como parte del proceso de booking se requiere
--                                                 regresar la información de pc_ums
--                                   11-dec-2003   Inclusión de parámetros de salida P_BR_SEQ ,
--                                                 P_BK_STAT  y  P_BK_COND
--                                   30-dec-2003   Cambio de status rmk a comentarios de
--                                                 varchar2(50) temporalmente mientras se arregla
--                                                 el tamaño en la tabla WEB_BK_REQUESTS y
--                                                 WEB_BK_RQST_TRANS
--                                   05-jan-2004   Cambios a comentarios normales de status con
--                                                 largo de 250 caracteres
--                                   07-jan-2004   Inclusión de cambio de fecha para procesamiento
--                                                 de año y evaluacion de periodo de reservacion
--                                                 para buques de pasajeros
--                                   09-jan-2004   Inclusión de cambios con respecto al manejo de
--                                                 reservaciones para DTUs
--                                   06-feb-2004   Cambios a comentarios de REJ (stat_rmk) de
--                                                 250 caracteres a 50 caracteres para EDCS Web
--                                   17-feb-2004   Desactivar validación de gas free indicator y beam
--                                   16-mar-2004   Cambiar validación de booking date contra 2 días
--                                                 antes en lugar de system date es request date
-- R4104       GVillarreal           18-may-2004   Cambiar validación de hora de competencia para
--                                                 incluir parametros de hora de inicio y fin de la competencia
--                                                 y validación de la hora del sistema para competencia
-- S4332       Jdam                  6-mar-2006    Subasta de booking
--
-- S9127       Jadiaz               25-may-2006    Adicion de indicadores TYP y BP_ID para aquellos casos  en que
--                                                 la reservacion de cupo es rechazada (REJECT).
-- S15142     GVillarreal        26-sep-2006     Inclusión de SUBSTR para envío de código USR en caso
--                                                                            de error desde trigger de BD (Particularmente error
--                                                                            BR_B_I_R_RQST_DATE
--=============================================================================
--
CURSOR NUM_COND  (p_bd_seq  BK_DAYS.seq%TYPE) IS
SELECT no_large_vsl,
       no_small_vsl,
       lev
FROM   BK_CONDS cond,
       BK_DAY_COND_ASSGS bdca,
       BK_DAYS days
WHERE  cond.lev = bdca.bco_lev
AND    bdca.seq = days.bdca_seq
AND    days.seq = p_bd_seq;

-- Variables para insertar en la tabla de Bk_Rqsts
vn_seq                       BK_RQSTS.seq%TYPE;
vv_srce                      BK_RQSTS.srce%TYPE            := 'AGENT';
vv_stat                      BK_RQSTS.stat%TYPE;
vd_stat_date                 BK_RQSTS.stat_date%TYPE;
vv_stat_rmk                  BK_RQSTS.stat_rmk%TYPE;
vv_stat_rmk_tmp              WEB_BK_REQUESTS.reason_for_rej%TYPE;
vn_bs_seq                    BK_RQSTS.bs_seq%TYPE          := p_bs_seq;
vv_rule_apply_ind            BK_RQSTS.rule_apply_ind%TYPE  := 'Y';
vn_crra_seq                  BK_RQSTS.crra_seq%TYPE        := F_FIND_AGENT_SEQ(p_agent);
--
-- Added by JaDiaz on September 23, 2008 S83474
vd_rqst_date                  BK_RQSTS.rqst_date%TYPE       := p_date_created;
vd_org_rqst_date              BK_RQSTS.rqst_date%TYPE       := p_date_created;
--
vd_bk_rqst_date              BK_RQSTS.rqst_bk_date%TYPE    := p_bk_date;
vv_rest_when_bk              BK_RQSTS.rest_when_bk%TYPE;
vn_bd_seq                    BK_RQSTS.bd_seq%TYPE;
vv_bp_id                     BK_RQSTS.bp_id%TYPE;
vv_typ                       BK_RQSTS.typ%TYPE;
vv_spc_slot_assg             BK_RQSTS.spc_slot_assg%TYPE   := 'N';
--
vn_per                       BK_PERS.id%TYPE;
vn_beam                      CUST_VSL_CHARS.extm_beam%TYPE := p_extm_beam;
vv_slot_typ                  VARCHAR2(5);
vn_direction                 ITIN_ITEMS.tii_trn_dir%TYPE   := p_trn_dir;
vn_param_valid               NUMBER;
vn_exist_vessel              NUMBER;
vn_per_date_from             NUMBER;
vd_pass_date_from            DATE;
vd_pass_date_to              DATE;
vv_open                      VARCHAR2(1);
e_null_slots                 EXCEPTION;
e_large_cond                 EXCEPTION;
e_small_cond                 EXCEPTION;
e_no_pd_code                 EXCEPTION;
e_dtu                        EXCEPTION;
e_already_bk                 EXCEPTION;
e_for_compet                 EXCEPTION;
e_invalid_dir                EXCEPTION;
e_invalid_bk_day             EXCEPTION;
e_invalid_cust_cd            EXCEPTION;
e_invalid_beam               EXCEPTION;
e_gas_free_is_missing        EXCEPTION;
--4484
e_small_dir	                 EXCEPTION;
e_large_dir	                 EXCEPTION;
e_large_dir_rest             EXCEPTION;
e_small_dir_rest             EXCEPTION;
--R.5554
e_max_large_cond             EXCEPTION;
--
vn_rest_vess                 NUMBER := 0;
vn_large_vess                BK_CONDS.no_large_vsl%TYPE;
vn_small_vess                BK_CONDS.no_small_vsl%TYPE;
vn_cond                      BK_CONDS.lev%TYPE;
vv_vessel_type               VSL_TYPS.cd%TYPE              := p_vsl_type;
vn_per_date_to               BK_PERS.date_to%TYPE;
--
--
--R3746: Added on January 09, 2004--GNV
vd_barge_stat_date           DATE;
vv_barge_rest_when_bk        BK_RQSTS.rest_when_bk%TYPE;
vn_barge_bd_seq              BK_RQSTS.bd_seq%TYPE;
vn_barge_bp_id               BK_RQSTS.bp_id%TYPE;
vv_barge_typ                 BK_RQSTS.typ%TYPE;
e_no_barge_info              EXCEPTION;

--R4104: Added on May 18, 2004 -- GNV
vn_begin_compet              NUMBER    := TO_NUMBER(Sf_Get_Param_Val(210,'BOOKING'));
vn_end_compet                NUMBER    := TO_NUMBER(Sf_Get_Param_Val(220,'BOOKING'));
--
--R.4591
vn_between_para_month        NUMBER(3);
--R.4484
vv_message                   VARCHAR2(16);
--

--S15142: Added on September 26, 2006 -- GNV
vv_sqlerrm                      VARCHAR2(2000);

--S22195:Added on Diciembre 14, 2006 --LCedeno
e_max_small_cond	EXCEPTION;

--S61519
vv_class          BK_SIZE_TYPS.CLASS%TYPE;
BEGIN

   --S59588/S61519: Added on January 08, 2007 -- LC
   --Version original de método de booking a ser reemplazado
   --por método nuevo.
   Pkg_Plog.sec_implementation ('PKG_BOOKING.P_PROCESS_BK_BEF_V1',
                                 SUBSTR(
                                   'P_Vsl_Type= '
                                 || p_vsl_type
                                 || CHR (10)
                                 ||'P_Csn_seq= '
                                 || TO_CHAR(p_csn_seq)
                                 || CHR (10)
                                 ||'p_bs_seq= '
                                 || TO_CHAR(p_bs_seq)
                                 || CHR (10)
                                 ||'p_iti_seq= '
                                 || TO_CHAR(p_iti_seq)
                                 || CHR (10)
                                 ||'p_cust_cd= '
                                 || p_cust_cd
                                 || CHR (10)
                                 ||'p_real_cust_cd= '
                                 || p_real_cust_cd
                                 || CHR (10)
                                 ||'p_date_created= '
                                 || TO_CHAR(p_date_created,'DD-MON-YYYY HH24MI')
                                 || CHR (10)
                                 ||'p_bk_date= '
                                 || TO_CHAR(p_bk_date,'DD-MON-YYYY HH24MI')
                                 || CHR (10)
								 ||'p_trn_dir= '
                                 || p_trn_dir
                                 || CHR (10)
								 ||'p_iti_trn_dir= '
                                 || p_iti_trn_dir
                                 || CHR (10)
								 ||'p_extm_beam= '
								 || TO_CHAR(p_extm_beam)
								 || CHR (10)
								 ||'p_beam= '
								 || TO_CHAR(p_beam)
								 || CHR (10)
								 ||'p_hml= '
								 || p_hml
								 || CHR (10)
								 ||'p_rest= '
								 || p_rest
								 || CHR (10)
								 ||'p_gas_free_ind= '
								 || p_gas_free_ind
								 || CHR (10)
								 ||'p_pde_cd= '
								 || p_pde_cd
								 || CHR (10)
								 ||'p_dtu_int= '
								 || p_dtu_int
								 || CHR (10)
								 ||'p_agent= '
								 || p_agent
								 || CHR (10)
								 ||'p_dtu_seq= '
								 || TO_CHAR(p_dtu_seq)
								 || CHR (10)
								 ||'p_tug_no= '
								 || TO_CHAR(p_tug_no)
								 || CHR (10)
								 ||'p_barge_no= '
								 || TO_CHAR(p_barge_no)
								 || CHR (10)
								 ||'p_br_seq= '
								 || TO_CHAR(p_br_seq)
								 || CHR (10)
								 ||'p_bk_stat= '
								 || p_bk_stat
								 || CHR (10)
								 ||'p_bk_cond= '
								 || TO_CHAR(p_bk_cond)
								 || CHR (10)
								 ||'p_error_code= '
								 || p_error_code
								 || CHR (10)
								 ||'p_success= '
								 || p_success
								 || CHR (10)
								 ||'p_no_frft_cd= '
								 || p_no_frft_cd
								 || CHR (10)
								 ||'p_loa= '
								 || TO_CHAR(p_loa)
								 || CHR (10)
								 ||'p_spec_book_ind= '
								 || p_spec_book_ind
								 || CHR (10)
								 ||'p_amount= '
								 || TO_CHAR(p_amount),1,1950),
                                   'INFO');


   --
   -- Verifica si el customer code de la solicitud es igual al del itinerario a reservar.
   IF NVL(p_real_cust_cd, '.') <> NVL(p_cust_cd, '@') THEN
       RAISE e_invalid_cust_cd;
    END IF;
   --

   -- S4332
   IF (P_SPEC_BOOK_IND IS NULL OR P_SPEC_BOOK_IND = pkg_book_datadict.f_bso_xp ) AND NVL(P_AMOUNT,0) = 0 THEN

   -- Verifica si la solicitud fue recibida durante la recoleccion para el periodo de competencia
   --R4104: Regla para validar la hora de competencia para booking
   --1. Hora del sistema se encuentra entre hora de inicio y fin del periodo de competencia
   --   a. Fecha de Request del Booking es igual a la fecha del sistema
   --      a.1 Hora de la fecha de request del booking está entre la hora de inicio y fin de la
   --          competencia
   --          Se dispara exception de que no se puede reservar hasta que termine la competencia
   --      a.2 Hora no está entre el periodo de competencia
   --          Se realiza el proceso de reservación
   --   b. Fecha del Request del booking no es igual a la fecha del sistema
   --      Se realiza el proceso de reservación
   --2. Hora del sistema no está entre el periodo de competencia
   --   Se realiza la reservacion
   --
     IF TO_NUMBER(TO_CHAR(Sf_Get_Sys_Date(),'HH24MI')) BETWEEN vn_begin_compet AND vn_end_compet THEN
        IF TRUNC(p_date_created) = TRUNC(Sf_Get_Sys_Date) THEN
           IF TO_NUMBER(TO_CHAR(p_date_created, 'HH24MI'))   BETWEEN vn_begin_compet AND vn_end_compet THEN
              RAISE e_for_compet;
           END IF;
        END IF;
     END IF;
   END IF;   -- S4332

   --
   -- Verifica si la nave ya está booked
   IF Sf_Get_Bk_Existence(p_iti_seq) THEN
      RAISE e_already_bk;
   END IF;
   --
   -- Verifica si la direccion de transito de la reservacion no coincide con la del itinerario
   IF p_trn_dir <> p_iti_trn_dir THEN
      RAISE e_invalid_dir;
   END IF;
   --

   -- S4332 16-MAR-2006
   IF (P_SPEC_BOOK_IND IS NULL OR P_SPEC_BOOK_IND = pkg_book_datadict.f_bso_xp ) AND NVL(P_AMOUNT,0) = 0 THEN

     -- Verifica si la fecha del booking es valida
     --R3746: Changed on March 16, 2004 -- GNV
      --

      --
      IF TRUNC(p_bk_date) - TRUNC(vd_rqst_date) < 2 THEN
         RAISE e_invalid_bk_day;
      END IF;

    END IF; -- S4332 16-MAR-2006


   --Select the day sequence for apply the booking
   --
   BEGIN
      SELECT seq
        INTO vn_bd_seq
        FROM BK_DAYS
       WHERE TRUNC(DATE_FROM) = TRUNC(vd_bk_rqst_date);
   EXCEPTION
      WHEN OTHERS THEN
          vn_bd_seq := NULL;
   END;
   --
   OPEN NUM_COND(vn_bd_seq);
      FETCH NUM_COND INTO vn_large_vess,vn_small_vess,vn_cond;
   CLOSE NUM_COND;

   -- S4332
   IF (P_SPEC_BOOK_IND IS NULL OR P_SPEC_BOOK_IND = pkg_book_datadict.f_bso_xp ) AND NVL(P_AMOUNT,0) = 0 THEN

      --
      --    Inicio de la evaluacion del booking
      BEGIN
        --
        -- R1889: Valores de HML para el Booking y mensaje de warning
        -- Se solicita confirmacion, cuando se esta intentdo bookear una nave tipo '04' (Tanker),
        -- y el PD Code es 'H' o '#'.
        -- FIN.
        --Select the period for the actual request date by beam
        --
        IF p_no_frft_cd IS NULL THEN     --Adicionado para R. 4887
           --
           IF TO_CHAR(p_date_created,'HH24MI') < '0900' THEN -- if/then added by mrxp-tnv apr/10/2000
              vd_rqst_date := vd_rqst_date - 1 ;
           END IF;
           --
           BEGIN
             SELECT id,
                    date_from,
                    date_to
             INTO   vn_per,
                    vn_per_date_from,
                    vn_per_date_to
             FROM   BK_PERS
             WHERE  TRUNC(vd_bk_rqst_date)-TRUNC(vd_rqst_date) BETWEEN date_from AND date_to;
           EXCEPTION
             WHEN OTHERS THEN
               vn_per := NULL;
               vn_per_date_from := NULL;
               vn_per_date_to   := NULL;
           END;
           --
           -- Verify the open period condition
           IF (TRUNC(vd_bk_rqst_date)-TRUNC(vd_rqst_date)) = vn_per_date_to THEN
               BEGIN
                 SELECT 'Y'
                 INTO   vv_open
                 FROM   dual
                 WHERE  Sf_Get_Param_Val(10,'BOOKING') <= TO_CHAR(vd_rqst_date,'HH24MI');
               EXCEPTION
                 WHEN OTHERS THEN
                   vv_open := NULL;
               END;
           END IF;
           --
        END IF;   --Adicionado para R. 4887
        --
        -- Establish the vessel type with beam
        IF vn_beam >= Sf_Get_Param_Val(56,'BOOKING') THEN
           vv_slot_typ := PKG_BOOK_DATADICT.f_bk_typ_large;
           --S61519 - Busqueda de la clase
           vv_class    := PKG_BOOK_DATADICT.f_bvtc_super;
        ELSE
           vv_slot_typ := PKG_BOOK_DATADICT.f_bk_typ_small;
           --S61519 - Busqueda de la clase
           vv_class    := PKG_BOOK_DATADICT.f_bvtc_reg;
        END IF;
        --
        --  avoid the evaluation of the itegrated barge units
        IF (NVL(p_dtu_int,' ') = 'N' AND p_dtu_seq IS NULL)OR
           (p_dtu_seq IS NOT NULL    AND
            NVL(p_dtu_int,' ') = 'Y' AND
            NVL(p_vsl_type,' ') IN ('13','14','15','23','24','25','26')) THEN

            BEGIN
              -- Pending check for the restricted vessel
              -- Checking for direction rules
              IF vv_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large THEN
                 --
                 -- Select the number of existing records with a give condition
                 /*CONDITIONS:
                   1.  When the day transit is equal to day transit of request ship
                   2.  When have a slot assigns
                   3.  When the transit direction is equal of the request vessel
                   4.  When the vessel not have status of Cancell,Rejected or Void
                 */
                 -- For validate the number of vessel allowed by the actual condition
                 IF NVL(Sf_Get_Vessel_By_Dir(3,vn_bd_seq,'',vv_slot_typ,vv_class),0) >= NVL(vn_large_vess,0) THEN
                    RAISE e_large_cond;
                 END IF;
                 -- R.4484 Cambio de programacion por el llamado a un nuevo procedimiento que encapsula los cambios solicitados en R.4374
                 Pkg_Booking.P_MESSAGE_PARAM(vd_bk_rqst_date,vd_rqst_date, vn_cond, vv_slot_typ, vn_bd_seq, p_hml, p_rest, P_TRN_DIR, vv_class, vv_message);
                 --
              ELSIF vv_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_small THEN
                 --
                 -- For validate the number of vessel allowed by the actual condition
                 IF NVL(Sf_Get_Vessel_By_Dir(3,vn_bd_seq,'',vv_slot_typ,vv_class),0) >= NVL(vn_small_vess,0) THEN
                    RAISE e_small_cond;
                 END IF;
                 -- R.4484 Cambio de programacion por llamado al procedimiento
                 Pkg_Booking.P_MESSAGE_PARAM(vd_bk_rqst_date,vd_rqst_date, vn_cond, vv_slot_typ, vn_bd_seq, p_hml, p_rest, P_TRN_DIR, vv_class, vv_message);
                 --===========================================================================
                 --
              END IF;
              --S22195: Cambio de programacion para envio de exception
                IF vv_message IS NOT NULL THEN
              	    vv_stat      := 'REJ';
              	    vd_stat_date := Sf_Get_Sys_Date;
              	    -- S9127
                    vv_bp_id     := vn_per;
                    vv_typ       := vv_slot_typ;
                    IF    vv_message = 'e_max_large_cond' THEN

                       RAISE e_max_large_cond;

                    ELSIF  vv_message = 'e_max_small_cond' THEN

                       RAISE e_max_small_cond;

                    ELSIF vv_message = 'e_small_dir' THEN

                       RAISE e_small_dir;

                    ELSIF vv_message = 'e_large_dir' THEN

                       RAISE e_large_dir;

                    ELSIF vv_message = 'e_large_dir_rest' THEN

                       RAISE e_large_dir_rest;

                    ELSIF vv_message = 'e_small_dir_rest' THEN

                       RAISE e_small_dir_rest;

                    END IF;
                END IF;
            --
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                NULL;
            END;
        END IF;    --avoid the evaluation of the itegrated barge units
        --
        BEGIN
          -- IF p_dtu_seq IS NULL THEN --:br10.INTEGR_BOOK IS NULL THEN --For avoid the evaluation of the itegrated barge units
          -- R3746: Changed on January 09, 2004 -- GNV
          -- para procesar reservaciones de barge integrated units
          -- IF p_dtu_int = 'N' THEN
          --
          IF (NVL(p_dtu_int,' ') = 'N'        AND  -- Naves Normales
              p_dtu_seq IS NULL)               OR
             (p_dtu_seq IS NOT NULL           AND  -- Barge Integrated
              NVL(p_dtu_int,' ') = 'Y'        AND
              NVL(p_vsl_type,' ') IN ('13','14','15','23','24','25','26')) THEN

              -- IF :NO_FRFT_CD IS NULL THEN
              IF  p_no_frft_cd IS NULL THEN       --Adicionado para R.4887
                  --
                  IF Sf_Exist_Slot(vn_per,vn_bd_seq,vv_slot_typ,vd_rqst_date,vv_class) THEN
                     --
                     -- :br10.bd_seq := vn_bd_seq;
                     vv_bp_id     := vn_per;
                     vv_typ       := vv_slot_typ;
                     vv_stat      := 'BKD';
                     vd_stat_date :=TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
                     vv_stat_rmk  := 'Vessel Booked';
                     --
                  ELSE
                     --
                     -- For commercial passenger vessel allocations.
                     vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY');
                     -- R.4591
                     vd_pass_date_to   := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY');
                     --
                     SELECT TO_NUMBER(TO_CHAR(TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY'),'MM')) -
                            TO_NUMBER(TO_CHAR(TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY'),'MM'))
                     INTO vn_between_para_month
                     FROM dual;
                     --
                     IF vn_between_para_month >= 0 THEN
                        --
                        /*R.4591 Evaluacion de fecha de reservacion vs periodo de gracia para cruceros
                        -- PERIODO DE EVALUACION: inicia en el año en curso y termina el mismo año
                        -- Fecha de reservacion inferior al periodo a terminarse en el año en curso
                        -- Fecha de reservacion entre periodo del año en curso
                        -- Fecha de reservacion superior al periodo en curso
                        */
                        IF     TRUNC(vd_bk_rqst_date) < TRUNC(vd_pass_date_to) THEN
                               --
                               vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY');
                               vd_pass_date_to   := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE,'YYYY'),'DD-MON-YYYY');
                               --
                        ELSIF  TRUNC(vd_bk_rqst_date) < TRUNC(vd_pass_date_to) + 365 THEN
                               --
                               vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE + 365,'YYYY'),'DD-MON-YYYY');
                               vd_pass_date_to   := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE + 365,'YYYY'),'DD-MON-YYYY');
                               --
                        ELSE
                               --
                               vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE + 730,'YYYY'),'DD-MON-YYYY');
                               vd_pass_date_to   := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE + 730,'YYYY'),'DD-MON-YYYY');
                               --
                        END IF;
                        --
			         ELSE
                        --
			            /*R.4591 Evaluacion de fecha de reservacion vs periodo de gracia para cruceros
                        -- PERIODO DE EVALUACION: inicia en el año en curso y termina el siguiente año
                        -- Fecha de reservacion inferior al periodo a iniciarse en el año en curso eg. bk-date 01-sep-04 today 01-ago-04
                        -- Fecha de reservacion entre periodo del año en curso pero inferior al siguiente periodo eg. bk-date 01-dic-04 today 01-abr-04
                        -- Fecha de reservacion superior al siguiente periodo eg. bk-date 01-dic-05 today 21-dic-04
                        */
                        --
                        IF TRUNC(vd_bk_rqst_date) < TRUNC(vd_pass_date_from) THEN
                           --
                           vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE - 365,'YYYY'),'DD-MON-YYYY');
                           --
                        ELSIF  TRUNC(vd_bk_rqst_date) < TRUNC(vd_pass_date_from) + 365 THEN
                           --
                           vd_pass_date_to   := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE + 365,'YYYY'),'DD-MON-YYYY');
                           --
                        ELSE
                           --
                           vd_pass_date_from := TO_DATE(Sf_Get_Param_Val(52,'BOOKING')||'-'||TO_CHAR(SYSDATE + 365,'YYYY'),'DD-MON-YYYY');
                           vd_pass_date_to := TO_DATE(Sf_Get_Param_Val(54,'BOOKING')||'-'||TO_CHAR(SYSDATE + 730,'YYYY'),'DD-MON-YYYY');
                           --
                        END IF;
                        --
			         END IF;
                     --
                     IF vn_per = 1                                                       AND
                        vv_vessel_type = '11'                                            AND
                        Sf_Count_Passg_Vess(vn_bd_seq) < Sf_Get_Param_Val(50,'BOOKING')  AND
                        TRUNC(vd_bk_rqst_date) BETWEEN TRUNC(vd_pass_date_from)
                                                   AND TRUNC(vd_pass_date_to)            AND
                        TRUNC(vd_bk_rqst_date)-TRUNC(vd_rqst_date) >= (vn_per_date_to - Sf_Get_Param_Val(15,'BOOKING')) THEN --R.4591
                        --
                        IF Sf_Exist_Slot(vn_per,vn_bd_seq,PKG_BOOK_DATADICT.f_bk_typ_large,vd_rqst_date, vv_class) AND
                           vv_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_small    THEN
                           --
                           vv_bp_id         := vn_per;
                           vv_typ           := PKG_BOOK_DATADICT.f_bk_typ_small;
                           vv_spc_slot_assg := 'Y';
                           vv_stat          := 'BKD';
                           vd_stat_date     := TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
                           vv_stat_rmk      :=  'Booked in large slot. Commercial Passenger Vessel';
                           --
                        ELSIF Sf_Exist_Slot(vn_per,vn_bd_seq,PKG_BOOK_DATADICT.f_bk_typ_small,vd_rqst_date,vv_class) AND
                           vv_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large       THEN
                           --
                           vv_bp_id         := vn_per;
					       vv_typ           := PKG_BOOK_DATADICT.f_bk_typ_large;
					       vv_spc_slot_assg := 'Y';
					       vv_stat          := 'BKD';
					       vd_stat_date     := TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
					       vv_stat_rmk      := 'Booked in small slot. Commercial passenger vessel';
                           --
                        ELSE
                           RAISE e_null_slots;
                        END IF;
                     ELSE
                        RAISE e_null_slots;
                     END IF;
                     --
                  END IF;
                  --
              ELSE -- p_no_frft_cd.  Adicionado para R.4887
                  --
                  IF Sf_Exist_Slot(vn_per,vn_bd_seq,vv_slot_typ,vd_rqst_date,vv_class) THEN
                     --
                     vv_bp_id     := vn_per;
                     vv_typ       := vv_slot_typ;
                     vv_stat      := 'BKD';
                     vd_stat_date := TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
                     vv_stat_rmk  := 'Vessel Booked';
                     --
                  END IF;
                  --
              END IF;
          END IF; -- For avoid the evaluation of the integrated barge units
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
	        vv_stat     := 'REJ';
	        vv_stat_rmk := 'No slots available';
            --
            -- S9127
            vv_bp_id    := vn_per;
            vv_typ      := vv_slot_typ;
            --
        END;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          --
          vv_stat      := 'REJ';
          vd_stat_date := Sf_Get_Sys_Date;
          vv_stat_rmk  := 'Period is closed for the request date';
          vn_bd_seq    := NULL;
          vv_bp_id     := NULL;
          vv_typ       := NULL;
          --
        WHEN e_null_slots THEN
          --
          vv_stat      := 'REJ';
          vd_stat_date := Sf_Get_Sys_Date;
          vv_stat_rmk  := 'No slots available';
          --
          -- S9127
          vv_bp_id     := vn_per;
          vv_typ       := vv_slot_typ;
          --
        WHEN e_small_cond THEN
          --
          vv_stat      := 'REJ';
          vd_stat_date := Sf_Get_Sys_Date;
          vv_stat_rmk  := 'No. small vessels exceeded for the condition'||vn_cond;
          --
          -- S9127
          vv_bp_id     := vn_per;
          vv_typ       := vv_slot_typ;
          --
        WHEN e_large_cond THEN
          --
          vv_stat      := 'REJ';
          vd_stat_date := Sf_Get_Sys_Date;
          vv_stat_rmk  := 'No. large vessels exceeded for the condition'||vn_cond;
          --
          -- S9127
          vv_bp_id     := vn_per;
          vv_typ       := vv_slot_typ;
        --
        --S22195: Cambio de posicion de excepciones
        WHEN e_max_large_cond THEN

              vv_stat_rmk  := 'Maximum restricted large vessels exceeded for the condition'||vn_cond;

         WHEN e_max_small_cond THEN

              vv_stat_rmk  := 'Maximum restricted small vessels exceeded for the condition'||vn_cond;

         WHEN e_small_dir THEN

              vv_stat_rmk  := 'Number of small vessels exceeded in same direction';

         WHEN e_large_dir THEN

              vv_stat_rmk  := 'Number of large vessels exceeded in same direction';

         WHEN e_large_dir_rest THEN

              vv_stat_rmk  := 'No. large vessels w/rest. exceeded in same dir.';

         WHEN e_small_dir_rest THEN

              vv_stat_rmk  := 'No. small vessels w/rest. exceeded in same dir.';

      END; -- FIN de la evaluacion del booking
      --
   ELSIF P_SPEC_BOOK_IND = pkg_book_datadict.f_bso_au AND NVL(P_AMOUNT,0) > 0 THEN -- S4332
      --
      BEGIN
        SELECT id
        INTO   vv_bp_id
        FROM   BK_PERS
        WHERE  TRUNC(vd_bk_rqst_date)-TRUNC(vd_rqst_date) BETWEEN date_from AND date_to;
      EXCEPTION
        WHEN OTHERS THEN
          vv_bp_id := NULL;
      END;

      IF vn_beam >= Sf_Get_Param_Val(56,'BOOKING') THEN
         vv_typ := PKG_BOOK_DATADICT.f_bk_typ_large;
         --
         vv_class := PKG_BOOK_DATADICT.f_bvtc_super;
      ELSE
         vv_typ :=PKG_BOOK_DATADICT.f_bk_typ_small;
         --
          vv_class := PKG_BOOK_DATADICT.f_bvtc_reg;
      END IF;

      BEGIN
        vv_stat := 'BKD';
        vd_stat_date :=TO_DATE(TO_CHAR(vd_bk_rqst_date,'DD-MON-YYYY')||' '||F_GET_ARR_PAR(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
        vv_stat_rmk := 'Vessel Booked';
      END;
   END IF; -- S4332 FIN DE BUSQUEDA DE INFORMACION PARA SUBASTA
   --
   -- Exception for the select day sequence
   ---------------------------------------------------
   IF p_extm_beam >= Sf_Get_Param_Val(56,'BOOKING') THEN
    --IF p_hml IN ('M','D', 'N','O') THEN
      IF INSTR(PKG_BOOK_DATADICT.f_hml_rest_list, p_hml) > 0 THEN
         vv_rest_when_bk :='Y';
      ELSE
         vv_rest_when_bk :='N';
      END IF;
   ELSE
      IF p_rest IS NOT NULL THEN
         vv_rest_when_bk :='Y';
      ELSE
         vv_rest_when_bk :='N';
      END IF;
   END IF;
   --
   SELECT br_seq.NEXTVAL
   INTO   vn_seq
   FROM   dual;
   --
   --
   IF p_dtu_seq IS NOT NULL        AND  -- Tug Integrated Booking Process
      NVL(p_dtu_int,' ') = 'Y'     AND
      NVL(p_vsl_type,' ') = '18'   THEN

      BEGIN
        SELECT bk.typ,
               bk.rest_when_bk,
               bk.bp_id,
               bk.bd_seq,
               bk.stat_date
        INTO   vv_barge_typ,
               vv_barge_rest_when_bk,
               vn_barge_bp_id,
               vn_barge_bd_seq,
               vd_barge_stat_date
        FROM   CUST_SCHED_NEEDS cust,
               ITIN_ITEMS       iti,
               BK_RQSTS         bk,
               SHIP_ID_NO       SIN
        WHERE cust.seq             = iti.csn_seq
        AND iti.bs_seq             = bk.bs_seq
        AND iti.seq                = Sf_Get_Iti_Book(bk.bs_seq)
        AND SIN.seq                = cust.sin_seq
        AND SIN.ship_no            = p_barge_no
        AND TRUNC(bk.rqst_bk_date) = TRUNC(vd_bk_rqst_date)
        AND bk.stat                = 'BKD';
      EXCEPTION
        WHEN OTHERS THEN
          RAISE e_no_barge_info;
      END;
      --
      --S61519
      --Busqueda de la clase para obtener BSPS_SEQ
      IF vv_barge_typ = PKG_BOOK_DATADICT.f_bk_typ_large THEN

        vv_class := PKG_BOOK_DATADICT.f_bvtc_super;

      ELSIF vv_barge_typ = PKG_BOOK_DATADICT.f_bk_typ_small THEN

        vv_class := PKG_BOOK_DATADICT.f_bvtc_reg;

      END IF;
      --
      INSERT INTO vw_booking_rqsts(  BR_SEQ
                                   , SRCE
                                   , STAT
                                   , STAT_RMK
                                   , BS_SEQ
                                   , RULE_APPLY_IND
                                   , CRRA_SEQ
                                   , RQST_DATE
                                   , STAT_DATE
                                   , REST_WHEN_BK
                                   , RQST_BK_DATE
                                   , BD_SEQ
                                   , BP_ID
                                   , TYP
                                   , SPC_SLOT_ASSG
                                   , PENALTY_IND
                                   , DTU_SEQ
                                   , FIX_AMT        -- S4332
                                   , BSPS_SEQ )
      VALUES(  vn_seq
             , vv_srce
             , 'BKD'
             , 'Vessel Booked integrated w/Barge SIN='||TO_CHAR(p_barge_no)
             , vn_bs_seq
             , vv_rule_apply_ind
             , vn_crra_seq
             --
             -- Comment by JaDiaz on September 23, 2008 S83474
             -- , vd_rqst_date
             , vd_org_rqst_date
             --
             , vd_barge_stat_date
             , vv_barge_rest_when_bk
             , vd_bk_rqst_date
             , vn_barge_bd_seq
             , vn_barge_bp_id
             , vv_barge_typ
             , vv_spc_slot_assg
             ,'N'
             ,p_dtu_seq
             ,p_amount     -- S4332
             ,PKG_BOOK_REFTAB_UTIL.f_get_bk_bsps_seq(vn_barge_bd_seq, vv_barge_typ, vn_barge_bp_id, vv_class ));
      --
      IF SQL%FOUND THEN
         p_br_seq  := vn_seq;
         p_success := 'TRUE';
         p_bk_stat := 'BKD';
         p_bk_cond := vn_cond;
      END IF;
      --
   ELSE
      --
      INSERT INTO vw_booking_rqsts(  BR_SEQ
                                   , SRCE
                                   , STAT
                                   , STAT_RMK
                                   , BS_SEQ
                                   , RULE_APPLY_IND
                                   , CRRA_SEQ
                                   , RQST_DATE
                                   , STAT_DATE
                                   , REST_WHEN_BK
                                   , RQST_BK_DATE
                                   , BD_SEQ
                                   , BP_ID
                                   , TYP
                                   , SPC_SLOT_ASSG
                                   , PENALTY_IND
                                   , DTU_SEQ
                                   , FIX_AMT   -- S4332
                                   , BSPS_SEQ)
      VALUES(  vn_seq
             , vv_srce
             , vv_stat
             , vv_stat_rmk
             , vn_bs_seq
             , vv_rule_apply_ind
             , vn_crra_seq
             --
             -- Comment by JaDiaz on September 23, 2008 S83474
             -- , vd_rqst_date
             , vd_org_rqst_date
             --
             , vd_stat_date
             , vv_rest_when_bk
             , vd_bk_rqst_date
             , vn_bd_seq
             , vv_bp_id
             , vv_typ
             , vv_spc_slot_assg
             ,'N'
             , p_dtu_seq
             , p_amount       -- S4332
             ,PKG_BOOK_REFTAB_UTIL.f_get_bk_bsps_seq(vn_bd_seq, vv_typ, vv_bp_id, vv_class ));
      --
      IF SQL%FOUND THEN
         p_br_seq  := vn_seq;
         p_success := 'TRUE';
         p_bk_stat := vv_stat;
         p_bk_cond := vn_cond;
      END IF;
      --
   END IF;
   --
EXCEPTION
   --
   WHEN NO_DATA_FOUND THEN
     --
     IF vd_bk_rqst_date IS NULL THEN
        --
        p_success := 'FALSE';
        p_error_code := 'USR-00114';
        Pkg_Evtms_Db_Util.p_error('USR-00114','The request booking date is null');
        --
     ELSE
        --
        p_success := 'FALSE';
        p_error_code := 'USR-00115';
        Pkg_Evtms_Db_Util.p_error('USR-00115','Not exist booking date in BK_DAYS table');
        --
     END IF;
     --
   WHEN e_already_bk THEN
     --
     p_success := 'FALSE';
     p_error_code := 'USR-30106';
     Pkg_Evtms_Db_Util.p_error('USR-30106','This vessel was already booked');
     --
   WHEN e_for_compet THEN
     --
     p_success := 'FALSE';
     p_error_code := 'USR-01866';
     Pkg_Evtms_Db_Util.p_error('USR-01866','Booking Competition Period has not finished yet. Please try later ...');
     --
   WHEN e_invalid_dir THEN
     --
     p_success := 'FALSE';
     p_error_code := 'USR-30108';
     Pkg_Evtms_Db_Util.p_error('USR-30108','Booking request with itinerary of different transit direction');
     --
   WHEN e_invalid_bk_day THEN
     --
     p_success := 'FALSE';
     p_error_code := 'USR-30109';
     Pkg_Evtms_Db_Util.p_error('USR-30109','There is not an open period for this booking date');
     --
   WHEN e_invalid_cust_cd THEN
     --
     p_success := 'FALSE';
     p_error_code := 'USR-30110';
     Pkg_Evtms_Db_Util.p_error('USR-30110','Differences in Customer Codes');
     --
   WHEN e_invalid_beam THEN
     --
     p_success := 'FALSE';
     p_error_code := 'USR-30111';
     Pkg_Evtms_Db_Util.p_error('USR-30111','Differences in Vessel Beams');
     --
   WHEN e_gas_free_is_missing THEN
     --
     p_success := 'FALSE';
     p_error_code := 'USR-30117';
     Pkg_Evtms_Db_Util.p_error('USR-30117','Gas Free can not be null for Tankers or Dry/Bulk Liquid Carrie');
     --
   WHEN e_no_barge_info THEN
     --
     p_success := 'FALSE';
     p_error_code := 'USR-30058';
     Pkg_Evtms_Db_Util.p_error('USR-30058','This tug cannot be booked because it was integrated to a barge. Need to book barge first');
     --
   WHEN  OTHERS THEN
     --
     p_success := 'FALSE';

     /*S15142:Changed on September 26, 2006 -- GNV
     p_error_code := SQLCODE;
     Pkg_Evtms_Db_Util.p_error(SQLCODE,SQLERRM);
     --
     */

     vv_sqlerrm := SQLERRM;

     IF INSTR(vv_SQLERRM, 'USR-') > 0 THEN

        p_success := 'FALSE';
        p_error_code := SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9);
        pkg_evtms_db_util.p_error(SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9),'Error');

     ELSE

        IF INSTR(vv_SQLERRM, 'USR-') = 0 THEN

           p_success := 'FALSE';
           p_error_code   := SQLCODE;
           pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

        ELSE

           p_success := 'FALSE';
           p_error_code   := SQLCODE;
           pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

        END IF;

     END IF;

     Pkg_Plog.sec_implementation ('PKG_BOOKING.P_PROCESS_BK_BEF_V1',
                                 SUBSTR(
                                   'P_Vsl_Type= '
                                 || p_vsl_type
                                 || CHR (10)
                                 ||'P_Csn_seq= '
                                 || TO_CHAR(p_csn_seq)
                                 || CHR (10)
                                 ||'p_bs_seq= '
                                 || TO_CHAR(p_bs_seq)
                                 || CHR (10)
                                 ||'p_iti_seq= '
                                 || TO_CHAR(p_iti_seq)
                                 || CHR (10)
                                 ||'p_cust_cd= '
                                 || p_cust_cd
                                 || CHR (10)
                                 ||'p_real_cust_cd= '
                                 || p_real_cust_cd
                                 || CHR (10)
                                 ||'p_date_created= '
                                 || TO_CHAR(p_date_created,'DD-MON-YYYY HH24MI')
                                 || CHR (10)
                                 ||'p_bk_date= '
                                 || TO_CHAR(p_bk_date,'DD-MON-YYYY HH24MI')
                                 || CHR (10)
								 ||'p_trn_dir= '
                                 || p_trn_dir
                                 || CHR (10)
								 ||'p_iti_trn_dir= '
                                 || p_iti_trn_dir
                                 || CHR (10)
								 ||'p_extm_beam= '
								 || TO_CHAR(p_extm_beam)
								 || CHR (10)
								 ||'p_beam= '
								 || TO_CHAR(p_beam)
								 || CHR (10)
								 ||'p_hml= '
								 || p_hml
								 || CHR (10)
								 ||'p_rest= '
								 || p_rest
								 || CHR (10)
								 ||'p_gas_free_ind= '
								 || p_gas_free_ind
								 || CHR (10)
								 ||'p_pde_cd= '
								 || p_pde_cd
								 || CHR (10)
								 ||'p_dtu_int= '
								 || p_dtu_int
								 || CHR (10)
								 ||'p_agent= '
								 || p_agent
								 || CHR (10)
								 ||'p_dtu_seq= '
								 || TO_CHAR(p_dtu_seq)
								 || CHR (10)
								 ||'p_tug_no= '
								 || TO_CHAR(p_tug_no)
								 || CHR (10)
								 ||'p_barge_no= '
								 || TO_CHAR(p_barge_no)
								 || CHR (10)
								 ||'p_br_seq= '
								 || TO_CHAR(p_br_seq)
								 || CHR (10)
								 ||'p_bk_stat= '
								 || p_bk_stat
								 || CHR (10)
								 ||'p_bk_cond= '
								 || TO_CHAR(p_bk_cond)
								 || CHR (10)
								 ||'p_error_code= '
								 || p_error_code
								 || CHR (10)
								 ||'p_success= '
								 || p_success
								 || CHR (10)
								 ||'p_no_frft_cd= '
								 || p_no_frft_cd
								 || CHR (10)
								 ||'p_loa= '
								 || TO_CHAR(p_loa)
								 || CHR (10)
								 ||'p_spec_book_ind= '
								 || p_spec_book_ind
								 || CHR (10)
								 ||'p_amount= '
								 || TO_CHAR(p_amount)
								 || CHR (10)
								 ||'SQLERRM= '
								 || SQLERRM,1,1950),
                                   'INFO');

END;
/* Calculate booking fee, period, arrival time for booking requests */
PROCEDURE P_RTV_BOOK_FEE
 (P_BS_SEQ IN BK_RQSTS.BS_SEQ%TYPE
 ,P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_EXTM_BEAM IN CUST_VSL_CHARS.EXTM_BEAM%TYPE
 ,P_HML IN CUST_VSL_CHARS.LVC_HML_QUALIFIED%TYPE
 ,P_REST IN REST_USR_CDS.CD%TYPE
 ,P_BK_FEE OUT CHARGES.CHRG_AMT%TYPE
 ,P_BK_PERIOD OUT BK_RQSTS.BP_ID%TYPE
 ,P_SLOT_AVAIL OUT NUMBER
 ,P_ARR_TIME OUT VARCHAR2
 ,P_REJ_REASON OUT BK_RQSTS.STAT_RMK%TYPE
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de calcular el booking fee, period, arrival time, rejected
--                reason para un booking request hecho.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               11-nov-2003               Version Inicial transportada desde el
--                                                                                          procedimiento P_PROCESS_BOOKING
--                                                                                          para realizar el cálculo de los booking fee
--                                                                                          en forma externa.
--                                                    12-nov-2003              Inclusión de procedimiento de manejo de errores
--                                                     24-nov-2003             Incluir parámetro de salida P_PC_UMS_PRE_FEE
--                                                    11-dec-2003              Cambios en cursor C_GET_BK_RQST_REC para
--                                                                                          seleccionar registros con la secuencia de
--                                                                                          BK_RQSTS
--                                                                                          Eliminación de parámetro P_PC_UMS_PRE_FEE
--=============================================================================

/* R3430: Modified on December 11, 2003 -- GNV
CURSOR c_get_bk_rqst_rec(p_bs_seq  IN  bk_rqsts.bs_seq%TYPE) IS
SELECT seq,
                stat,
                stat_rmk,
                bs_seq,
                rqst_date,
                rqst_bk_date,
                typ,
                bp_id
   FROM  bk_rqsts br
 WHERE br.bs_seq = p_bs_seq
       AND br.seq = (SELECT max(brs.seq)
                                     FROM bk_rqsts brs
                                  WHERE brs.bs_seq = p_bs_seq
                                        AND brs.stat <> 'VOID BKD');
*/

CURSOR c_get_bk_rqst_rec(p_bs_seq  IN  bk_rqsts.bs_seq%TYPE,
                                                       p_br_seq   IN  bk_rqsts.seq%TYPE) IS
SELECT seq,
                stat,
                stat_rmk,
                bs_seq,
                rqst_date,
                rqst_bk_date,
                typ,
                bp_id
   FROM  bk_rqsts br
 WHERE br.bs_seq = p_bs_seq
       AND br.seq = p_br_seq
       AND br.stat <> 'VOID BKD';


vn_seq                             bk_rqsts.seq%TYPE;
vv_stat                             bk_rqsts.stat%TYPE;
vv_stat_rmk                   bk_rqsts.stat_rmk%TYPE;
vn_bs_seq                      bk_rqsts.bs_seq%TYPE;
vd_rqst_date                  bk_rqsts.rqst_date%TYPE;
vd_rqst_bk_date           bk_rqsts.rqst_bk_date%TYPE;
vv_typ                               bk_rqsts.typ%TYPE;
vn_bp_id                          bk_rqsts.bp_id%TYPE;
BEGIN

    OPEN c_get_bk_rqst_rec(p_bs_seq,p_br_seq);

    IF c_get_bk_rqst_rec%ISOPEN THEN

       FETCH c_get_bk_rqst_rec INTO  vn_seq,
                                     vv_stat,
                                     vv_stat_rmk,
                                     vn_bs_seq,
                                     vd_rqst_date,
                                     vd_rqst_bk_date,
                                     vv_typ,
                                     vn_bp_id;

       IF c_get_bk_rqst_rec%NOTFOUND THEN

          vn_seq          := NULL;
          vv_stat         := NULL;
          vv_stat_rmk     := NULL;
          vn_bs_seq       := NULL;
          vd_rqst_date    := NULL;
          vd_rqst_bk_date := NULL;
          vv_typ          := NULL;
          vn_bp_id        := NULL;

       END IF;

       CLOSE c_get_bk_rqst_rec;

    END IF;

    IF vv_stat = 'BKD' THEN

        p_bk_fee     := f_get_bk_fee(p_bs_seq);
        p_bk_period  := vn_bp_id;
        p_slot_avail := f_count_slot_avail(TRUNC(vd_rqst_bk_date), 'SLOT', vv_typ, vd_rqst_date);
        --R3430 Changed on December 18, 2003 -- GNV
        --p_arr_time   := to_date(to_char(vd_rqst_bk_date,'DD-MON-YYYY')||f_get_arr_par(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');
        p_arr_time   := f_get_arr_par(p_bs_seq,p_extm_beam,p_hml,p_rest);

    ELSIF  vv_stat = 'REJ' THEN

        p_rej_reason := vv_stat_rmk;

    END IF;

EXCEPTION
WHEN OTHERS THEN

    pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

END;
PROCEDURE P_CANCEL_BOOK
 (P_BK_SEQ IN WEB_BK_RQST_TRANS.SEQ%TYPE
 ,P_BK_STATUS IN WEB_BK_RQST_TRANS.RQST_STATUS%TYPE
 ,P_DATE_CREATED IN DATE
 ,P_BR_SEQ IN BK_RQSTS.BR_SEQ_REBK%TYPE
 ,P_BS_SEQ IN BK_RQSTS.BS_SEQ%TYPE
 ,P_EXTM_BEAM IN CUST_VSL_CHARS.EXTM_BEAM%TYPE
 ,P_HML IN CUST_VSL_CHARS.PERM_HL_IND%TYPE
 ,P_REST IN REST_USR_CDS.CD%TYPE
 ,P_TEMP_STATUS OUT BK_RQSTS.STAT%TYPE
 ,P_ERROR OUT VARCHAR2
 ,P_SUCCESS OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de cancelar los booking requests hechos por un buque.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               22-oct-2003               Version Inicial transportada desde el
--                                                                                          procedimiento P_CANCEL_BOOK
--                                                                                          de la pantalla VI5070FM (Process
--                                                                                          Booking  Request
--                                                    06-nov-2003               Inclusión de cláusula para bloquear registros
--                                                    10-nov-2003               Desactivar cálculo de cancellation fee y
--                                                                                          cancellation days el cual será hecho en el
--                                                                                          procedimiento P_RTV_CAN_FEE
--                                                    24-nov-2003              Incluir uso de vistas para booking requests
--S15142  GVillarreal                26-sep-2006              Inclusión de SUBSTR para manejo de mensajes
--                                                                                         USR- al enviar el parámetro de salida del código
--                                                                                          de error en WHEN-OTHERS
--=============================================================================

vv_sqlerrm             VARCHAR2(2000);
vv_days                   NUMBER;
vv_bk_date             bk_rqsts.rqst_bk_date%type;
vd_can_date          DATE;
e_not_booked       EXCEPTION;
BEGIN

   IF p_bk_seq IS NOT NULL THEN

         --
         IF p_bk_status = 'PEND' THEN

                BEGIN

                    -- R3430: Desactivado en Noviembre 11, 2003 -- GNV
                    --vd_can_date := p_date_created;
                    --
                    --

                    -- R3430: Added on November 7, 2003 -- GNV
                    -- to lock records for update

                    pkg_evtms_db_util.p_lock_table_rows('BK_RQSTS',FALSE,'SEQ = :1 AND STAT = :2',p_br_seq,'''BKD''');

                    /* R3430: Changed on November 24, 2003 -- GNV
                    UPDATE bk_rqsts
                       SET   stat = 'CAN'
                           , stat_rmk  = 'Cancel by Agent Request'
                           , stat_date = vd_can_date
                     WHERE seq = p_br_seq
                       AND stat = 'BKD';
                    */

                     UPDATE vw_booking_rqsts
                       SET   stat = 'CAN'
                           , stat_rmk  = 'Cancel by Agent Request'
                           , stat_date = p_date_created
                     WHERE br_seq = p_br_seq
                       AND stat = 'BKD';


                    --
                    -- No hay estatus BKD para cancelar
                    IF SQL%NOTFOUND THEN

                         RAISE e_not_booked;

                    ELSE

                        p_success := 'TRUE';

                    END IF;
                    --
                    --
                    /* R3430: Desactivado en Octubre 22, 2003 -- GNV
                              para mejoras a la recolecciónd de datos.
                              Tabla Web_Bk_Cancls desaparece
                    UPDATE web_bk_cancls
                    SET status = 'APPRV'
			        --
			        -- VJaen 18-Dec-2002
			        -- Envio de informacion de housekeeping para la base de datos intermedia
			        , date_updated = (SELECT SYSDATE FROM dual)
			        , update_user_id = user
                    WHERE seq = :wbc.seq;
                    */
                    --
                    --
                    p_temp_status := 'APPRV';

                    --R3430: Desactivado en Octubre 22, 2003 -- GNV
                    --commit;
                    --
                    --

                    /* R3430: Desactivado en Noviembre 11, 2003 -- GNV
                       El cálculo se llevará a cabo en el procedimiento
                       P_RTV_CAN_FEE

                    SELECT rqst_bk_date
                      INTO vv_bk_date
                      FROM bk_rqsts
                     WHERE seq = p_br_seq;
                     --
                     --

                     vv_bk_date := TO_DATE(TO_CHAR(vv_bk_date,'DD-MON-YYYY ')||f_get_arr_par(p_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');

                     vv_days := ABS(vv_bk_date - vd_can_date);

                     p_can_days := TRUNC(vv_days);

                     p_can_hours := TRUNC(ABS(vv_days - trunc(vv_days))*24);

				     p_calc_cancel_fee(p_br_seq,p_can_fee);

				     */

                EXCEPTION
                   WHEN e_not_booked THEN

                        --R3430: Changed to BD message on October 22, 2003 -- GNV
                        --MSG_ALERT('This vessel is not in Booked status.', 'E', false);

                        --R3430: Changed on November 12, 2003 -- GNV
                        --raise_application_error(-20001,'This vessel is not in Booked Status');
                        p_success := 'FALSE';
                        p_error := 'USR-01749';
                        pkg_evtms_db_util.p_error('USR-01749','This vessel is not in Booked Status');

                   WHEN OTHERS THEN

                        vv_sqlerrm := SQLERRM;

                        IF INSTR(vv_SQLERRM, 'USR-') > 0 THEN

                           --R3430: Changed to BD message on October 22, 2003 -- GNV
                           --QMS$FORMS_ERRORS.PUSH(SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9), 'E','OFG',0);

                           --R3430: Changed on November 12, 2003 -- GNV
                           --raise_application_error(-20001,SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9));

                           p_success := 'FALSE';

                           --S15142: Changed on September 26, 2006 -- GNV
                           --p_error   := SQLCODE;
                           p_error := SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9);
                           pkg_evtms_db_util.p_error(SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9),'Error');

                        ELSE

                           IF INSTR(vv_SQLERRM, 'USR-') = 0 THEN

                              --R3430: Changed to BD message on October 22, 2003 -- GNV
                              --MSG_ALERT(vv_SQLERRM, 'E',FALSE);

                              --R3430: Changed on November 12, 2003 -- GNV
                              --raise_application_error(-20001,vv_sqlerrm);

                              p_success := 'FALSE';
                              p_error   := SQLCODE;
                              pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);


                           ELSE

                              --R3430: Changed to BD message on October 22, 2003 -- GNV
                              --MSG_ALERT(SQLCODE||'-'||vv_SQLERRM, 'E',FALSE);

                              --R3430: Changed on November 12, 2003 -- GNV
                              --raise_application_error(-20001,SQLCODE||'-'||vv_sqlerrm);
                              p_success := 'FALSE';
                              p_error   := SQLCODE;
                              pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

                           END IF;

                        END IF;

                END;

            ELSE

                --R3430: Changed to BD message on October 22, 2003 -- GNV
                -- This request was already processed.
                --QMS$FORMS_ERRORS.PUSH('USR-30114','E','OFG',0);

                -- R3430: Changed on November 12, 2003 -- GNV
                --raise_application_error(-20001,'USR-30114');
                p_success := 'FALSE';
                p_error   := 'USR-30114';
                pkg_evtms_db_util.p_error('USR-30114','This request was already processed');

            END IF;

   END IF;

EXCEPTION
   WHEN OTHERS THEN

        /*S15142: Changed on September 26, 2006 -- GNV
        p_success := 'FALSE';
        p_error   := SQLCODE;
        pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);
        */

        vv_sqlerrm := SQLERRM;

        IF INSTR(vv_SQLERRM, 'USR-') > 0 THEN

           p_success := 'FALSE';
           p_error := SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9);
           pkg_evtms_db_util.p_error(SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9),'Error');

        ELSE

           IF INSTR(vv_SQLERRM, 'USR-') = 0 THEN

              p_success := 'FALSE';
              p_error   := SQLCODE;
              pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

           ELSE

              p_success := 'FALSE';
              p_error   := SQLCODE;
              pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

           END IF;

        END IF;

END;
/* Retrieve cancellation fees for booking cancellation */
PROCEDURE P_RTV_CAN_FEE
 (P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_DATE_CREATED IN DATE
 ,P_EXTM_BEAM IN CUST_VSL_CHARS.EXTM_BEAM%TYPE
 ,P_HML IN CUST_VSL_CHARS.LVC_HML_QUALIFIED%TYPE
 ,P_REST IN REST_USR_CDS.CD%TYPE
 ,P_CAN_DAYS OUT NUMBER
 ,P_CAN_HOURS OUT NUMBER
 ,P_CAN_FEE OUT NUMBER
 ,P_DAYLIGHT_TRN_CAN_FEE OUT NUMBER
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de calcular el cancellation fee, cancellation days and
--               hours  para un booking request hecho.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               11-nov-2003               Version Inicial transportada desde el
--                                                                                          procedimiento P_CANCEL_BOOK
--                                                                                          para realizar el cálculo de los booking fee
--                                                                                          en forma externa.
--                                                    12-nov-2003              Inclusión de procedimiento de manejo de errores
--                                                    26-nov-2003              Inclusión de parámetro de salida
--                                                                                         P_DAYLIGHT_TRN_CAN_FEE
--=============================================================================


CURSOR c_get_bk_rqst_rec(p_br_seq  IN  bk_rqsts.seq%TYPE) IS
SELECT seq,
                stat,
                stat_rmk,
                bs_seq,
                rqst_date,
                rqst_bk_date,
                typ,
                bp_id
   FROM  bk_rqsts br
 WHERE br.seq = p_br_seq;


vn_seq                             bk_rqsts.seq%TYPE;
vv_stat                             bk_rqsts.stat%TYPE;
vv_stat_rmk                   bk_rqsts.stat_rmk%TYPE;
vn_bs_seq                      bk_rqsts.bs_seq%TYPE;
vd_rqst_date                  bk_rqsts.rqst_date%TYPE;
vd_rqst_bk_date           bk_rqsts.rqst_bk_date%TYPE;
vv_typ                               bk_rqsts.typ%TYPE;
vn_bp_id                          bk_rqsts.bp_id%TYPE;
vd_can_date                  DATE;
vd_bk_date                    DATE;
vn_days                           NUMBER;
BEGIN

    OPEN c_get_bk_rqst_rec(p_br_seq);

    IF c_get_bk_rqst_rec%ISOPEN THEN

        FETCH c_get_bk_rqst_rec INTO  vn_seq,
                                      vv_stat,
                                      vv_stat_rmk,
                                      vn_bs_seq,
                                      vd_rqst_date,
                                      vd_rqst_bk_date,
                                      vv_typ,
                                      vn_bp_id;

        IF c_get_bk_rqst_rec%NOTFOUND THEN

            vn_seq          := NULL;
            vv_stat         := NULL;
            vv_stat_rmk     := NULL;
            vn_bs_seq       := NULL;
            vd_rqst_date    := NULL;
            vd_rqst_bk_date := NULL;
            vv_typ          := NULL;
            vn_bp_id        := NULL;

        END IF;

        CLOSE c_get_bk_rqst_rec;

    END IF;

    vd_can_date := p_date_created;

    vd_bk_date := TO_DATE(TO_CHAR(vd_rqst_bk_date,'DD-MON-YYYY ')||f_get_arr_par(vn_bs_seq,p_extm_beam,p_hml,p_rest),'DD-MON-YYYY HH24MI');

    vn_days := ABS(vd_bk_date - vd_can_date);

    p_can_days := TRUNC(vn_days);

    p_can_hours := TRUNC(ABS(vn_days - trunc(vn_days))*24);

    p_calc_cancel_fee(p_br_seq,p_can_fee);

EXCEPTION
   WHEN OTHERS THEN

        pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

END;
PROCEDURE P_CALC_CANCEL_FEE
 (P_BR_SEQ IN BK_RQSTS.BR_SEQ_REBK%TYPE
 ,P_CAN_FEE OUT NUMBER
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de calcular el cargo por cancelación de reservación de
--                 buque.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               22-oct-2003               Version Inicial transportada desde el
--                                                                                          procedimiento P_DISPLAY_CANCEL_FEE
--                                                                                          de la pantalla VI5070FM (Process
--                                                                                          Booking  Request

--=============================================================================

vt_book_fees  PKG_MB620000PR.charge_table_type;
vn_fee               NUMBER := 0;
BEGIN

    vt_book_fees := PKG_MB620000PR.F_GET_BOOK_CHARGES(p_br_seq,
                                                      'CF',
                                                      'T',
                                                      'S',
                                                       NULL,
                                                       NULL,
                                                       NULL,
                                                       NULL,
                                                       NULL,
                                                       NULL,
                                                       NULL,
                                                       NULL,
                                                       NULL,
                                                       NULL);
    --
    --
    FOR table_reg IN 1..vt_book_fees.COUNT
    LOOP

        vn_fee := vn_fee + vt_book_fees(table_reg).amount;

    END LOOP;
    --
    --

	p_can_fee := vn_fee;


EXCEPTION
   WHEN OTHERS THEN

      p_can_fee := NULL;

END;
PROCEDURE P_RTV_PC_UMS
 (P_CSN_SEQ IN CUST_SCHED_NEEDS.SEQ%TYPE
 ,P_TRN_BR_DATE IN DATE
 ,P_PC_UMS_NET OUT NUMBER
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de buscar la informacíon de PC/UMS válida para una visita
--                del barco en la fecha de la visita.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               25-nov-2003               Version Inicial
--                                                   11-dec-2003               Eliminar parámetros de salida P_PC_GROSS,
--                                                                                          P_PC_UMS_DECK
--=============================================================================

vt_chars_tab  		pkg_csn_vsl_chars.csn_cvc_table;  -- Definition table for chars
wpcums_net_ton		pc_ums_certs.net_tons%TYPE;
vn_no_row                                   NUMBER;
BEGIN

    vn_no_row := 1;

    vt_chars_tab      := pkg_csn_vsl_chars.f_get_csn_chars('V',p_csn_seq,p_trn_br_date,NULL);
    wpcums_net_ton    := vt_chars_tab(vn_no_row).pcums_net_ton;

    p_pc_ums_net  := wpcums_net_ton;


EXCEPTION
  WHEN OTHERS THEN

    pkg_evtms_db_util.p_error(sqlcode,sqlerrm);

END;
/* Insert declared hazardous cargo items */
PROCEDURE P_INS_MJR_HAZ_CGO
 (P_CSN_SEQ IN CUST_SCHED_NEEDS.SEQ%TYPE
 ,P_HCA_SEQ IN HAZ_CLASS_ASSIGS.SEQ%TYPE
 ,P_HCUC_UN_NO IN HAZ_COMMD_UN_CLASS.UN_NO%TYPE
 ,P_HCID_CLASS_NO IN HAZ_COMMD_IMO_CLASS.CLASS%TYPE
 ,P_HCID_DIV_NO IN HAZ_COMMD_IMO_DIV.DIV_NO%TYPE
 ,P_PROPER_SHIPNAME IN HAZ_CLASS_ASSIGS.PROPR_SHIPPG_NAME%TYPE
 ,P_METRIC_TONS IN HAZ_CGO_ITEMS.METRIC_TONS%TYPE
 ,P_STRGE_TYP IN HAZ_CGO_ITEMS.STRGE_TYP%TYPE
 ,P_AD_SEQ IN ACTVT_DETS.SEQ%TYPE
 ,P_CVD_SEQ IN CUST_VISIT_DETS.SEQ%TYPE
 ,P_HCTN_SEQ IN HAZ_COMMD_TRADE_NAMES.SEQ%TYPE
 ,P_HCCC_CD IN HAZ_COMMD_CHRIS_CLASS.CD%TYPE
 ,P_PSN_SEQ IN PCC_ITEM_SCHED_NEEDS.SEQ%TYPE
 ,P_RMK IN HAZ_CGO_ITEMS.RMK%TYPE
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de insertar la información de MAJOR DANGEROUS
--                CARGO del formulario web Transit Booking Request en la tabla HAZ_CGO_ITEMS
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               30-oct-2003               Version Inicial.
--                                                    06-nov-2003             Activación de validación para inserción de registro
--                                                                                        de Major Hazardous Cargo buscando registro de
--                                                                                        tipo OHC
--                                                                                        Incluir como parámetros de entrada las otras
--                                                                                        columnas del registro de la tabla HAZ_CGO_ITEMS
--                                                   24-nov-2003             Eliminar parámetros  e inserción a columna
--                                                                                        de P_ICSC_ID y P_CTD_ID

--=============================================================================


vn_long_tons                      haz_cgo_items.long_tons%TYPE;
vv_dcled_item_ind            haz_cgo_items.dcled_item_ind%TYPE := 'N';
vv_prel_item_ind               haz_cgo_items.prel_item_ind%TYPE := 'Y';
vv_haz_cgo_typ                 haz_cgo_items.typ%TYPE := 'OHC';
vn_cnt_decl_item              number(8);
haz_cgo_ins_already       EXCEPTION;
BEGIN


    BEGIN

      /* Desactivated until we can resolve this question:
         we need to make a revision to  make a query about
         the information registered from the cargo declaration web form.
         If the declared indicator will be set to Y and the
         typ = 'OHC' from the hazardous cargo declaration to validate
         if declared hazardous cargo exists before insertion or
         not. Another doubt if the validation consists to look
         for any record of hazardous cargo declaration detail with the
         same IMO CLASS, DIVISION, UN NUMBER   of the record that
         wants to be inserted from the Booking Request web Form or
         if we need to look for any record with typ = 'OHC' and
         declared item ind = Y to avoid insertion of hazardous cargo
         from booking request web form */

      /* Activated on November 5, 2003 -- GNV
         the validation will be considered as if the visit has records
         with type = 'OHC' the record of cargo from booking request
         will not be inserted */


      SELECT count(*)
        INTO vn_cnt_decl_item
        FROM haz_cgo_items hci
       WHERE hci.csn_seq = p_csn_seq
             AND hci.typ = vv_haz_cgo_typ;
             --AND dcled_item_ind = vv_dcled_item_ind;

    EXCEPTION
       WHEN OTHERS THEN

          vn_cnt_decl_item := NULL;

    END;

    IF vn_cnt_decl_item IS NOT NULL AND
       NOT(vn_cnt_decl_item > 0)    THEN

       vn_long_tons := to_number(sf_conv_mt_eng('L',nvl(p_metric_tons,0),4));

       INSERT INTO haz_cgo_items(seq,
                                 dcled_item_ind,
                                 typ,
                                 strge_typ,
                                 prel_item_ind,
                                 csn_seq,
                                 hca_seq,
                                 hcuc_un_no,
                                 hcid_class_no,
                                 hcid_div_no,
                                 long_tons,
                                 metric_tons,
                                 ad_seq,
                                 cvd_seq,
                                 hctn_seq,
                                 hccc_cd,
                                 psn_seq,
                                 rmk)
       VALUES(hci_seq.nextval,
              vv_dcled_item_ind,
              vv_haz_cgo_typ,
              p_strge_typ,
              vv_prel_item_ind,
              p_csn_seq,
              p_hca_seq,
              p_hcuc_un_no,
              p_hcid_class_no,
              p_hcid_div_no,
              vn_long_tons,
              p_metric_tons,
              p_ad_seq,
              p_cvd_seq,
              p_hctn_seq,
              p_hccc_cd,
              p_psn_seq,
              p_rmk);

    ELSE

        raise haz_cgo_ins_already;

    END IF;

EXCEPTION
WHEN haz_cgo_ins_already THEN

     --R3430: Changed on November 12, 2003
     --RAISE_APPLICATION_ERROR(-20000,'On Board Hazardous Cargo already registered');
     pkg_evtms_db_util.p_error('USR-01752','On Board Hazardous Cargo already registered');

WHEN OTHERS THEN

     --R3430: Changed on November 12, 2003
     --RAISE_APPLICATION_ERROR(-20000,'Insert of Major Dangerous Cargo Failed');
     pkg_evtms_db_util.p_error('USR-01751','Insert of Major Dangerous Cargo Failed');

END;
/* Insert declared hazardous cargo items */
PROCEDURE P_INS_LAST_HAZ_CGO
 (P_CSN_SEQ IN CUST_SCHED_NEEDS.SEQ%TYPE
 ,P_HCA_SEQ IN HAZ_CLASS_ASSIGS.SEQ%TYPE
 ,P_HCUC_UN_NO IN HAZ_COMMD_UN_CLASS.UN_NO%TYPE
 ,P_HCID_CLASS_NO IN HAZ_COMMD_IMO_CLASS.CLASS%TYPE
 ,P_HCID_DIV_NO IN HAZ_COMMD_IMO_DIV.DIV_NO%TYPE
 ,P_PROPER_SHIPNAME IN HAZ_CLASS_ASSIGS.PROPR_SHIPPG_NAME%TYPE
 ,P_METRIC_TONS IN HAZ_CGO_ITEMS.METRIC_TONS%TYPE
 ,P_STRGE_TYP IN HAZ_CGO_ITEMS.STRGE_TYP%TYPE
 ,P_PSN_SEQ IN PCC_ITEM_SCHED_NEEDS.SEQ%TYPE
 ,P_GAS_FREE_IND IN CUST_VISIT_DETS.TBLC_GAS_FREE_IND%TYPE
 ,P_AD_SEQ IN ACTVT_DETS.SEQ%TYPE
 ,P_HCTN_SEQ IN HAZ_COMMD_TRADE_NAMES.SEQ%TYPE
 ,P_HCCC_CD IN HAZ_COMMD_CHRIS_CLASS.CD%TYPE
 ,P_RMK IN HAZ_CGO_ITEMS.RMK%TYPE
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de insertar la información de LAST DANGEROUS
--                CARGO del formulario web Transit Booking Request en la tabla HAZ_CGO_ITEMS
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               30-oct-2003                Version Inicial.
--                                                    06-nov-2003               Incluir condicion para insertar registro de
--                                                                                          Customer Visit Details si el indicador de
--                                                                                          gas free del buque está activado.
--                                                                                          Se incluyeron como parámetros de entrada
--                                                                                          las otras columnas del registro de Hazardous_
--                                                                                          Cargo_Item.
--                                                     12-nov-2003              Inclusión de procedimiento de manejo de errores
--                                                     24-nov-2003             Eliminar parámetro e inserción a columna  --                                                                                          de P_ICSC_ID y P_CTD_ID
--=============================================================================


vn_long_tons                      haz_cgo_items.long_tons%TYPE;
vv_dcled_item_ind            haz_cgo_items.dcled_item_ind%TYPE := 'N';
vv_prel_item_ind               haz_cgo_items.prel_item_ind%TYPE := 'N';
vv_haz_cgo_typ                 haz_cgo_items.typ%TYPE := 'PHCI';
vn_cnt_decl_item              number(8);
vn_cvd_seq                        cust_visit_dets.seq%TYPE;
vd_cvd_date                       DATE;
vv_safe_ent_ind                cust_visit_dets.safe_ent_ind%TYPE;
vv_safe_hot_work            cust_visit_dets.safe_hot_work_ind%TYPE;
BEGIN


    /* Desactivated until we can clarify if this validation
       is needed
       Activated on November 5, 2003 -- GNV
       We are going to validate if exists a previous last cargo
       information defined, if not, then we will insert it from
       Transit Booking Request */

    BEGIN

      SELECT count(*)
        INTO vn_cnt_decl_item
        FROM haz_cgo_items hci
       WHERE hci.csn_seq = p_csn_seq
             AND hci.typ = vv_haz_cgo_typ;

    EXCEPTION
       WHEN OTHERS THEN

          vn_cnt_decl_item := NULL;

    END;

    IF vn_cnt_decl_item IS NOT NULL AND
       NOT(vn_cnt_decl_item > 0)    THEN

       vn_long_tons := to_number(sf_conv_mt_eng('L',nvl(p_metric_tons,0),4));

       BEGIN

          SELECT MAX(date_created)
            INTO vd_cvd_date
            FROM cust_visit_dets
           WHERE csn_seq = p_csn_seq;

          BEGIN

             SELECT seq
               INTO vn_cvd_seq
               FROM cust_visit_dets
              WHERE date_created = vd_cvd_date
                AND csn_seq = p_csn_seq;

          EXCEPTION
             WHEN OTHERS THEN

                 vn_cvd_seq := NULL;

          END;

          IF vn_cvd_seq IS NULL THEN

              IF p_gas_free_ind = 'Y' THEN

                    vv_safe_ent_ind := 'Y';
		            vv_safe_hot_work := 'Y';

                    INSERT INTO cust_visit_dets(seq,
                                                csn_seq,
                                                typ,
                                                obcd_full_teu_abov,
                                                obcd_empty_teu_abov,
                                                obcd_full_teu_blow,
                                                obcd_empty_teu_blow,
                                                obcd_reefer_teu_abov,
                                                obcd_reefer_teu_blow,
                                                obcd_cont_size,
                                                obcd_full_ton_abov,
                                                obcd_empty_ton_abov,
                                                obcd_reefer_ton_abov,
                                                obcd_full_ton_blow,
                                                obcd_empty_ton_blow,
                                                obcd_reefer_ton_blow,
                                                tblc_in_bala_ind,
                                                tblc_inrtd_ind,
                                                tblc_gas_free_ind,
                                                tblc_rmk,
                                                safe_ent_ind,
                                                safe_hot_work_ind,
                                                telex_date)
                    VALUES(cvd_seq.nextval,
                           p_csn_seq,
                           'TBLC',
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           'N',
                           'N',
                           p_gas_free_ind,
                           NULL,
                           vv_safe_ent_ind,
                           vv_safe_hot_work,
                           NULL)
                   RETURNING SEQ INTO vn_cvd_seq;

              END IF;

          END IF;

       EXCEPTION
          WHEN OTHERS THEN

                IF p_gas_free_ind = 'Y' THEN

                     vv_safe_ent_ind := 'Y';
		             vv_safe_hot_work := 'Y';

                     INSERT INTO cust_visit_dets(seq,
                                                 csn_seq,
                                                 typ,
                                                 obcd_full_teu_abov,
                                                 obcd_empty_teu_abov,
                                                 obcd_full_teu_blow,
                                                 obcd_empty_teu_blow,
                                                 obcd_reefer_teu_abov,
                                                 obcd_reefer_teu_blow,
                                                 obcd_cont_size,
                                                 obcd_full_ton_abov,
                                                 obcd_empty_ton_abov,
                                                 obcd_reefer_ton_abov,
                                                 obcd_full_ton_blow,
                                                 obcd_empty_ton_blow,
                                                 obcd_reefer_ton_blow,
                                                 tblc_in_bala_ind,
                                                 tblc_inrtd_ind,
                                                 tblc_gas_free_ind,
                                                 tblc_rmk,
                                                 safe_ent_ind,
                                                 safe_hot_work_ind,
                                                 telex_date)
                     VALUES(cvd_seq.nextval,
                            p_csn_seq,
                            'TBLC',
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            'N',
                            'N',
                            p_gas_free_ind,
                            NULL,
                            vv_safe_ent_ind,
                            vv_safe_hot_work,
                            NULL)
                     RETURNING SEQ INTO vn_cvd_seq;

                END IF;

       END;

       IF vn_cvd_seq IS NOT NULL THEN

          INSERT INTO haz_cgo_items(seq,
                                    dcled_item_ind,
                                    typ,
                                    strge_typ,
                                    prel_item_ind,
                                    csn_seq,
                                    hca_seq,
                                    hcuc_un_no,
                                    hcid_class_no,
                                    hcid_div_no,
                                    long_tons,
                                    metric_tons,
                                    ad_seq,
                                    cvd_seq,
                                    hctn_seq,
                                    hccc_cd,
                                    psn_seq,
                                    rmk)
          VALUES(hci_seq.nextval,
                 vv_dcled_item_ind,
                 vv_haz_cgo_typ,
                 p_strge_typ,
                 vv_prel_item_ind,
                 p_csn_seq,
                 p_hca_seq,
                 p_hcuc_un_no,
                 p_hcid_class_no,
                 p_hcid_div_no,
                 vn_long_tons,
                 p_metric_tons,
                 p_ad_seq,
                 vn_cvd_seq,
                 p_hctn_seq,
                 p_hccc_cd,
                 p_psn_seq,
                 p_rmk);

       END IF;

    END IF;

EXCEPTION
WHEN OTHERS THEN

     --R3430: Changed on November 12, 2003 -- GNV
     --    RAISE_APPLICATION_ERROR(-20000,'Insert of Last Dangerous Cargo Failed');

     pkg_evtms_db_util.p_error('USR-01750','Insert of Last Dangerous Cargo Failed');


END;
PROCEDURE P_UPD_BOOK_RQSTS
 (P_WBR_SEQ IN NUMBER
 ,P_BR_SEQ IN NUMBER
 ,P_REG_BK_FEE IN NUMBER
 ,P_BK_PERS IN NUMBER
 ,P_PC_UMS_NET IN NUMBER
 ,P_REST IN REST_USR_CDS.CD%TYPE
 ,P_HML IN CUST_VSL_CHARS.LVC_HML_QUALIFIED%TYPE
 ,P_Y_JOB_RMK IN VARCHAR2
 ,P_REQ_ARR_TIME IN VARCHAR2
 ,P_REASON_FOR_REJ IN VARCHAR2
 ,P_RQST_STATUS IN VARCHAR2
 ,P_STAT_DATE IN DATE
 ,P_TRANS_STAT IN VARCHAR2
 ,P_BK_COND IN BK_CONDS.LEV%TYPE
 ,P_ACP_REP_APP IN WEB_BK_REQUESTS.ACP_REP_APP_OFF%TYPE
 ,P_SUCCESS OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de actualizar en la tabla de solicitudes de booking para
--                EDCS los valores de los fee y el booking period calculado en la solicitud de booking
--                procesada .
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador    	Fecha		Cambios realizados
-- ----------- ------------		--------   	----------		-----------------------------------------
-- R3430    GVillarreal         25-nov-2003	Version Inicial
--								04-dec-2003	Agregar parámetros p_stat_date y p_trans_stat
--								11-dec-2003	Eliminar parámetros P_PC_GROSS,
--                                     		P_PC_UMS_DECK, P_PC_UMS_PRE_FEE y
--                                     		columnas respectivas de la instrucción UPDATE
--								17-dec-2003	Cambiar nombre de tabla WEB_BK_RQSTS a
--                                          tabla WEB_BK_REQUESTS
--                             	14-jan-2004	Incluir actualización de campos de restriction y hml.
-- R5127 	VJaen		  		25-Abr-2005	Manejo de Toll Basis Info.
--=============================================================================
	--
	vv_toll_basis_info VARCHAR2(32000);
BEGIN

         pkg_evtms_db_util.p_lock_table_rows('WEB_BK_REQUESTS',FALSE,'SEQ = :1',p_wbr_seq);

	 	 --
	 	 -- Busca información de las tarifas aplicables a la reservación.
	 	 vv_toll_basis_info := SUBSTR(f_get_toll_basis_info(p_br_seq, SF_GET_PARAM_VAL(NULL, 'TBR_TBI_EDCS_S')), 1, 50);
	     --

         UPDATE vw_wb_book_rqsts
           SET reason_for_rej   = p_reason_for_rej,
               reg_bk_fee       = p_reg_bk_fee,
               br_seq           = p_br_seq,
               bk_pers          = p_bk_pers,
               restriction      = p_rest,
               hml              = p_hml,
               y_job_rmk        = p_y_job_rmk,
               pc_ums_net       = p_pc_ums_net,
               req_arr_time     = p_req_arr_time,
               rqst_status      = p_rqst_status,
               stat_date        = p_stat_date,
               trans_stat       = p_trans_stat,
               bk_cond          = p_bk_cond,
               acp_rep_app_off  = p_acp_rep_app,
			   toll_basis_info  = vv_toll_basis_info
         WHERE wbr_seq = p_wbr_seq;

         IF SQL%FOUND THEN

            p_success := 'TRUE';

         ELSE

            p_success := 'FALSE';

            --S15142.T15: Added on August 08, 2007 -- GNV
            --Do rollback transaction for booking transactions,
            --specially Swap, Substitution, Change Date
            ROLLBACK;

         END IF;

         --S15142.T15: Desactivated on August 08, 2007 -- GNV
         --Due to rollback transaction
         --The commit will be handle in the Web Booking Request
         --Module (VI5060FM)
         --commit;

EXCEPTION

    WHEN OTHERS THEN

       p_success := 'FALSE';

       --S15142.T15: Added on August 08, 2007 -- GNV
       --Do rollback transaction for booking transactions,
       --specially Swap, Substitution, Change Date
       ROLLBACK;

       pkg_evtms_db_util.p_error(sqlcode,sqlerrm);

END P_UPD_BOOK_RQSTS;
PROCEDURE P_UPD_BOOK_TRANS
 (P_WBT_SEQ IN NUMBER
 ,P_BR_SEQ IN NUMBER
 ,P_BR_CANCL_DAY IN NUMBER
 ,P_BR_CANCL_HR IN NUMBER
 ,P_BR_CANCL_FEE IN NUMBER
 ,P_REASON_FOR_REJ IN VARCHAR2
 ,P_DAYLIGHT_TRN_CAN_FEE IN NUMBER
 ,P_RQST_STATUS IN VARCHAR2
 ,P_STAT_DATE IN DATE
 ,P_TRANS_STAT IN VARCHAR2
 ,P_ACP_REP_APP IN WEB_BK_REQUESTS.ACP_REP_APP_OFF%TYPE
 ,P_SUCCESS OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de actualizar en la tabla de solicitudes de booking
--                cancellations para
--                EDCS los valores de los fee y el booking period calculado en la solicitud de booking
--                procesada .
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               25-nov-2003               Version Inicial
--                                                   04-dec-2003                Agregar parámetros p_stat_date y p_trans_stat
--                                                   17-dec-2003                Cambiar nombre de tabla de WEB_BK_CANCLS
--                                                                                           a WEB_BK_RQST_TRANS
--=============================================================================
BEGIN

    pkg_evtms_db_util.p_lock_table_rows('WEB_BK_RQST_TRANS',FALSE,'SEQ = :1',p_wbt_seq);

    UPDATE vw_wb_book_trans
           SET reason_for_rej   = p_reason_for_rej,
               --br_seq           = p_br_seq,
               br_cancl_dy      = p_br_cancl_day,
               br_cancl_hr      = p_br_cancl_hr,
               br_cancl_fee     = p_br_cancl_fee,
               daylight_trn_cancl_fee = p_daylight_trn_can_fee,
               rqst_status      = p_rqst_status,
               acp_rep_app_off  = p_acp_rep_app,
               stat_date        = p_stat_date,
               trans_stat       = p_trans_stat
         WHERE wbt_seq = p_wbt_seq;

         IF SQL%FOUND THEN

            p_success := 'TRUE';

         ELSE

            p_success := 'FALSE';

            --S15142.T15: Added on August 08, 2007 -- GNV
            --Do rollback transaction for booking transactions,
            --specially Swap, Substitution, Change Date
            ROLLBACK;

         END IF;

         --S15142.T15: Desactivated on August 08, 2007 -- GNV
         --Due to rollback transaction
         --The commit will be handle in the Web Booking Request
         --Module (VI5060FM)
         --commit;

EXCEPTION

    WHEN OTHERS THEN

       p_success := 'FALSE';

       --S15142.T15: Added on August 08, 2007 -- GNV
       --Do rollback transaction for booking transactions,
       --specially Swap, Substitution, Change Date
       ROLLBACK;

       pkg_evtms_db_util.p_error(sqlcode,sqlerrm);

END;
/* Accesar la cantidad de slots de booking disponibles */
FUNCTION F_COUNT_SLOT_AVAIL
 (P_DATE_FROM IN BK_DAYS.DATE_FROM%TYPE
 ,P_CLASS IN VARCHAR2
 ,P_BK_TYP IN BK_RQSTS.TYP%TYPE
 ,P_RQST_DATE IN BK_RQSTS.RQST_DATE%TYPE
 ,P_EVAL_PREV_DATE IN VARCHAR2 := 'Y'
 )
 RETURN NUMBER
 IS
--============================================================================
-- DESCRIPCION:
-- 	Esta función se encarga de calcular la cantidad de slots disponibles para reservacion
--                en un periodo.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               22-oct-2003               Version Inicial transportada desde la
--                                                                                          función F_COUNT_SLOT_AVAIL
--                                                                                          de la pantalla VI5060FM (Process
--                                                                                          Booking  Request

--=============================================================================

vn_slot_avail           number :=0;
vn_typ                       bk_rqsts.typ%type;
vn_per_id                 bk_pers.id%type;
vd_bk_date              bk_rqsts.rqst_bk_date%type := p_date_from;
vd_rqsts_date         bk_rqsts.rqst_date%type :=NVL(p_rqst_date,SYSDATE);
vv_eval_prev_date varchar2(1)                 := 'Y';
BEGIN
  --
  vv_eval_prev_date := Nvl(P_eval_prev_date,'Y');
  --
  IF vv_eval_prev_date = 'Y' THEN
     --
     -- Selecting the booking period
     IF TO_CHAR(vd_rqsts_date,'HH24MI') < '0900' THEN -- before was '0914' mrxp-tnv apr/10/2000
        --
        vd_rqsts_date := vd_rqsts_date - 1 ;
     ELSE
        --
        vd_rqsts_date := vd_rqsts_date;
        --
     END IF;
     --
  END IF;
  --
  BEGIN
    --
    SELECT id
    INTO   vn_per_id
    FROM   bk_pers
    WHERE  NVL(TRUNC(vd_bk_date)-trunc(vd_rqsts_date),0) BETWEEN date_from AND date_to;
    --
    vn_typ:= p_bk_typ;
    --
    SELECT SUM(slots_ava)
      INTO vn_slot_avail
      FROM slot_availables
     WHERE typ   = vn_typ
       AND bp_id = vn_per_id
       AND class <> Pkg_Book_Datadict.f_bvtc_xpiece
       AND TRUNC(date_from)  = TRUNC(p_date_from)
       AND TRUNC(rqsts_date) = TRUNC(vd_rqsts_date);

    IF p_class = 'SLOT' THEN
       --
       RETURN vn_slot_avail;
       --
    ELSIF p_class = 'PER' THEN
       --
       RETURN vn_per_id;
       --
    END IF;
    --
  EXCEPTION
     WHEN NO_DATA_FOUND THEN
          --
          RETURN 0;
          --
  END;
  --
EXCEPTION
   WHEN OTHERS THEN
        --
        RETURN 0;
        --
END;
FUNCTION F_GET_BK_FEE
 (P_BS_SEQ IN BK_RQSTS.BS_SEQ%TYPE
 )
 RETURN NUMBER
 IS
--============================================================================
-- DESCRIPCION:
-- 	Esta función se encarga de calcular el cargo por reservación (booking fee).
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               22-oct-2003               Version Inicial transportada desde la
--                                                                                          función F_GET_BK_FEE
--                                                                                          de la pantalla VI5060FM (Process
--                                                                                          Booking  Request

--=============================================================================

vn_bk_fee                   NUMBER := 0;
BK_chrg_tab              Pkg_Toll_Charges.chrg_table_typ;
BEGIN

     BK_chrg_tab := Pkg_Toll_Charges.F_GEN_TOLL_CHARGES
                                    (p_bs_seq
                                     , NULL
                                     , NULL
                                     , NULL
                                     , NULL
                                     , NULL
                                     , NULL
                                     , NULL
                                     , NULL
                                     , NULL
                                     , NULL
                                     , NULL -- Est pcums
                                     , NULL
                                     , NULL
                                     , NULL
                                     , NULL
                                     , 'S'
                                     , sf_get_vsl_typ(sf_get_visit_no(p_bs_seq)));
                                     --, 'BOOKING' );
     --
     --
     FOR I IN 1..BK_chrg_tab.COUNT
     LOOP
         IF BK_chrg_tab(I).charge_typ = 'BOOKING' THEN

            vn_bk_fee := vn_bk_fee + BK_chrg_tab(I).AMOUNT;

            /* R3430: Desactivado en Octubre 22, 2003 -- GNV
                      para mejoras a la recolección de datos.
                      Este código sólo aplica a la pantalla

            IF BK_chrg_tab(I).qty<>1 THEN

               :rates.unit:=sf_get_ref_meaning(BK_chrg_tab(i).unit,'UOM');
               :rates.rate:= BK_chrg_tab(i).rate;
               :rates.qty:= BK_chrg_tab(i).qty;
               synchronize;

               IF i < BK_chrg_tab.count THEN

                  next_record;

               END IF;

            END IF;
            */

         END IF;

     END LOOP;
     --
     --
     RETURN(vn_bk_fee);

EXCEPTION
  WHEN OTHERS THEN

     RETURN(NULL);


END;
/* Get the arrival time parameter */
FUNCTION F_GET_ARR_PAR
 (P_BS_SEQ IN BK_RQSTS.BS_SEQ%TYPE
 ,P_EXTM_BEAM IN CUST_VSL_CHARS.EXTM_BEAM%TYPE
 ,P_HML IN CUST_VSL_CHARS.PERM_HL_IND%TYPE
 ,P_REST IN REST_USR_CDS.CD%TYPE
 )
 RETURN VARCHAR2
 IS
--============================================================================
-- DESCRIPCION:
-- 	Esta función se encarga de buscar el parámetro de hora de arrival time para la evaluación
--               de un booking request
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               22-oct-2003               Version Inicial transportada desde la
--                                                                                          función F_GET_ARR_PAR
--                                                                                          de la pantalla VI5060FM (Process
--                                                                                          Booking  Request

--=============================================================================

vv_req_arr        VARCHAR2(5);
vn_beam           CUST_VSL_CHARS.EXTM_BEAM%TYPE := p_extm_beam;
BEGIN

   vv_req_arr:= sf_get_arr_time_par(p_bs_seq);

   IF vv_req_arr IS NULL     AND
      vn_beam    IS NOT NULL THEN

      IF vn_beam >= sf_get_param_val(56,'BOOKING') THEN

       --IF p_hml IN ('M','D', 'N','O') THEN
         IF INSTR(PKG_BOOK_DATADICT.f_hml_rest_list, p_hml) > 0 THEN

             vv_req_arr:=  sf_get_param_val(12,'BOOKING');

          ELSE

             vv_req_arr:=  sf_get_param_val(14,'BOOKING');

          END IF;

      ELSE

          IF p_rest IS NOT NULL  THEN

              vv_req_arr:=  sf_get_param_val(16,'BOOKING');

          ELSE

              vv_req_arr:=  sf_get_param_val(18,'BOOKING');

          END IF;

      END IF;

   END IF;

   RETURN vv_req_arr;

EXCEPTION

   WHEN OTHERS THEN

      RETURN(NULL);

END;
/* Retrieve the agent sequence */
FUNCTION F_FIND_AGENT_SEQ
 (P_AGENT IN CONTACTS.SHORT_NAME%TYPE
 )
 RETURN INTEGER
 IS
--============================================================================
-- DESCRIPCION:
-- 	Esta función se encarga de traer la secuencia del registro de agente para el proceso
--                de booking
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               22-oct-2003               Version Inicial transportada desde la
--                                                                                          función F_FIND_AGENT_SEQ
--                                                                                          de la pantalla VI5060FM (Process
--                                                                                          Booking  Request

--=============================================================================

vn_agent_seq     NUMBER;
BEGIN

  SELECT crra.seq
    INTO vn_agent_seq
    FROM contacts con,
         ctac_role_resp_assgs crra
   WHERE crra.cr_cd     = 'AGEN'
     AND crra.rra_cr_cd = 'SUBBKR'
     AND crra.con_seq   = con.seq
     AND con.short_name = p_agent
     AND crra.active = 'Y';
   --
   RETURN vn_agent_seq;

EXCEPTION
  WHEN NO_DATA_FOUND THEN

    RETURN NULL;

  WHEN OTHERS THEN

    RETURN(NULL);

END;
PROCEDURE P_SAME_DAY_TRN
 (P_VISIT_SEQ IN WEB_BK_RQST_TRANS.VISIT_SEQ%TYPE
 ,P_SHIP_NO IN WEB_BK_RQST_TRANS.SHIP_NO%TYPE
 ,P_SHIP_NAME IN WEB_BK_RQST_TRANS.SHIP_NAME%TYPE
 ,P_AGENT IN WEB_BK_RQST_TRANS.AGENT%TYPE
 ,P_REASON_FOR_REJ IN WEB_BK_RQST_TRANS.REASON_FOR_REJ%TYPE
 ,P_BR_SEQ IN WEB_BK_RQST_TRANS.BR_SEQ%TYPE
 ,P_TRN_DIR IN WEB_BK_RQST_TRANS.TRN_DIR%TYPE
 ,P_ACP_REP_APP_OFF IN WEB_BK_RQST_TRANS.ACP_REP_APP_OFF%TYPE
 ,P_OPTION_RQST IN WEB_BK_RQST_TRANS.OPTION_RQST%TYPE
 ,P_BR_CANCL_DATE IN WEB_BK_RQST_TRANS.BR_CANCL_DATE%TYPE
 ,P_BR_RET_DATE IN WEB_BK_RQST_TRANS.BR_RET_DATE%TYPE
 ,P_BR_CANCL_DY IN WEB_BK_RQST_TRANS.BR_CANCL_DY%TYPE
 ,P_BR_CANCL_HR IN WEB_BK_RQST_TRANS.BR_CANCL_HR%TYPE
 ,P_BR_CANCL_FEE IN WEB_BK_RQST_TRANS.BR_CANCL_FEE%TYPE
 ,P_AGT_NAME IN WEB_BK_RQST_TRANS.AGT_NAME%TYPE
 ,P_TRN_BR_DATE IN WEB_BK_RQST_TRANS.TRN_BR_DATE%TYPE
 ,P_BK_DAYLIGHT_DATE IN WEB_BK_RQST_TRANS.BK_DAYLIGHT_DATE%TYPE
 ,P_BK_DAYLIGHT_CANCL_DATE IN WEB_BK_RQST_TRANS.BK_DAYLIGHT_CANCL_DATE%TYPE
 ,P_DAYLIGHT_CANCL_FEE IN WEB_BK_RQST_TRANS.DAYLIGHT_TRN_CANCL_FEE%TYPE
 ,P_RQST_STATUS IN WEB_BK_RQST_TRANS.RQST_STATUS%TYPE
 ,P_STAT_DATE IN WEB_BK_RQST_TRANS.STAT_DATE%TYPE
 ,P_TRANS_STAT IN WEB_BK_RQST_TRANS.TRANS_STAT%TYPE
 ,P_SUCCESS OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de insertar en la tabla de solicitudes de
--                transacciones sobre booking requests para   EDCS una solicitud de booking
--                same day transit .
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               19-dec-2003               Version Inicial
BEGIN

        INSERT INTO vw_wb_book_trans( visit_seq,
                                      ship_no,
                                      ship_name,
                                      agent,
                                      reason_for_rej,
                                      br_seq,
                                      trn_dir,
                                      acp_rep_app_off,
                                      option_rqst,
                                      br_cancl_date,
                                      br_ret_date,
                                      br_cancl_dy,
                                      br_cancl_hr,
                                      br_cancl_fee,
                                      agt_name,
                                      trn_br_date,
                                      bk_daylight_date,
                                      bk_daylight_cancl_date,
                                      daylight_trn_cancl_fee,
                                      rqst_status,
                                      stat_date,
                                      trans_stat)
        VALUES(p_visit_seq,
               p_ship_no,
               p_ship_name,
               p_agent,
               p_reason_for_rej,
               p_br_seq,
               p_trn_dir,
               p_acp_rep_app_off,
               p_option_rqst,
               p_br_cancl_date,
               p_br_ret_date,
               p_br_cancl_dy,
               p_br_cancl_hr,
               p_br_cancl_fee,
               p_agt_name,
               p_trn_br_date,
               p_bk_daylight_date,
               p_bk_daylight_cancl_date,
               p_daylight_cancl_fee,
               p_rqst_status,
               p_stat_date,
               p_trans_stat);


    commit;

    IF SQL%FOUND THEN

       p_success := 'TRUE';

    ELSE

       p_success := 'FALSE';

    END IF;

EXCEPTION

    WHEN OTHERS THEN

       p_success := 'FALSE';
       pkg_evtms_db_util.p_error(sqlcode,sqlerrm);

END;
PROCEDURE P_UPD_SAME_DAY_TRN
 (P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_PENALTY_IND IN BK_RQSTS.PENALTY_IND%TYPE
 ,P_SUCCESS OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de actualizar el penalty indicator para un same day
--                transit en la tabla de booking requests.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3430    GVillarreal               19-dec-2003               Version Inicial
--                 GVillarreal               20-jan-2004                Cambio de actualización en lugar de la vista
--                                                                                          VW_BOOKING_RQSTS a la tabla BK_RQSTS
--                                                                                          por validación de estatus no cambiado
--                                                                                          incluido en trigger BR_B_U_R_STAT.
-- R3746    GVillarreal              17-feb-2004                Desactivated commit for java programs
BEGIN

    /*
    pkg_evtms_db_util.p_lock_table_rows('BK_RQSTS',FALSE,'SEQ = :1',p_br_seq);

    UPDATE vw_booking_rqsts
       SET penalty_ind = p_penalty_ind
     WHERE br_seq = p_br_seq;
     */

    UPDATE bk_rqsts
       SET penalty_ind = p_penalty_ind
     WHERE seq = p_br_seq;


    IF SQL%FOUND THEN

       p_success := 'TRUE';

    ELSE

       p_success := 'FALSE';
       pkg_evtms_db_util.p_error('USR-01842','Update of PENALTY_IND on BK_RQSTS for SDT could not be completed');

    END IF;

    --commit;

EXCEPTION

    WHEN OTHERS THEN

       p_success := 'FALSE';
       pkg_evtms_db_util.p_error(sqlcode,sqlerrm);

END;
/* Process daylight transit requests */
PROCEDURE P_DAY_BOOKING
 (P_CSN_SEQ IN CUST_SCHED_NEEDS.SEQ%TYPE
 ,P_BR_SEQ IN BK_DLT_RQSTS.BR_SEQ%TYPE
 ,P_SHIP_NO IN SHIP_ID_NO.SHIP_NO%TYPE
 ,P_DT_RQST_DATE IN DATE
 ,P_EXTM_BEAM IN CUST_VSL_CHARS.EXTM_BEAM%TYPE
 ,P_HML IN CUST_VSL_CHARS.LVC_HML_QUALIFIED%TYPE
 ,P_REST IN REST_USR_CDS.CD%TYPE
 ,P_TRN_DIR IN ITIN_ITEMS.TII_TRN_DIR%TYPE
 ,P_HL_IND IN CUST_VSL_CHARS.PERM_HL_IND%TYPE
 ,P_AGENT IN CONTACTS.SHORT_NAME%TYPE
 ,P_DR_SEQ OUT BK_DLT_RQSTS.SEQ%TYPE
 ,P_DB_STAT OUT BK_DLT_RQSTS.STAT%TYPE
 ,P_ERROR_CODE OUT VARCHAR2
 ,P_SUCCESS OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de procesar los daylight transit booking requests
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3653    GVillarreal               06-FEB-2004               Version Inicial
-- R3746    GVillarreal               16-MAR-2004              Modificación de la validación del booking date
--                                                                                           contra el system date. Se desactivo la validación

CURSOR c_booking_info(p_book_seq IN BK_RQSTS.SEQ%TYPE) IS
      SELECT seq,
                      typ,
                      bp_id,
                      bd_seq,
                      rqst_bk_date,
                      rest_when_bk,
                      PKG_BOOK_REFTAB_UTIL.F_GET_BK_SIZE_CLASS(BSPS_SEQ)
         FROM  bk_rqsts
       WHERE seq = p_book_seq
             AND stat = 'BKD';

vn_seq                                 BK_RQSTS.SEQ%TYPE;
vv_typ                                   BK_RQSTS.TYP%TYPE;
vn_bp_id                              BK_RQSTS.BP_ID%TYPE;
vn_bd_seq                          BK_RQSTS.BD_SEQ%TYPE;
vd_rqst_bk_date               DATE;
vv_rest_when_bk             VARCHAR2(1);
vn_bdr_seq                        BK_DLT_RQSTS.SEQ%TYPE;
vn_bk_cond                       BK_CONDS.LEV%TYPE;
vv_stat                                BK_DLT_RQSTS.STAT%TYPE;
vv_stat_rmk                      BK_DLT_RQSTS.STAT_RMK%TYPE;
vd_stat_date                     DATE;
vv_srce                               BK_DLT_RQSTS.SRCE%TYPE;
vn_dl_exist_seq               BK_DLT_RQSTS.SEQ%TYPE;

--S61519
vv_class                              BK_SIZE_TYPS.CLASS%TYPE;

e_handline_vessel            EXCEPTION;
e_vessel_large                  EXCEPTION;
e_vessel_rest                    EXCEPTION;
e_daylight_exist	          EXCEPTION;
e_no_book_info                 EXCEPTION;
e_invalid_rqst_date          EXCEPTION;
e_invalid_booked_date    EXCEPTION;
e_small_dir_rest               EXCEPTION;
e_no_cond                          EXCEPTION;
BEGIN

   -- Validate if vessel is handline. Daylight transit are not allowed for handlines.
   IF p_hl_ind = 'Y' THEN

      raise e_handline_vessel;

   END IF;

   -- Daylight transit is not allowed for vessels with beam >= 91 ft.
   IF NVL(p_extm_beam,0) >= sf_get_param_val(56,'BOOKING') THEN

      raise e_vessel_large;

   END IF;

   -- Daylight transit is not allowed for restricted vessels
   --IF p_hml IN ('M','N','D','O') OR
   IF ( INSTR(PKG_BOOK_DATADICT.f_hml_rest_list, p_hml) > 0) OR
      ( p_rest IS NOT NULL)     THEN

      raise e_vessel_rest;

   END IF;

   -- Validate if exists a daylight transit booking in process
   BEGIN

       SELECT max(seq)
         INTO vn_dl_exist_seq
         FROM bk_dlt_rqsts
        WHERE br_seq = p_br_seq
          AND stat NOT IN ('DTCAN','DTREJ','VOID DTB');

       IF vn_dl_exist_seq IS NOT NULL THEN

          raise e_daylight_exist;

       END IF;

   END;

   -- Retrieve booking to associate daylight transit
   OPEN c_booking_info(p_br_seq);

   IF c_booking_info%ISOPEN THEN

      FETCH c_booking_info INTO vn_seq,
                                vv_typ,
                                vn_bp_id,
                                vn_bd_seq,
                                vd_rqst_bk_date,
                                vv_rest_when_bk,
                                vv_class;        --S61519

      IF c_booking_info%NOTFOUND THEN

         vn_seq := NULL;

      END IF;

      CLOSE c_booking_info;

   ELSE

      vn_seq := NULL;

   END IF;

   IF vn_seq IS NULL THEN

      raise e_no_book_info;

   END IF;

   BEGIN

      -- Validate booking date and requested date
      IF TRUNC(vd_rqst_bk_date) - TRUNC(p_dt_rqst_date) < 2 THEN

         raise e_invalid_rqst_date;

      END IF;

      -- Validate booking date with system's date
      /*R3746: Desactivated on March 16, 2004 -- GNV
      IF TRUNC(vd_rqst_bk_date) - TRUNC(sf_get_sys_date) <= 2 THEN

         RAISE e_invalid_booked_date;

      END IF;
      */

      IF vv_typ = PKG_BOOK_DATADICT.f_bk_typ_small THEN

         BEGIN

            SELECT bco_lev
              INTO vn_bk_cond
              FROM bk_day_cond_assgs bdca,
                   bk_days bd
             WHERE bdca.seq = bd.bdca_seq
               AND bd.seq   = vn_bd_seq;

         EXCEPTION
            WHEN OTHERS THEN

                vn_bk_cond := NULL;

         END;

         IF vn_bk_cond IS NOT NULL THEN

            -- Restricted Slots evaluation for vessels requesting daylight transit
            -- 60 or more days in advance
            IF TRUNC(vd_rqst_bk_date) - TRUNC(p_dt_rqst_date) >= TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN

               -- Restricted slots definition for condition 1
               IF vn_bk_cond = 1 THEN
                ------------------------------comentariado por R.4374---------------------
                 /* IF (NVL(sf_get_vessel_by_dir(2,vn_bd_seq,'N',vv_typ),0) +
                      NVL(sf_get_vessel_by_dir(2,vn_bd_seq,'S',vv_typ),0)) >=
                      NVL(to_number(sf_get_param_val(8,'BOOKING')),0)  THEN
                             raise e_small_dir_rest;
                   END IF;*/
                --------------------------------------------------------------------------
                --R.4374
                IF p_trn_dir = 'N' THEN
                    IF (vv_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(8,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                        NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,801),SF_GET_PARAM_VAL(801,'BOOKING'))
                        ) OR
                        (vv_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(11,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                         NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,4),0)
                        )
                        THEN
                           raise e_small_dir_rest;
                    END IF;
                ELSIF p_trn_dir = 'S' THEN
                    IF (vv_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(8,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                        NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,802),SF_GET_PARAM_VAL(802,'BOOKING'))
                        ) OR
                        (vv_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(11,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                         NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,10),0)
                        )
                        THEN
                            raise e_small_dir_rest;
                    END IF;
                END IF;
                --=========================================================================
               -- Restricted slots definition for condition 2
               ELSIF vn_bk_cond = 2 THEN
                  ------------------------------comentariado por R.4374---------------------
                  /*IF (NVL(sf_get_vessel_by_dir(2,vn_bd_seq,'N',vv_typ),0) +
                      NVL(sf_get_vessel_by_dir(2,vn_bd_seq,'S',vv_typ),0)) >=
                      NVL(to_number(sf_get_param_val(32,'BOOKING')),0)  THEN
                            raise e_small_dir_rest;
                    END IF;*/
                  --------------------------------------------------------------------------
                  --R.4374
                  IF p_trn_dir = 'N' THEN
                    IF (vv_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(8,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                        NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,321),SF_GET_PARAM_VAL(321,'BOOKING'))
                        ) OR
                        (vv_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(11,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                         NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,6),0)
                        )
                        THEN
                           raise e_small_dir_rest;
                    END IF;
                  ELSIF p_trn_dir = 'S' THEN
                    IF (vv_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(8,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                        NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,322),SF_GET_PARAM_VAL(322,'BOOKING'))
                        ) OR
                        (vv_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(11,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                         NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,8),0)
                        )
                        THEN
                           raise e_small_dir_rest;
                    END IF;
                  END IF;
                 --===============================================================================

               -- Restricted slots definition for condition 3
               ELSIF vn_bk_cond = 3 THEN
                   ------------------------------comentariado por R.4374---------------------
                  /*IF (NVL(sf_get_vessel_by_dir(2,vn_bd_seq,'N',vv_typ),0) +
                      NVL(sf_get_vessel_by_dir(2,vn_bd_seq,'S',vv_typ),0)) >=
                      NVL(to_number(sf_get_param_val(40,'BOOKING')),0)  THEN
                                raise e_small_dir_rest;
                    END IF;   */
                  ----------------------------------------------------------------------------
                  --R.4374
                  IF p_trn_dir = 'N' THEN
                    IF (vv_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(8,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                        NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,400),SF_GET_PARAM_VAL(400,'BOOKING'))
                        ) OR
                        (vv_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(11,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                         NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,18),0)
                        )
                        THEN
                           raise e_small_dir_rest;
                    END IF;
                  ELSIF p_trn_dir = 'S' THEN
                    IF (vv_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(8,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                        NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,401),SF_GET_PARAM_VAL(401,'BOOKING'))
                        ) OR
                        (vv_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND NVL(sf_get_vessel_by_dir(11,vn_bd_seq,p_trn_dir,vv_typ,vv_class),0) >=
                         NVL(SF_GET_BK_COND_PARAM(p_trn_dir,'Y',PKG_BOOK_DATADICT.f_bk_typ_small,vn_bk_cond,19),0)
                        )
                        THEN
                           raise e_small_dir_rest;
                    END IF;
                  END IF;
                 --===============================================================================

               END IF;

            END IF;

         ELSE

            raise e_no_cond;

         END IF;

         vv_stat := 'DTPEND';

      END IF;


   EXCEPTION

      WHEN e_invalid_rqst_date THEN

         vv_stat := 'DTREJ';
         vv_stat_rmk := sf_get_message_text('USR-01812');

      WHEN e_invalid_booked_date THEN

         vv_stat := 'DTREJ';
         vv_stat_rmk := sf_get_message_text('USR-01813');

      WHEN e_small_dir_rest THEN

         vv_stat := 'DTREJ';
         vv_stat_rmk := sf_get_message_text('USR-01814');

      WHEN e_no_cond THEN

         vv_stat := 'DTREJ';
         vv_stat_rmk := sf_get_message_text('USR-01815');
   END;

   SELECT bdr_seq.nextval
     INTO vn_bdr_seq
     FROM dual;

    vd_stat_date := sf_get_sys_date;
    vv_srce      := 'AGENT';

   INSERT INTO vw_day_book_rqsts(vdbr_seq,
                                 srce,
                                 stat,
                                 rqst_date,
                                 stat_date,
                                 rqst_cancl_date,
                                 stat_rmk,
                                 br_seq)
   VALUES(vn_bdr_seq,
          vv_srce,
          vv_stat,
          p_dt_rqst_date,
          vd_stat_date,
          NULL,
          vv_stat_rmk,
          p_br_seq);

   IF SQL%FOUND THEN

      p_dr_seq  := vn_bdr_seq;
      p_success := 'TRUE';
      p_db_stat := vv_stat;

   END IF;

EXCEPTION
   WHEN e_handline_vessel THEN

        p_success := 'FALSE';
        p_error_code := 'USR-01808';
        pkg_evtms_db_util.p_error('USR-01808','Daylight Transit Booking is not allowed for handline vessels');

   WHEN e_vessel_large THEN

        p_success := 'FALSE';
        p_error_code := 'USR-01809';
        pkg_evtms_db_util.p_error('USR-01809','Daylight Transit Booking is not allowed for restricted vessels');

   WHEN e_vessel_rest THEN

        p_success := 'FALSE';
        p_error_code := 'USR-01810';
        pkg_evtms_db_util.p_error('USR-01810','Daylight Transit Booking is not allowed for large vessels');

   WHEN e_daylight_exist THEN

        p_success := 'FALSE';
        p_error_code := 'USR-01838';
        pkg_evtms_db_util.p_error('USR-01838','Daylight Transit Booking exists in processing status');

   WHEN e_no_book_info THEN

        p_success := 'FALSE';
        p_error_code := 'USR-01811';
        pkg_evtms_db_util.p_error('USR-01811','Vessel does not have a valid booking information created for daylight transit request');

   WHEN OTHERS THEN

        p_success := 'FALSE';
        p_error_code := SQLCODE;
        pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

END;
PROCEDURE P_GUAR_DAY_BOOK
 (P_DAY_SEQ IN BK_DLT_RQSTS.SEQ%TYPE
 ,P_BR_SEQ IN BK_DLT_RQSTS.BR_SEQ%TYPE
 ,P_TEMP_STATUS OUT VARCHAR2
 ,P_ERROR_CODE OUT VARCHAR2
 ,P_SUCCESS OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de cambiar el status del daylight transit booking request
--                de PENDING a GUARANTEED evaluando la fecha de la solicitud.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3653    GVillarreal               06-FEB-2004               Version Inicial

CURSOR c_pend_dlt(p_day_seq IN BK_DLT_RQSTS.SEQ%TYPE,
                                        p_br_seq   IN  BK_DLT_RQSTS.BR_SEQ%TYPE) IS
SELECT vdbr_seq,
                stat,
                rqst_date
    FROM vw_day_book_rqsts
 WHERE vdbr_seq = p_day_seq
       AND br_seq = p_br_seq;


vn_dlt_seq                BK_DLT_RQSTS.SEQ%TYPE;
vv_stat                       BK_DLT_RQSTS.STAT%TYPE;
vv_stat_rmk             BK_DLT_RQSTS.STAT_RMK%TYPE;
vd_rqst_date            DATE;
vd_bk_date               DATE;
vv_sqlerrm               VARCHAR2(2000);
e_no_dl_book          EXCEPTION;
e_no_book_info      EXCEPTION;
e_no_pend_dl          EXCEPTION;
e_not_dl_upd           EXCEPTION;
e_pend_book           EXCEPTION;
BEGIN

  OPEN c_pend_dlt(p_day_seq,p_br_seq);

  IF c_pend_dlt%ISOPEN THEN

     FETCH c_pend_dlt INTO vn_dlt_seq,
                           vv_stat,
                           vd_rqst_date;

     IF c_pend_dlt%NOTFOUND THEN

        vn_dlt_seq := NULL;
        vv_stat := NULL;
        vd_rqst_date := NULL;

     END IF;

     CLOSE c_pend_dlt;

  ELSE

     vn_dlt_seq := NULL;
     vv_stat := NULL;
     vd_rqst_date := NULL;

  END IF;

  IF vn_dlt_seq IS NULL THEN

     raise e_no_dl_book;

  END IF;

  IF vv_stat != 'DTPEND' THEN

     raise e_no_pend_dl;

  END IF;

  BEGIN

    SELECT rqst_bk_date
      INTO vd_bk_date
      FROM vw_booking_rqsts
     WHERE br_seq = p_br_seq
       AND stat NOT IN ('CAN','REJ','VOID BKD');

  EXCEPTION
    WHEN OTHERS THEN

       raise e_no_book_info;

  END;

  IF TRUNC(vd_bk_date) - TRUNC(vd_rqst_date) >= TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN

     vv_stat     := 'DTGUAR';
     vv_stat_rmk := sf_get_message_text('USR-01822');

     pkg_evtms_db_util.p_lock_table_rows('BK_DLT_RQSTS',FALSE,'SEQ = :1 AND BR_SEQ = :2',p_day_seq,p_br_seq);

     UPDATE vw_day_book_rqsts
        SET stat      = vv_stat,
            stat_date = sf_get_sys_date,
            stat_rmk  = vv_stat_rmk
      WHERE vdbr_seq    = p_day_seq
        AND br_seq = p_br_seq;

     IF SQL%FOUND THEN

        p_success := 'TRUE';
        p_temp_status := 'APPRV';

        --S15142.T15: Desactivated on August 08, 2007 -- GNV
        --Due to rollback transaction
        --The commit will be handle in the Web Booking Request
        --Module (VI5060FM)
        --commit;

     ELSE

        raise e_not_dl_upd;

     END IF;

  ELSE

     raise e_pend_book;

  END IF;

EXCEPTION
   WHEN e_no_dl_book THEN

        p_success := 'FALSE';
        p_error_code := 'USR-01816';
        pkg_evtms_db_util.p_error('USR-01816','Daylight Transit Booking information is not created');

   WHEN e_no_pend_dl THEN

        p_success := 'FALSE';
        p_error_code := 'USR-01817';
        pkg_evtms_db_util.p_error('USR-01817','Daylight Transit Booking is not in PENDING status');

   WHEN e_no_book_info THEN

        p_success := 'FALSE';
        p_error_code := 'USR-01811';
        pkg_evtms_db_util.p_error('USR-01811','Vessel does not have a valid booking information created for daylight transit request');

   WHEN e_not_dl_upd THEN

        p_success := 'FALSE';
        p_error_code := 'USR-01818';
        pkg_evtms_db_util.p_error('USR-01818','Update iof Daylight Transit Booking Status could not be completed');

        --S15142.T15: Added on August 08, 2007 -- GNV
        --Do rollback transaction for booking transactions,
        --Avoid the daylight request created if has to be
        --guaranteed and it is not possible to change status

        ROLLBACK;

   WHEN e_pend_book THEN

        p_success := 'FALSE';
        p_temp_status := 'PEND';
        p_error_code := 'USR-01819';
        --pkg_evtms_db_util.p_error('USR-01819','Daylight Transit Booking was requested with less than required days in advance. It will remain in PENDING status until approval');

   WHEN OTHERS THEN

        p_success := 'FALSE';
        p_error_code := SQLCODE;
        pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

        --S15142.T15: Added on August 08, 2007 -- GNV
        --Do rollback transaction for booking transactions,
        --Avoid the daylight request created if has to be
        --guaranteed and it is not possible to change status

        ROLLBACK;

END;
PROCEDURE P_DAY_CAN_BOOK
 (P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_DLT_STATUS IN BK_DLT_RQSTS.STAT%TYPE
 ,P_DLT_CAN_DATE IN DATE
 ,P_DAY_SEQ OUT BK_DLT_RQSTS.SEQ%TYPE
 ,P_TEMP_STATUS OUT BK_RQSTS.STAT%TYPE
 ,P_ERROR OUT VARCHAR2
 ,P_SUCCESS OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de cancelar los daylight transit booking requests hechos --                por un buque.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3653    GVillarreal               07-feb-2004               Version Inicial
--=============================================================================

CURSOR c_dlt_date(p_date_created IN BK_DLT_RQSTS.DATE_CREATED%TYPE,
                                                  p_br_seq IN BK_DLT_RQSTS.BR_SEQ%TYPE) IS
SELECT vdbr_seq,
                stat
    FROM vw_day_book_rqsts
 WHERE date_created  = p_date_created
       AND br_seq = p_br_seq;

vd_dlt_date            DATE;
vn_dlt_seq             BK_DLT_RQSTS.SEQ%TYPE;
vv_dlt_stat             BK_DLT_RQSTS.STAT%TYPE;
vv_sqlerrm             VARCHAR2(2000);
e_not_booked       EXCEPTION;
BEGIN

   IF p_br_seq  IS NOT NULL THEN

         --
         IF p_dlt_status = 'PEND' THEN

                BEGIN

                     BEGIN

                       SELECT max(date_created)
                         INTO vd_dlt_date
                         FROM vw_day_book_rqsts
                        WHERE br_seq = p_br_seq
                          AND stat IN ('DTPEND','DTGUAR');

                     EXCEPTION
                        WHEN OTHERS THEN

                           raise e_not_booked;

                     END;

                     IF vd_dlt_date IS NULL THEN

                        raise e_not_booked;

                     END IF;

                     OPEN c_dlt_date(vd_dlt_date,p_br_seq);

                     IF c_dlt_date%ISOPEN THEN

                        FETCH c_dlt_date INTO vn_dlt_seq,
                                              vv_dlt_stat;

                        IF c_dlt_date%NOTFOUND THEN

                           vn_dlt_seq := NULL;
                           vv_dlt_stat := NULL;

                        END IF;

                     ELSE

                        vn_dlt_seq := NULL;
                        vv_dlt_stat := NULL;

                     END IF;

                     IF vn_dlt_seq IS NULL THEN

                        raise e_not_booked;

                     END IF;

                     pkg_evtms_db_util.p_lock_table_rows('BK_DLT_RQSTS',FALSE,'SEQ = :1 AND BR_SEQ = :2 AND STAT = :3',p_day_seq,p_br_seq,vv_dlt_stat);

                     UPDATE vw_day_book_rqsts
                       SET   stat = 'DTCAN'
                           , stat_rmk  = sf_get_message_text('USR-01820')
                           , stat_date = sf_get_sys_date
                           , rqst_cancl_date = p_dlt_can_date
                     WHERE vdbr_seq = vn_dlt_seq
                       AND br_seq = p_br_seq
                       AND stat = vv_dlt_stat;


                    --
                    -- No hay estatus PEND o GUAR para cancelar
                    IF SQL%NOTFOUND THEN

                         RAISE e_not_booked;

                    ELSE

                        p_success := 'TRUE';
                        p_day_seq := vn_dlt_seq;
					    p_temp_status := 'APPRV';

                    END IF;



               EXCEPTION
                   WHEN e_not_booked THEN

                        p_success := 'FALSE';
                        p_error := 'USR-01821';
                        pkg_evtms_db_util.p_error('USR-01821','This daylight transit request is not in PENDING or GUARANTEED status allowed to CANCEL the request');

                   WHEN OTHERS THEN

                        vv_sqlerrm := SQLERRM;

                        IF INSTR(vv_SQLERRM, 'USR-') > 0 THEN

                           p_success := 'FALSE';
                           p_error   := SQLCODE;
                           pkg_evtms_db_util.p_error(SUBSTR(vv_SQLERRM, INSTR(vv_SQLERRM, 'USR-'), 9),'Error');

                        ELSE

                           IF INSTR(vv_SQLERRM, 'USR-') = 0 THEN

                              p_success := 'FALSE';
                              p_error   := SQLCODE;
                              pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);


                           ELSE

                              p_success := 'FALSE';
                              p_error   := SQLCODE;
                              pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

                           END IF;

                        END IF;

                END;

         ELSE

                p_success := 'FALSE';
                p_error   := 'USR-30114';
                pkg_evtms_db_util.p_error('USR-30114','This request was already processed');

         END IF;

   END IF;

EXCEPTION
   WHEN OTHERS THEN

        p_success := 'FALSE';
        p_error   := SQLCODE;
        pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);

END;
/* Calculate booking fee, period, arrival time for booking requests */
PROCEDURE P_RTV_DL_BOOK_FEE
 (P_DAY_SEQ IN BK_DLT_RQSTS.SEQ%TYPE
 ,P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_RQST_BK_DATE IN BK_RQSTS.RQST_BK_DATE%TYPE
 ,P_DL_FEE OUT CHARGES.CHRG_AMT%TYPE
 ,P_SLOT_AVAIL OUT NUMBER
 ,P_ARR_TIME OUT VARCHAR2
 ,P_REJ_REASON OUT BK_RQSTS.STAT_RMK%TYPE
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de calcular el daylight transit cancellation  fee,
--                cancellation days,  rejected reason para un daylight transit booking request hecho.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3653    GVillarreal               07-feb-2004               Version Inicial
--                                                   22-mar-2004              Inclusión de cambios con respecto al cálculo
--                                                                                         de los cargos de reservación de daylight transit
-- R3907   GVillarreal               31-mar-2004              Activación de llamado a función de cargos
--
--=============================================================================
CURSOR c_get_dl_bk_rqst(p_day_seq  IN  bk_dlt_rqsts.seq%TYPE,
                                                    p_br_seq   IN  bk_rqsts.seq%TYPE) IS
SELECT vdbr_seq,
                stat,
                stat_rmk,
                rqst_date
   FROM  vw_day_book_rqsts
 WHERE vdbr_seq = p_day_seq
       AND br_seq = p_br_seq;
       --AND stat IN ('DTPEND','DTGUAR');


vn_seq                             bk_rqsts.seq%TYPE;
vv_stat                             bk_rqsts.stat%TYPE;
vv_stat_rmk                   bk_rqsts.stat_rmk%TYPE;
vd_rqst_date                  bk_rqsts.rqst_date%TYPE;

--R3653: Added on March 22, 2004 -- GNV
vn_bs_seq                      bk_rqsts.bs_seq%TYPE;
vr_charge_rec                pkg_mb621000pr.rate_record_type;
BEGIN

    OPEN c_get_dl_bk_rqst(p_day_seq,p_br_seq);

    IF c_get_dl_bk_rqst%ISOPEN THEN

       FETCH c_get_dl_bk_rqst INTO  vn_seq,
                                    vv_stat,
                                    vv_stat_rmk,
                                    vd_rqst_date;

       IF c_get_dl_bk_rqst%NOTFOUND THEN

          vn_seq          := NULL;
          vv_stat         := NULL;
          vv_stat_rmk     := NULL;
          vd_rqst_date    := NULL;

       END IF;

       CLOSE c_get_dl_bk_rqst;

    END IF;

    IF vv_stat IN('DTPEND','DTGUAR') THEN

        /*
        IF TRUNC(p_rqst_bk_date) - TRUNC(vd_rqst_date) >= TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN

           p_dl_fee := TO_NUMBER(sf_get_param_val(201,'BOOKING'));

        ELSE

           p_dl_fee := TO_NUMBER(sf_get_param_val(202,'BOOKING'));

        END IF;
        */

        --R3653: Added on March 22, 2004 -- GNV
        /*R3653 -- Desactivated on March 23, 2004 -- GNV
          R3907 -- Activated on March 31, 2004 -- GNV*/

        BEGIN

           SELECT bs_seq
             INTO vn_bs_seq
             FROM bk_rqsts
            WHERE seq = p_br_seq;

        EXCEPTION
        WHEN OTHERS THEN

           vn_bs_seq := NULL;

        END;

        --28AUG15 CMartin SR107552 Adecuaciones por canal ampliado
        --28AUG15         SR132271 Adecuaciones por entrada de nuevo aplicativo Booking
        -- SF_GET_TRANSIT_DATE  debe reemplazarse por SF_GET_ACTUAL_TRANSIT_DATE
        vr_charge_rec := pkg_mb621000pr.f_get_bdt_charge(nvl(SF_GET_ACTUAL_TRANSIT_DATE(vn_bs_seq),p_rqst_bk_date),
                                                            p_rqst_bk_date,
                                                            vd_rqst_date);

        p_dl_fee := vr_charge_rec.amount;

        --p_slot_avail := f_count_slot_avail(TRUNC(vd_rqst_bk_date), 'SLOT', vv_typ, vd_rqst_date);

        IF NVL(vv_stat,' ') = 'DTGUAR' THEN

           p_arr_time   := sf_get_param_val(16,'BOOKING');

        ELSE

           p_arr_time   := NULL;

        END IF;

    ELSIF  vv_stat = 'DTREJ' THEN

        p_rej_reason := vv_stat_rmk;

    END IF;

EXCEPTION
WHEN OTHERS THEN

    pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);
    p_dl_fee := NULL;

END;
/* Retrieve cancellation fees for booking cancellation */
PROCEDURE P_RTV_DL_CAN_FEE
 (P_DAY_SEQ IN BK_DLT_RQSTS.SEQ%TYPE
 ,P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_DL_CAN_DATE IN DATE
 ,P_RQST_BK_DATE IN DATE
 ,P_CAN_DAYS OUT NUMBER
 ,P_CAN_HOURS OUT NUMBER
 ,P_DAYLIGHT_TRN_CAN_FEE OUT NUMBER
 )
 IS
--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de calcular el cancellation fee, cancellation days and
--               hours  para un dayight transit booking request hecho.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador         Fecha                         Cambios realizados
-- ----------- --------------------         ----------                       -----------------------------------------
-- R3653    GVillarreal               07-feb-2004               Version Inicial
--                                                   22-mar-2004              Inclusión de cambios con respecto al cálculo
--                                                                                         de los cargos de reservación de daylight transit
-- R3907   GVillarreal               31-mar-2004              Activación de llamado a función de cargos
--=============================================================================


CURSOR c_get_dl_bk_rqst(p_day_seq  IN  bk_dlt_rqsts.seq%TYPE,
                                                    p_br_seq   IN  bk_dlt_rqsts.seq%TYPE) IS
SELECT vdbr_seq,
                stat,
                stat_rmk,
                rqst_date,
                stat_date  /*R3653/3907: Added on March 31, 2004 -- GNV*/
   FROM  vw_day_book_rqsts
 WHERE vdbr_seq = p_day_seq
       AND br_seq = p_br_seq;
       --AND stat IN ('DTCAN');


vn_seq                             bk_dlt_rqsts.seq%TYPE;
vv_stat                             bk_dlt_rqsts.stat%TYPE;
vv_stat_rmk                   bk_dlt_rqsts.stat_rmk%TYPE;
vd_rqst_date                  bk_dlt_rqsts.rqst_date%TYPE;
vd_can_date                  DATE;
vd_bk_date                    DATE;
vn_days                           NUMBER;
vv_last_status              bk_dlt_rqsts.stat%TYPE;

--R3653: Added on March 22, 2004 -- GNV
vn_bs_seq                      bk_rqsts.bs_seq%TYPE;
vr_charge_rec                pkg_mb621000pr.rate_record_type;
vd_stat_date                 DATE;
BEGIN

    OPEN c_get_dl_bk_rqst(p_day_seq,p_br_seq);

    IF c_get_dl_bk_rqst%ISOPEN THEN

       FETCH c_get_dl_bk_rqst INTO  vn_seq,
                                    vv_stat,
                                    vv_stat_rmk,
                                    vd_rqst_date,
                                    /*R3653/3907 Added on March 31, 2004 -- GNV*/
                                    vd_stat_date;

       IF c_get_dl_bk_rqst%NOTFOUND THEN

          vn_seq          := NULL;
          vv_stat         := NULL;
          vv_stat_rmk     := NULL;
          vd_rqst_date    := NULL;
          /*R3653/3907 Added on March 31, 2004 -- GNv*/
          vd_stat_date    := NULL;

       END IF;

       CLOSE c_get_dl_bk_rqst;

    END IF;

    BEGIN

        SELECT stat
          INTO vv_last_status
          FROM bk_dlt_stats
         WHERE hist_seq =(SELECT MAX(status1.hist_seq)
                            FROM bk_dlt_stats status1
                           WHERE status1.bdr_seq = vn_seq
                              AND status1.hist_seq IN (SELECT MAX(status.hist_seq)
                                                         FROM bk_dlt_stats status
                                                        WHERE status.bdr_seq= vn_seq
                                                          AND status.stat IN ('DTPEND','DTGUAR')));

    EXCEPTION
       WHEN NO_DATA_FOUND THEN

           vv_last_status := NULL;

    END;

    /*R3653/3907 Added on March 31, 2004 -- GNV*/
    IF nvl(vv_stat,' ') <>  'DTCAN' THEN

       SELECT sysdate
         INTO vd_stat_date
         FROM dual;

    END IF;

    vd_bk_date := TO_DATE(TO_CHAR(p_rqst_bk_date,'DD-MON-YYYY ')||sf_get_param_val(16,'BOOKING'),'DD-MON-YYYY HH24MI');

    vn_days := ABS(vd_bk_date - p_dl_can_date);

    p_can_days := TRUNC(vn_days);

    p_can_hours := TRUNC(ABS(vn_days - trunc(vn_days))*24);

    /*R3907 March 31, 2004 -- GNV --Desactivated for charges calculation

    IF TRUNC(p_rqst_bk_date) - TRUNC(vd_rqst_date)  >= TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN

       IF TRUNC(p_rqst_bk_date) - TRUNC(p_dl_can_date) >= 31 AND
          TRUNC(p_rqst_bk_date) - TRUNC(p_dl_can_date) <= TO_NUMBER(sf_get_param_val(200,'BOOKING')) - 1 THEN

          p_daylight_trn_can_fee := TO_NUMBER(sf_get_param_val(201,'BOOKING'))*0.10;

       ELSIF TRUNC(p_rqst_bk_date) - TRUNC(p_dl_can_date) >= 22 AND
             TRUNC(p_rqst_bk_date) - TRUNC(p_dl_can_date) <= 30 THEN

          p_daylight_trn_can_fee := TO_NUMBER(sf_get_param_val(201,'BOOKING'))*0.40;

       ELSIF TRUNC(p_rqst_bk_date) - TRUNC(p_dl_can_date) >= 4 AND
             TRUNC(p_rqst_bk_date) - TRUNC(p_dl_can_date) <= 21 THEN

          p_daylight_trn_can_fee := TO_NUMBER(sf_get_param_val(201,'BOOKING'))*0.60;

       ELSIF TRUNC(p_rqst_bk_date) - TRUNC(p_dl_can_date) >= 2 AND
             TRUNC(p_rqst_bk_date) - TRUNC(p_dl_can_date) <= 3 THEN

          p_daylight_trn_can_fee := TO_NUMBER(sf_get_param_val(201,'BOOKING'))*0.80;

       ELSIF TRUNC(p_rqst_bk_date) - TRUNC(p_dl_can_date) < 2 THEN

          p_daylight_trn_can_fee := TO_NUMBER(sf_get_param_val(201,'BOOKING'));

       END IF;

    ELSIF TRUNC(p_rqst_bk_date) - TRUNC(vd_rqst_date) < TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN

       IF NVL(vv_last_status,' ') = 'DTGUAR' THEN

          IF TRUNC(p_rqst_bk_date) - TRUNC(p_dl_can_date) < 2 THEN

             p_daylight_trn_can_fee := TO_NUMBER(sf_get_param_val(202,'BOOKING'));

          END IF;

       END IF;

    END IF;
    */

    --R653: Added on March 22, 2004 -- GNV
    /*R3653: Desactivated on March 23, 2004--GNV
      R3907: Activated on March 31, 2004 -- GNV  */

    BEGIN

       SELECT bs_seq
         INTO vn_bs_seq
         FROM bk_rqsts
        WHERE seq = p_br_seq;

    EXCEPTION
      WHEN OTHERS THEN

          vn_bs_seq := NULL;

    END;
    --R.3828. Adicion de 2 ultimos parametros
    vr_charge_rec := pkg_mb621000pr.f_get_cancel_bdt(vd_stat_date,
                                                    p_rqst_bk_date,
                                                    p_dl_can_date,
                                                    vd_rqst_date,
                                                    vv_last_status,
                                                    p_br_seq,
                                                    'S');

    p_daylight_trn_can_fee := vr_charge_rec.amount;


EXCEPTION
   WHEN OTHERS THEN

        pkg_evtms_db_util.p_error(SQLCODE,SQLERRM);
        p_daylight_trn_can_fee := NULL;

END;
FUNCTION F_GET_TOLL_BASIS_INFO
 (P_BR_SEQ NUMBER
 ,P_SEPARATOR VARCHAR2 := SF_GET_PARAM_VAL(NULL, 'TBR_TBI_EVTMS_S')
 )
 RETURN VARCHAR2
 IS
--============================================================================
-- DESCRIPCION:
--    Esta funcion devuelve la lista de las tarifas aplicables para los cargos de un booking.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha       Cambios realizados
-- ------- -------------  ----------  -----------------------------------------
-- R5127   VJaen           22-abr-2005 Version Inicial
               -- S4332 Jdam                             9-sep-2006 Subasta de booking
 -- S61519   VJaen           16-Jan-2008 Inclusión de evaluación de tipo Best Offer
 --                                      y booking basados en dimensiones, para retornar
 --                                      el texto respectivo.
--=============================================================================
--
      vt_book_fees      pkg_mb620000pr.charge_table_type;
      vv_list           VARCHAR2 (32000)                 := NULL;
      vv_descr          VARCHAR2 (240);
      vn_sin            NUMBER;
      vn_fix_amt        bk_rqsts.fix_amt%TYPE;                        --S4332
      vn_bsps_seq       bk_rqsts.bsps_seq%TYPE;
      vd_br_rqst_date   bk_rqsts.rqst_date%TYPE;
BEGIN
      --
      -- Obtiene la lista de cargos.
      vt_book_fees :=
         pkg_mb620000pr.f_get_book_charges (p_br_seq,
                                            'BF',
                                            'T',
                                            'S',
                                            NULL,
                                            NULL,
                                            NULL,
                                            NULL,
                                            NULL,
                                            NULL,
                                            NULL,
                                            NULL,
                                            NULL,
                                            NULL
                                           );

      --
      IF vt_book_fees.COUNT > 0
      THEN
         --
         -- Busca el SIN del buque.
         SELECT ship_no, fix_amt                                      -- S4332
                                , br.bsps_seq, br.rqst_date
           INTO vn_sin, vn_fix_amt                                    -- S4332
                                  , vn_bsps_seq, vd_br_rqst_date
           FROM ship_id_no SIN,
                cust_sched_needs csn,
                itin_items iti,
                bk_rqsts br
          WHERE SIN.seq = csn.sin_seq
            AND csn.seq = iti.csn_seq
            AND iti.bs_seq = br.bs_seq
            AND iti.typ = 'TII'
            AND iti.stat_cd <> 'CAN'
            AND iti.tii_first_in_bs = 'Y'
            AND br.seq = p_br_seq;

         --
         --
         IF NVL (vn_fix_amt, 0) > 0
         THEN
            RETURN ('BASED ON AUCTION');                             -- S4332
         ELSIF pkg_book_reftab_util.f_is_bestoffer                   -- S61519
                                                                  (vn_bsps_seq) =
                                                                           'Y'
         THEN
            RETURN ('BASED ON BEST OFFER');
         ELSIF vd_br_rqst_date >= pkg_book_datadict.f_bill_eff_date_v1
         THEN
            RETURN 'BASED ON VESSEL DIMENSIONS';
         END IF;

         FOR table_reg IN 1 .. vt_book_fees.COUNT
         LOOP
            IF vt_book_fees (table_reg).qty <> 1
            THEN
               -- Obtiene la descripción del dominio.
               vv_descr :=
                    sf_get_ref_meaning (vt_book_fees (table_reg).unit, 'UOM');

               --
               -- Adiciona el mensaje de ABOVE DECK cuando el buque es híbrido.
               IF sf_is_hybrid (vn_sin) AND vv_descr = 'TEU'
               THEN
                  vv_descr := vv_descr || ' ABOVE DECK';
               END IF;

               --
               vv_list :=
                     vv_list
                  || TO_CHAR (vt_book_fees (table_reg).qty, 'FM9,999,990')
                  || ' '
                  || vv_descr
                  || p_separator;
            END IF;
         END LOOP;
      END IF;

      --
      IF vv_list IS NOT NULL
      THEN
         --
         -- Remueve el separador del final de la lista.
         vv_list :=
                  SUBSTR (vv_list, 1, LENGTH (vv_list) - LENGTH (p_separator));
      END IF;

       --
      -- Retorna la lista resultante.
      DBMS_OUTPUT.put_line (vv_list);
      --
      RETURN vv_list;
   EXCEPTION
      WHEN OTHERS
      THEN
         DBMS_OUTPUT.put_line (SQLERRM);
         RETURN NULL;
   END f_get_toll_basis_info;
PROCEDURE P_MESSAGE_PARAM
 (P_BK_RQST_DATE IN DATE
 ,P_RQST_DATE IN DATE
 ,P_COND_LEVEL IN BK_CONDS.LEV%TYPE
 ,P_SLOT_TYP IN VARCHAR2
 ,P_BD_SEQ IN BK_DAYS.SEQ%TYPE
 ,P_DRV_HML IN VARCHAR2
 ,P_DRV_PDE_CD IN REST_USR_CDS.CD%TYPE
 ,P_TRN_DIR IN VARCHAR2
 ,P_CLASS IN BK_SIZE_TYPS.CLASS%TYPE
 ,P_MESSAGE OUT VARCHAR2
 )
 IS
 vn_count  NUMBER(4);
vn_total_x_dir NUMBER(3);
vn_max_val_rest NUMBER(4);
vn_val_rest_dir number(4);
--

CURSOR COND_PARAM IS
 SELECT DIR, PARAM_VAL, REST_COND, VAL
     FROM BK_COND_PARAMS
  WHERE BK_VSL_TYP = p_slot_typ
        AND BCO_LEV      = p_cond_level
       AND ( (PARAM_VAL    in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                    p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece)
                   OR
	 (PARAM_VAL   not in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND (p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece OR p_class IS NULL))
                )
        AND P_RQST_DATE  BETWEEN DATE_FROM AND DATE_TO
    ORDER BY DIR, REST_COND;

--
CURSOR DAY_PARAM (p_bk_rqst_date IN DATE) IS
SELECT DIR, PARAM_VAL, REST_IND, VAL
   FROM BK_DAY_PARAMS
 WHERE VSL_TYP = p_slot_typ
       AND ( (PARAM_VAL    in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                   p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece)
                   OR
                (PARAM_VAL   not in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND (p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece OR p_class IS NULL))
                )
      AND  BD_SEQ = (SELECT seq FROM BK_DAYS WHERE date_from = p_bk_rqst_date)
  ORDER BY DIR, REST_IND;
--R.4484
BEGIN
  --
  SELECT count(*)
  INTO vn_count
  FROM bk_day_params
  WHERE bd_seq = (SELECT seq FROM BK_DAYS WHERE date_from = p_bk_rqst_date);
  --
  IF vn_count > 0 THEN
    FOR day_param_current IN DAY_PARAM(p_bk_rqst_date) LOOP
       --Validacion por Direccion
       IF day_param_current.dir = p_trn_dir THEN
           --
       	  --Búsqueda de maximo de slots disponibles por direccion y tipo para una fecha especifica
          SELECT MAX(val) INTO vn_total_x_dir
	        FROM BK_DAY_PARAMS
	         WHERE VSL_TYP = p_slot_typ
	           AND BD_SEQ = (SELECT seq FROM BK_DAYS WHERE date_from = p_bk_rqst_date)
	            AND ( ( PARAM_VAL    in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                        p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece
                       )
                       OR
                       ( PARAM_VAL   not in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                         (p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece OR p_class IS NULL)
                        )
                     )
	           AND DIR  = p_trn_dir;
            --
       	    --Comparación de cantidad de reserva x Dir y tipo vs slots totales disponibles por direccion y tipo
            --S61519
            --Se segrega el IF para tomar en consideración los slots totales ocupados por direccion
            --tanto para los XPieces como para los slots que no son XPiece
            --
            --IF NVL(Sf_Get_Vessel_By_Dir(1,p_bd_seq,day_param_current.dir,p_slot_typ),0)  >= vn_total_x_dir THEN

            IF ( p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND
                 NVL(Sf_Get_Vessel_By_Dir(7,p_bd_seq,day_param_current.dir,p_slot_typ,p_class),0)  >= vn_total_x_dir
               ) OR
               ( p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND
                 NVL(Sf_Get_Vessel_By_Dir(5,p_bd_seq,day_param_current.dir,p_slot_typ,p_class),0)  >= vn_total_x_dir
               )
               THEN

                IF p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large THEN
    		        p_message:= 'e_large_dir';
    		        EXIT;
  		        ELSE
  		            p_message:= 'e_small_dir';
  		            EXIT;
  		        END IF;
  		    --Validacion por Restriccion
                --ELSIF (p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large AND p_drv_hml IN ('M','D','N','O')) OR
                  ELSIF (p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large AND (INSTR(PKG_BOOK_DATADICT.f_hml_rest_list, p_drv_hml) > 0)) OR
                        (p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_small AND p_drv_pde_cd IS NOT NULL) THEN
                 --
             	 --Validacion por Restriccion
             	 --
                 --Busqueda de slots totales disponibles por restriccion y tipo para una fecha especifica
                 SELECT val INTO vn_max_val_rest
	               FROM BK_DAY_PARAMS
	              WHERE VSL_TYP = p_slot_typ
	                AND BD_SEQ = (SELECT seq FROM BK_DAYS WHERE date_from = p_bk_rqst_date)
	                AND ( ( PARAM_VAL    in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                            p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece
                           )
                           OR
                          ( PARAM_VAL   not in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                           (p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece OR p_class IS NULL)
                          )
                        )
	                AND DIR  = 'B';
	             --
	             --Busqueda de de slots disponibles por restriccion, tipo y direccion para una fecha especifica
                 SELECT val INTO vn_val_rest_dir
	              FROM BK_DAY_PARAMS
	             WHERE VSL_TYP = p_slot_typ
	               AND BD_SEQ = (SELECT seq FROM BK_DAYS WHERE date_from = p_bk_rqst_date)
	               AND DIR = p_trn_dir
	               AND ( ( PARAM_VAL    in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                           p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece
                          )
                       OR
                       ( PARAM_VAL   not in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                         (p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece OR p_class IS NULL)
                        )
                     )
	               AND REST_IND = 'Y';
                 --
	             --Comparación de cantidad de reserva de buques restringidos por tipo  vs
	             --slots totales disponibles por restriccion y tipo para una fecha especifica

	             --S61519
                 --Se segrega el IF para tomar en consideración los slots totales ocupados por direccion
                 --tanto para los XPieces como para los slots que no son XPiece

                 --IF NVL(Sf_Get_Vessel_By_Dir(4,p_bd_seq,'',p_slot_typ),0) >=  vn_max_val_rest THEN

	               IF  ( p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND
                         NVL(Sf_Get_Vessel_By_Dir(10,p_bd_seq,'',p_slot_typ,p_class),0)  >= vn_max_val_rest
                       ) OR
	                   ( p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND --vn_max_val_rest <> 0 AND
	                     NVL(Sf_Get_Vessel_By_Dir(12,p_bd_seq,'',p_slot_typ,p_class),0)  >= vn_max_val_rest
                       )
                       THEN

                         IF p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large THEN
                             p_message:= 'e_max_large_cond';
                             EXIT;
                         ELSE
                             p_message := 'e_max_small_cond';
                             EXIT;
                         END IF;
                 --
	             --Comparación de cantidad de reserva de buques restringidos por tipo y direccion  vs
	             --slots disponibles por restriccion, tipo y direccion para una fecha especifica

	             --S61519
                 --Se segrega el IF para tomar en consideración los slots totales ocupados por direccion
                 --tanto para los XPieces como para los slots que no son XPiece

                 --ELSIF  NVL(Sf_Get_Vessel_By_Dir(2,p_bd_seq,day_param_current.dir,p_slot_typ),0) >=  vn_val_rest_dir THEN

                 ELSIF ( p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND
                         NVL(Sf_Get_Vessel_By_Dir(8,p_bd_seq,day_param_current.dir,p_slot_typ,p_class),0)  >= vn_val_rest_dir
                       ) OR
	                   ( p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND --vn_val_rest_dir <> 0 AND
	                     NVL(Sf_Get_Vessel_By_Dir(11,p_bd_seq,day_param_current.dir,p_slot_typ,p_class),0)  >= vn_val_rest_dir
                       )
                       THEN
                          IF p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large THEN
                              p_message:= 'e_large_dir_rest';
                              EXIT;
                          ELSE
                              p_message:= 'e_small_dir_rest';
                              EXIT;
                          END IF;
                ELSE
                    EXIT;
                END IF;
           ELSE
              EXIT;
           END IF;
        END IF; --day_param_current.dir = p_trn_dir
     END LOOP;
  ELSE
    FOR cond_param_current IN COND_PARAM LOOP
        --Validacion por Direccion
	    IF cond_param_current.dir = p_trn_dir THEN
	        --
       	    --Búsqueda de maximo slots disponibles por direccion,condicion y tipo para una fecha especifica
            SELECT MAX(to_number(val)) INTO vn_total_x_dir
	          FROM BK_COND_PARAMS
	         WHERE BK_VSL_TYP = p_slot_typ
	           AND BCO_LEV    = p_cond_level
	           AND P_RQST_DATE BETWEEN DATE_FROM AND DATE_TO
	           AND ( ( PARAM_VAL    in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                       p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece
                      )
                       OR
	                 ( PARAM_VAL   not in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
	                   (p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece OR p_class IS NULL)
	                  )
                   )
	           AND DIR = p_trn_dir;
	        --
       	    --Comparación de cantidad de reserva x Dir y tipo vs slots totales disponibles por direccion, condicion y tipo para una fecha especifica
       	    --S61519
            --Se segrega el IF para tomar en consideración los slots totales ocupados por direccion
            --tanto para los XPieces como para los slots que no son XPiece

            --IF NVL(Sf_Get_Vessel_By_Dir(1,p_bd_seq,cond_param_current.dir,p_slot_typ),0) >=  vn_total_x_dir THEN

              IF ( p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND
                   NVL(Sf_Get_Vessel_By_Dir(7,p_bd_seq,cond_param_current.dir,p_slot_typ,p_class),0)  >= vn_total_x_dir
                  ) OR
                  ( p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND
                    NVL(Sf_Get_Vessel_By_Dir(5,p_bd_seq,cond_param_current.dir,p_slot_typ,p_class),0)  >= vn_total_x_dir
                  )
                 THEN

  			      IF p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large THEN
    				  p_message:= 'e_large_dir';
    				  EXIT;
  			      ELSE
  				      p_message:= 'e_small_dir';
  				      EXIT;
  			      END IF;
  			 --Validacion por Restriccion
      	    --ELSIF (p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large AND p_drv_hml IN ('M','D','N','O')) OR
      	      ELSIF (p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large AND (INSTR(PKG_BOOK_DATADICT.f_hml_rest_list, p_drv_hml) > 0)) OR
                    (p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_small AND p_drv_pde_cd IS NOT NULL) THEN
                 --
                 --Busqueda de slots totales disponibles por restriccion, condicion y tipo para una fecha especifica
                 SELECT to_number(val) INTO vn_max_val_rest
	               FROM BK_COND_PARAMS
	              WHERE BK_VSL_TYP = p_slot_typ
	                AND BCO_LEV    = p_cond_level
	                AND P_RQST_DATE BETWEEN DATE_FROM AND DATE_TO
	                AND ( ( PARAM_VAL    in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                            p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece
                           )
                            OR
	                       ( PARAM_VAL   not in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
	                         (p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece OR p_class IS NULL)
	                        )
                        )
	                AND DIR = 'B';
	             --
	             --Busqueda de slots disponibles por restriccion, condicion, tipo y direccion para una fecha especifica
  	             SELECT to_number(val) INTO vn_val_rest_dir
	               FROM BK_COND_PARAMS
	              WHERE BK_VSL_TYP = p_slot_typ
	                AND BCO_LEV    = p_cond_level
	                AND P_RQST_DATE BETWEEN DATE_FROM AND DATE_TO
	                AND DIR = p_trn_dir
	                AND ( ( PARAM_VAL    in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
                            p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece
                          )
                           OR
	                     ( PARAM_VAL   not in (2,4,10,5,7,12,6,8,14,15,16,18,19,20,21,22,23,24,25,26) AND
	                       (p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece OR p_class IS NULL)
	                     )
                        )
	                AND REST_COND = 'Y';
	             --
	             --Comparación de cantidad de reserva de buques restringidos por tipo  vs
	             --slots totales disponibles por restriccion, condicion y tipo para una fecha especifica
	             --S61519
                 --Se segrega el IF para tomar en consideración los slots totales ocupados por direccion
                 --tanto para los XPieces como para los slots que no son XPiece

    	         --IF NVL(Sf_Get_Vessel_By_Dir(4,p_bd_seq,'',p_slot_typ),0) >= vn_max_val_rest THEN

    	         IF  ( p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND
                         NVL(Sf_Get_Vessel_By_Dir(10,p_bd_seq,'',p_slot_typ,p_class),0)  >= vn_max_val_rest
                      ) OR
	                  ( p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND --vn_max_val_rest <> 0 AND
	                     NVL(Sf_Get_Vessel_By_Dir(12,p_bd_seq,'',p_slot_typ,p_class),0)  >= vn_max_val_rest
                       )
                       THEN

  	                      IF p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large THEN
  	  			             p_message:= 'e_max_large_cond';
  	  			             EXIT;
  	                      ELSE
  	                         p_message:= 'e_max_small_cond';
  	                         EXIT;
  	                      END IF;
  	             --
	             --Comparación de cantidad de reserva de buques restringidos por tipo y direccion  vs
	             --slots disponibles por restriccion,condicion, tipo y direccion para una fecha especifica

	             --S61519
                 --Se segrega el IF para tomar en consideración los slots totales ocupados por direccion
                 --tanto para los XPieces como para los slots que no son XPiece

    	   	     --ELSIF NVL(Sf_Get_Vessel_By_Dir(2,p_bd_seq,cond_param_current.dir,p_slot_typ),0)  >=  vn_val_rest_dir THEN

    	   	      ELSIF ( p_class <> PKG_BOOK_DATADICT.f_bvtc_xpiece AND
                          NVL(Sf_Get_Vessel_By_Dir(8,p_bd_seq,cond_param_current.dir,p_slot_typ,p_class),0)  >= vn_val_rest_dir
                        ) OR
	                    ( p_class = PKG_BOOK_DATADICT.f_bvtc_xpiece AND --vn_val_rest_dir <> 0 AND
	                      NVL(Sf_Get_Vessel_By_Dir(11,p_bd_seq,cond_param_current.dir,p_slot_typ,p_class),0)  >= vn_val_rest_dir
                        )
                       THEN

  	  			       IF p_slot_typ = PKG_BOOK_DATADICT.f_bk_typ_large THEN
  	  				    	p_message:= 'e_large_dir_rest';
    		                EXIT;
  	  			       ELSE
  	  		 		        p_message:= 'e_small_dir_rest';
  	  		 		        EXIT;
  	  			       END IF;
  	  		      ELSE
  	  			      EXIT;
  	  			  END IF;
  	        ELSE
  	            EXIT;
  	  		END IF;
  	  	END IF; --cond_param_current.dir = p_trn_dir
  	  END LOOP;
  END IF;   --vn_count > 0
END;
/* Generate the Booking Swap, Substitution, Change Date Transactions */
PROCEDURE P_BOOK_TRANS_PROCS
 (P_WBP_SEQ IN WEB_BK_PROCS.SEQ%TYPE
 ,P_BR_SEQ_FIRST OUT BK_RQSTS.SEQ%TYPE
 ,P_BR_SEQ_SEC OUT BK_RQSTS.SEQ%TYPE
 ,P_BK_FEE OUT CHARGES.CHRG_AMT%TYPE
 ,P_CAN_DY OUT WEB_BK_RQST_TRANS.BR_CANCL_DY%TYPE
 ,P_CAN_HR OUT WEB_BK_RQST_TRANS.BR_CANCL_HR%TYPE
 ,P_CAN_FEE OUT CHARGES.CHRG_AMT%TYPE
 ,P_BK_TRANS_STAT OUT WEB_BK_REQUESTS.TRANS_STAT%TYPE
 ,P_BK_RQST_STAT OUT WEB_BK_REQUESTS.RQST_STATUS%TYPE
 ,P_BK_COND OUT BK_CONDS.LEV%TYPE
 ,P_ERROR_CODE OUT VARCHAR2
 ,P_ERROR_MESS OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
--   Este procedimiento se encarga invocar los procedimientos de Booking para Transacciones
--  de Swapping, Substitution, Change Date generadas desde EDCS, de acuerdo al tipo de --  transaccion a realizar.

-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador    Fecha                  Cambios realizados
-- -------------   --------------------  -------------------     -----------------------------
-- S15142      GVillarreal        31-AUG-2006      Versión Inicial


CURSOR c_web_bk_procs(p_web_proc    IN   web_bk_procs.seq%TYPE)  IS
SELECT  typ
FROM web_bk_procs
WHERE seq = p_web_proc;

vv_wbp_typ                  WEB_BK_PROCS.typ%TYPE;

vn_br_seq_first          BK_RQSTS.seq%TYPE;
vn_br_seq_sec           BK_RQSTS.seq%TYPE;
vn_bk_fee                    CHARGES.chrg_amt%TYPE;
vn_can_dy                    WEB_BK_RQST_TRANS.br_cancl_dy%TYPE;
vn_can_hr                    WEB_BK_RQST_TRANS.br_cancl_hr%TYPE;
vn_can_fee                  CHARGES.chrg_amt%TYPE;
vv_trans_stat              WEB_BK_REQUESTS.trans_stat%TYPE;
vv_rqst_stat                WEB_BK_REQUESTS.rqst_status%TYPE;
vn_bk_cond                 WEB_BK_REQUESTS.bk_cond%TYPE;

vv_error_code            VARCHAR2(30);
vv_error_mess           VARCHAR2(2000);
BEGIN

  /*Parámetros de este procedimiento:
   Entrada:
   P_WPB_SEQ = Secuencia del registro padre de la transacción
               en la tabla WEB_BK_PROCS

   Salida:
   P_BR_SEQ_FIRST = Secuencia del nuevo registro de BK_RQSTS
          Para caso de SWAP = Secuencia del registro
                              de BK_RQSTS del primer
                              buque Swapeado
          Para caso de SUBSTITUTION = Secuencia del registro
                              de BK_RQSTS creado con la
                              sustitución de la reservación
          Para caso de CHANGE DATE = Secuencia del registro
                              de BK_RQSTS creado con la
                              nueva fecha de reservación


   P_BR_SEQ_SEC = Secuencia del nuevo registro de BK_RQSTS
                  para el segundo buque Swapeado. No aplica
                  para los casos de Substitution y Change Date

   P_BK_FEE     = Cargo de Booking

   P_CAN_DY     = Cantidad de Días antes de la Fecha de Reservación con la
                  que se solicita la Cancelación

   P_CAN_HR     = Cantidad de horas antes de la fecha de reservación con la
                  que se solicita la cancelación

   P_CAN_FEE    = Cargo de Cancelación de Booking

   P_BK_TRANS_STAT = Estatus de la ejecución de la transacción
                     TRUE si la transacción fue exitosa
                     FALSE si la transacción no se pudo completar

   P_BK_RQST_STAT =  Estatus del Request de la Transacción
                     APPROVED (Transacción Aprobada)
                     REJECT   (Transacción Rechazada)
                     PEND     (Transacción Pendiente)

   P_BK_COND      = Condición existente en el canal al momento de
                    hacer la reservación. Esto se deriva de la condición
                    en BK_PERIODS con respecto al periodo del día de booking.

   P_ERROR_CODE   = Código de mensaje de error a retornar

   P_ERROR_MESS   = Texto de mensaje de error.
  */

  BEGIN

     /*Inicialización de Parámetros*/

     p_br_seq_first  := NULL;
     p_br_seq_sec    := NULL;
     p_bk_fee        := NULL;
     p_can_dy        := NULL;
     p_can_hr        := NULL;
     p_can_fee       := NULL;
     p_bk_trans_stat := NULL;
     p_bk_rqst_stat  := NULL;
     p_bk_cond       := NULL;
     p_error_code    := NULL;
     p_error_mess    := NULL;


     /*
     Búsqueda de registro padre de la transacción
     para determinar el tipo de la misma (SWAP,
     SUBSTITUTION, CHANGE DATE para procesamiento
     de TBR y TBC
     */
     IF p_wbp_seq IS NOT NULL THEN

        OPEN c_web_bk_procs(p_wbp_seq);

        IF c_web_bk_procs%ISOPEN THEN

           FETCH c_web_bk_procs INTO vv_wbp_typ;

           IF c_web_bk_procs%NOTFOUND THEN

              vv_wbp_typ := NULL;

           END IF;

        ELSE

           vv_wbp_typ := NULL;

        END IF;

        CLOSE c_web_bk_procs;

        --DBMS
        dbms_output.put_line('P_BOOK_TRANS_PROCS');
        dbms_output.put_line('WEB_BK_PROCS SEQ ='||to_char(p_wbp_seq));
        dbms_output.put_line('WEB_BK_PROCS TYP ='||vv_wbp_typ);

        IF vv_wbp_typ IS NOT NULL THEN

           /*
           Invocación a rutinas de procesamiento de
           transacciones SWAP, SUBSTITUTION, CHANGE DATE
           */
           CASE
             WHEN vv_wbp_typ = pkg_booking.cv_swap_cd THEN


                  /*Swap Transaction*/
                  p_book_trans_swap(p_wbp_seq,
                                    vv_wbp_typ,
                                    vn_br_seq_first,
                                    vn_br_seq_sec,
                                    vn_bk_fee,
                                    vv_trans_stat,
                                    vv_error_code);

                  --DBMS
                  dbms_output.put_line('Regrese de Swap Transaction con error code ='||vv_error_code);
                  dbms_output.put_line('Regrese de Swap Transaction con trans stat ='||vv_trans_stat);


             WHEN vv_wbp_typ = pkg_booking.cv_subs_cd THEN

                  /*Substitution Transaction*/
                  p_book_trans_subs(p_wbp_seq,
                                    vv_wbp_typ,
                                    vn_br_seq_first,
                                    vn_bk_fee,
                                    vn_can_dy,
                                    vn_can_hr,
                                    vn_can_fee,
                                    vv_trans_stat,
                                    vn_bk_cond,
                                    vv_error_code);

                  --DBMS
                  dbms_output.put_line('Regrese de Subs Transaction con error code ='||vv_error_code);
                  dbms_output.put_line('Regrese de Subs Transaction con trans stat ='||vv_trans_stat);

             WHEN vv_wbp_typ = pkg_booking.cv_chgd_cd THEN

                  /*Change Date Transaction*/
                  p_book_trans_chgd(p_wbp_seq,
                                    vv_wbp_typ,
                                    vn_br_seq_first,
                                    vn_bk_fee,
                                    vn_can_dy,
                                    vn_can_hr,
                                    vn_can_fee,
                                    vv_trans_stat,
                                    vn_bk_cond,
                                    vv_error_code);

                  --DBMS
                  dbms_output.put_line('Regrese de Change Date Transaction con error code ='||vv_error_code);
                  dbms_output.put_line('Regrese de Change Date Transaction con trans stat ='||vv_trans_stat);

           END CASE;

           /*Evaluación del resultado de la transacción*/
           CASE

             /*Transacción Completada es Aprobada*/
             WHEN vv_trans_stat = pkg_booking.cv_trans_stat_true THEN

                  vv_rqst_stat := pkg_booking.cv_rqst_status_apprv;

             /*Transacción Rechazada. Bookings y Cancelaciones no Completadas*/
             WHEN vv_trans_stat = pkg_booking.cv_trans_stat_false THEN

                  vv_rqst_stat := pkg_booking.cv_rqst_status_rej;

                  IF vv_error_code IS NULL THEN

                     /*Transaction Rejected due to invalid conditions*/
                     vv_error_code := 'USR-30297';

                  END IF;

             /*Error en Generación de la Transacción.*/
             ELSE

                  vv_rqst_stat := pkg_booking.cv_rqst_status_pend;

                  IF vv_error_code IS NULL THEN

                     /*Transaction could not be processed due to undefined error*/
                    vv_error_code := 'USR-30298';

                  END IF;

             END CASE;

             --DBMS
             dbms_output.put_line('Error Code Cambiado ='||vv_error_code);
             dbms_output.put_line('Rqst Status Final'||vv_rqst_stat);

        ELSE

           /*Data invalida en WEB_BK_PROCS. No se pudo
             encontrar información de tipo de Transacción*/
           vn_br_seq_first := NULL;
           vn_br_seq_sec   := NULL;
           vn_bk_fee       := NULL;
           vn_can_dy       := NULL;
           vn_can_hr       := NULL;
           vn_can_fee      := NULL;
           vv_trans_stat   := NULL;
           vn_bk_cond      := NULL;
           vv_rqst_stat    := pkg_booking.cv_rqst_status_pend;

           /*Transaction could not be processed due to missing reference for transaction type*/
           vv_error_code   := 'USR-30299';

        END IF;

  END IF;

  EXCEPTION
      WHEN OTHERS THEN

          vn_br_seq_first := NULL;
          vn_br_seq_sec   := NULL;
          vn_bk_fee       := NULL;
          vn_can_dy       := NULL;
          vn_can_hr       := NULL;
          vn_can_fee      := NULL;
          vv_trans_stat   := pkg_booking.cv_trans_stat_false;
          vn_bk_cond      := NULL;
          vv_rqst_stat    := pkg_booking.cv_rqst_status_rej;

          /*Error de BD Oracle*/
          vv_error_mess   := SQLERRM;

          --Para poder enviar el codigo de error se desactiva esta instruccion
          --Pkg_Evtms_Db_Util.p_generic_exception(SQLCODE,SQLERRM);

          IF INSTR(vv_error_mess, 'USR-') > 0 THEN

             vv_error_code := SUBSTR(vv_error_mess, INSTR(vv_error_mess, 'USR-'), 9);


          ELSE

             IF INSTR(vv_error_mess, 'USR-') = 0 THEN

                vv_error_code   := SQLCODE;

             ELSE

                vv_error_code   := SQLCODE;

             END IF;

          END IF;

  END;

  --S15142.T15: Added on August 08, 2007 -- GNV
  --Rollback si la transacción no fue exitosa
  IF nvl(vv_trans_stat,'FALSE') = 'FALSE' THEN

     ROLLBACK;

  END IF;--IF nvl(vv_trans_stat,'FALSE') = 'FALSE' THEN

  /*Asignación de Parámetros de Salida*/
  p_br_seq_first  := vn_br_seq_first;
  p_br_seq_sec    := vn_br_seq_sec;
  p_bk_fee        := vn_bk_fee;
  p_can_dy        := vn_can_dy;
  p_can_hr        := vn_can_hr;
  p_can_fee       := vn_can_fee;
  p_bk_trans_stat := vv_trans_stat;
  p_bk_rqst_stat  := vv_rqst_stat;
  p_bk_cond       := vn_bk_cond;
  p_error_code    := vv_error_code;
  p_error_mess    := vv_error_mess;

  --DBMS
  dbms_output.put_line('BOOK_PROCS p_br_seq_first ='||to_char(vn_br_seq_first));
  dbms_output.put_line('BOOK_PROCS p_br_seq_sec ='||to_char(vn_br_seq_sec));
  dbms_output.put_line('BOOK_PROCS p_bk_fee ='||to_char(vn_bk_fee));
  dbms_output.put_line('BOOK_PROCS p_can_dy ='||to_char(vn_can_dy));
  dbms_output.put_line('BOOK_PROCS p_can_hr ='||to_char(vn_can_hr));
  dbms_output.put_line('BOOK_PROCS p_can_fee ='||to_char(vn_can_fee));
  dbms_output.put_line('BOOK_PROCS p_bk_trans_stat ='||vv_trans_stat);
  dbms_output.put_line('BOOK_PROCS p_bk_rqst_stat ='||vv_rqst_stat);
  dbms_output.put_line('BOOK_PROCS p_error_code ='||vv_error_code);
  dbms_output.put_line('BOOK_PROCS p_error_mess ='||vv_error_mess);


END;
/* Generate the booking swap transaction */
PROCEDURE P_BOOK_TRANS_SWAP
 (P_BK_WBP_SEQ IN WEB_BK_PROCS.SEQ%TYPE
 ,P_BK_WBP_TYP IN WEB_BK_PROCS.TYP%TYPE
 ,P_BR_WB_SEQ_FIRST OUT BK_RQSTS.SEQ%TYPE
 ,P_BR_WB_SEQ_SEC OUT BK_RQSTS.SEQ%TYPE
 ,P_BK_WB_FEE OUT CHARGES.CHRG_AMT%TYPE
 ,P_BK_WB_STAT OUT WEB_BK_REQUESTS.TRANS_STAT%TYPE
 ,P_ERROR_CD OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
--   Este procedimiento se encarga realizar la transacción de  Swap de Reservaciones  (Bookings) --   de acuerdo a TBR (Transit Booking Requests)  de dos buques generados desde EDCS
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador    Fecha          Cambios realizados
-- ---------  --------------   ------------    -----------------------------
-- S15142      GVillarreal      01-SEP-2006    Versión Inicial
-- S61519      VJaen            15-Jan-2008    Prevencion de procesamiento para
--                                             booking especiales y grabado en tabla
--                                             de detalle.
--=============================================================================
      CURSOR c_web_bk_requests (p_bk_wbp_seq IN web_bk_procs.seq%TYPE)
      IS
         SELECT seq, visit_seq, ship_no, vty_cd, cust_cd, int_mov_seq,
                trn_dir, trn_br_date, trn_br_rqst_date, org_bk_date,
                org_bk_id, primary_ref, sec_ref, AGENT, fix_amt,
                new_extm_beam, dtu_int
           FROM web_bk_requests
          WHERE wbp_seq = p_bk_wbp_seq
            AND rqst_status = PKG_BOOKING.cv_rqst_status_pend;

      CURSOR c_web_bk_requests_to (
         p_bk_wbp_seq   IN   web_bk_procs.seq%TYPE,
         p_wbr_seq      IN   web_bk_requests.seq%TYPE
      )
      IS
         SELECT seq, visit_seq, ship_no, vty_cd, cust_cd, int_mov_seq,
                trn_dir, trn_br_date, trn_br_rqst_date, org_bk_date,
                org_bk_id, primary_ref, sec_ref, AGENT, fix_amt,
                new_extm_beam, dtu_int
           FROM web_bk_requests
          WHERE wbp_seq = p_bk_wbp_seq
            AND seq != p_wbr_seq
            AND rqst_status = PKG_BOOKING.cv_rqst_status_pend;

      vr_wb_bk_request_from           PKG_BOOKING.tr_web_bk_requests;
      vr_wb_bk_request_to             PKG_BOOKING.tr_web_bk_requests;
      vr_bk_rqsts_from                PKG_BOOKING.tr_bk_rqsts;
      vr_bk_rqsts_to                  PKG_BOOKING.tr_bk_rqsts;
      vv_bk_wb_stat                   web_bk_requests.trans_stat%TYPE;
      vn_bk_wb_fee                    charges.chrg_amt%TYPE;
      vn_br_wb_seq_first              bk_rqsts.seq%TYPE;
      vn_br_wb_seq_sec                bk_rqsts.seq%TYPE;
      vn_bs_seq_from                  bill_sets.seq%TYPE;
      vv_rest_from                    rest_usr_cds.cd%TYPE;
      -- From Definition on function SF_GET_REST_USER_BK
      vn_iti_seq_from                 itin_items.seq%TYPE;
      vv_iti_trn_dir_from             itin_items.tii_trn_dir%TYPE;
      vv_iti_cust_cd_from             VARCHAR2 (218);
      -- From Definition on function SF_GET_CUST_NAME
      vv_iti_pde_cd_from              itin_items.pde_cd%TYPE;
      vv_hml_from                     cust_vsl_chars.lvc_hml_qualified%TYPE;
      vn_ext_beam_from                cust_vsl_chars.extm_beam%TYPE;
      vv_gas_free_from                cust_visit_dets.tblc_gas_free_ind%TYPE;
      vv_req_arr_from                 VARCHAR2 (5);
      -- From Definition de funcion PKG_BOOKING.F_GET_ARR_PAR;
      vv_bk_rmk_from                  bk_rqsts.stat_rmk%TYPE;
      vn_bk_wb_fee_from               charges.chrg_amt%TYPE;
      vi_br_seq_from_son              bk_rqsts.seq%TYPE;
      vn_bs_seq_to                    bill_sets.seq%TYPE;
      vv_rest_to                      rest_usr_cds.cd%TYPE;
      -- From Definition on function SF_GET_REST_USER_BK
      vn_iti_seq_to                   itin_items.seq%TYPE;
      vv_iti_trn_dir_to               itin_items.tii_trn_dir%TYPE;
      vv_iti_cust_cd_to               VARCHAR2 (218);
      -- From Definition on function SF_GET_CUST_NAME
      vv_iti_pde_cd_to                itin_items.pde_cd%TYPE;
      vv_hml_to                       cust_vsl_chars.lvc_hml_qualified%TYPE;
      vn_ext_beam_to                  cust_vsl_chars.extm_beam%TYPE;
      vv_gas_free_to                  cust_visit_dets.tblc_gas_free_ind%TYPE;
      vv_req_arr_to                   VARCHAR2 (5);
      -- From Definition de funcion PKG_BOOKING.F_GET_ARR_PAR;
      vv_bk_rmk_to                    bk_rqsts.stat_rmk%TYPE;
      vn_bk_wb_fee_to                 charges.chrg_amt%TYPE;
      vi_br_seq_to_son                bk_rqsts.seq%TYPE;
      vv_error_cd                     VARCHAR2 (30);
      vv_upd_from                     VARCHAR2 (1);
      vv_upd_to                       VARCHAR2 (1);
      vv_bk_fix_amt_from              VARCHAR2 (1);
      vv_bk_fix_amt_to                VARCHAR2 (1);
--S15142: Added on July 05, 2007 -- GNV
      vv_vsl_typ_from                 ship_id_no.vty_cd%TYPE;
      vv_vsl_typ_to                   ship_id_no.vty_cd%TYPE;
      e_bk_swap_prev_one              EXCEPTION;
      e_bk_swap_prev_both             EXCEPTION;
      --
      -- S61519
      -- VJaen 15Ene2008
      e_avoid_oper_for_spec_booking   EXCEPTION;
      e_book_typ_must_be_equal        EXCEPTION;
--
vv_hml_rest_from                       VARCHAR2(1);
vv_hml_rest_to                       VARCHAR2(1);
BEGIN
      BEGIN
         p_br_wb_seq_first := NULL;
         p_br_wb_seq_sec := NULL;
         p_bk_wb_fee := NULL;
         p_bk_wb_stat := NULL;
         p_error_cd := NULL;
         vv_upd_from := 'N';
         vv_upd_to := 'N';
         vv_bk_fix_amt_from := 'N';
         vv_bk_fix_amt_to := 'N';

         IF p_bk_wbp_seq IS NOT NULL
         THEN
            OPEN c_web_bk_requests (p_bk_wbp_seq);

            --Buscar información de los TBR de los buques FROM y TO
            IF c_web_bk_requests%ISOPEN
            THEN
               FETCH c_web_bk_requests
                INTO vr_wb_bk_request_from;

               IF c_web_bk_requests%NOTFOUND
               THEN
                  vr_wb_bk_request_from := NULL;
               END IF;

               CLOSE c_web_bk_requests;

               --DBMS
               pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Secuencia Registro TBR buque FROM='
                                 || TO_CHAR (vr_wb_bk_request_from.seq),
                                 'INFO'
                                );
               pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Original BK ID buque FROM='
                                 || TO_CHAR (vr_wb_bk_request_from.org_bk_id),
                                 'INFO'
                                );
               DBMS_OUTPUT.put_line
                                   (   'Original BK DATE buque FROM='
                                    || TO_CHAR
                                            (vr_wb_bk_request_from.org_bk_date)
                                   );

               IF vr_wb_bk_request_from.seq IS NOT NULL
               THEN
                  OPEN c_web_bk_requests_to (p_bk_wbp_seq,
                                             vr_wb_bk_request_from.seq
                                            );

                  IF c_web_bk_requests_to%ISOPEN
                  THEN
                     FETCH c_web_bk_requests_to
                      INTO vr_wb_bk_request_to;

                     IF c_web_bk_requests_to%NOTFOUND
                     THEN
                        vr_wb_bk_request_to := NULL;
                     END IF;

                     CLOSE c_web_bk_requests_to;

                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Secuencia Registro TBR buque TO='
                                 || TO_CHAR (vr_wb_bk_request_to.seq),
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Original BK ID buque TO='
                                 || TO_CHAR (vr_wb_bk_request_to.org_bk_id),
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Original BK DATE buque TO='
                                 || TO_CHAR (vr_wb_bk_request_to.org_bk_date),
                                 'INFO'
                                );
                     --Comparación de Información del Itinerario del Buque de EVTMS
                     --contra la información recibida en el Transit Booking Request.

                     --Búsqueda de información de BK_RQSTS del buque FROM
                     p_get_bk_rqst_info (vr_wb_bk_request_from.org_bk_id,
                                         vr_bk_rqsts_from
                                        );

                     --
                     -- Evalua si el request es de Auction, Best Offer o X-Piece.
                     IF    vr_bk_rqsts_from.fix_amt IS NOT NULL
                        OR pkg_book_reftab_util.f_is_bestoffer
                                                    (vr_bk_rqsts_from.bsps_seq) =
                                                                           'Y'
                        OR pkg_book_reftab_util.f_is_xpiece
                                                    (vr_bk_rqsts_from.bsps_seq) =
                                                                           'Y'
                     THEN
                        --
                        -- S61519
                        -- VJaen 15Ene2008
                        -- Evita que se procese un swap/subs/chgd para un buque subastado, best offer o x-piece.
                        RAISE e_avoid_oper_for_spec_booking;
                     END IF;

                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Secuencia Registro BK_RQST buque FROM='
                                 || TO_CHAR (vr_bk_rqsts_from.seq),
                                 'INFO'
                                );
                     --Búsqueda del itinerario real de tránsito del billing set
                     --de la reservación original del buque FROM. No se
                     --tomará como referencia el itinerario de tránsito en el campo
                     --INT_MOV_SEQ del TBR para búsqueda de información del
                     --itinerario porque el mismo puede haber sido variado en
                     --EVTMS
                     vn_iti_seq_from :=
                                sf_get_first_iti_tra (vr_bk_rqsts_from.bs_seq);
                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Vn_iti_seq_From='
                                 || TO_CHAR (vn_iti_seq_from),
                                 'INFO'
                                );
                     --Búsqueda de información de ITIN_ITEMS del barco FROM
                     p_get_bk_itin_info (vr_wb_bk_request_from.visit_seq,
                                         vn_iti_seq_from,
                                         vn_bs_seq_from,
                                         vv_rest_from,
                                         vv_iti_trn_dir_from,
                                         vv_iti_cust_cd_from,
                                         vv_iti_pde_cd_from
                                        );
                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_bs_seq_from=' || TO_CHAR (vn_bs_seq_from),
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_rest_from=' || vv_rest_from,
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_trn_dir_from=' || vv_iti_trn_dir_from,
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_cust_cd_from=' || vv_iti_cust_cd_from,
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_pde_cd_from=' || vv_iti_pde_cd_from,
                                 'INFO'
                                );
                     --Búsqueda de información de CUST_VSL_CHARS del buque FROM
                     p_get_csn_vsl_chrs (vr_wb_bk_request_from.ship_no,
                                         vr_wb_bk_request_from.visit_seq,
                                         vv_hml_from,
                                         vn_ext_beam_from,
                                         vv_gas_free_from
                                        );
                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_hml_from=' || vv_hml_from,
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vn_ext_beam_from='
                                 || TO_CHAR (vn_ext_beam_from),
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_gas_free_from=' || vv_gas_free_from,
                                 'INFO'
                                );
                     --S15142: Added on July 05, 2007 -- GNv
                     --        De acuerdo a MTPalma, buscar tipos de buque para
                     --        considerar igualar hora de arribo de buque Passenger
                     --        con la del otro buque que no es Passenger para
                     --        obviar validación de Same Required Arrival Time
                     vv_vsl_typ_from :=
                              sf_get_vsl_typ (vr_wb_bk_request_from.visit_seq);
                     --Búsqueda de arrival time del buque FROM de acuerdo al booking
                     vv_req_arr_from :=
                        f_get_arr_par (vn_bs_seq_from,
                                       vn_ext_beam_from,
                                       vv_hml_from,
                                       vv_rest_from
                                      );
                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_req_arr_from=' || vv_req_arr_from,
                                 'INFO'
                                );
                     --Búsqueda de información de BK_RQSTS del buque TO
                     p_get_bk_rqst_info (vr_wb_bk_request_to.org_bk_id,
                                         vr_bk_rqsts_to
                                        );

                     --
                     -- Evalua si el request es de Auction, Best Offer o X-Piece.
                     IF    vr_bk_rqsts_to.fix_amt IS NOT NULL
                        OR pkg_book_reftab_util.f_is_bestoffer
                                                      (vr_bk_rqsts_to.bsps_seq) =
                                                                           'Y'
                        OR pkg_book_reftab_util.f_is_xpiece
                                                      (vr_bk_rqsts_to.bsps_seq) =
                                                                           'Y'
                     THEN
                        --
                        -- S61519
                        -- VJaen 15Ene2008
                        -- Evita que se procese un swap/subs/chgd para un buque subastado, best offer o x-piece.
                        RAISE e_avoid_oper_for_spec_booking;
                     END IF;

                     --
                     -- S61519
                     -- VJaen 16Ene2008
                     -- Evalúa que el tipo de booking sea igual para ambos buques.
                     IF pkg_book_reftab_util.f_get_bk_size_typ
                                                    (vr_bk_rqsts_from.bsps_seq) <>
                           pkg_book_reftab_util.f_get_bk_size_typ
                                                      (vr_bk_rqsts_to.bsps_seq)
                     THEN
                        RAISE e_book_typ_must_be_equal;
                     END IF;

                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Secuencia Registro BK_RQST buque TO='
                                 || TO_CHAR (vr_bk_rqsts_to.seq),
                                 'INFO'
                                );
                     --Búsqueda del itinerario real de tránsito del billing set
                     --de la reservación original del buque TO. No se
                     --tomará como referencia el itinerario de tránsito en el campo
                     --INT_MOV_SEQ del TBR para búsqueda de información del
                     --itinerario porque el mismo puede haber sido variado en
                     --EVTMS
                     vn_iti_seq_to :=
                                  sf_get_first_iti_tra (vr_bk_rqsts_to.bs_seq);
                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'Vn_iti_seq_to=' || TO_CHAR (vn_iti_seq_to),
                                 'INFO'
                                );
                     --Búsqueda de información de ITIN_ITEMS del barco TO
                     p_get_bk_itin_info (vr_wb_bk_request_to.visit_seq,
                                         vn_iti_seq_to,
                                         vn_bs_seq_to,
                                         vv_rest_to,
                                         vv_iti_trn_dir_to,
                                         vv_iti_cust_cd_to,
                                         vv_iti_pde_cd_to
                                        );
                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_bs_seq_to=' || TO_CHAR (vn_bs_seq_to),
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_rest_to=' || vv_rest_to,
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_trn_dir_to=' || vv_iti_trn_dir_to,
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_cust_cd_to=' || vv_iti_cust_cd_to,
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_pde_cd_to=' || vv_iti_pde_cd_to,
                                 'INFO'
                                );
                     --Búsqueda de información de CUST_VSL_CHARS del buque TO
                     p_get_csn_vsl_chrs (vr_wb_bk_request_to.ship_no,
                                         vr_wb_bk_request_to.visit_seq,
                                         vv_hml_to,
                                         vn_ext_beam_to,
                                         vv_gas_free_to
                                        );
                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_hml_to=' || vv_hml_to,
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_ext_beam_to=' || TO_CHAR (vn_ext_beam_to),
                                 'INFO'
                                );
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_gas_free_to=' || vv_gas_free_to,
                                 'INFO'
                                );
                     --S15142: Added on July 05, 2007 -- GNv
                     --        De acuerdo a MTPalma, buscar tipos de buque para
                     --        considerar igualar hora de arribo de buque Passenger
                     --        con la del otro buque que no es Passenger para
                     --        obviar validación de Same Required Arrival Time
                     vv_vsl_typ_to :=
                                sf_get_vsl_typ (vr_wb_bk_request_to.visit_seq);
                     --Búsqueda de arrival time del buque TO de acuerdo al booking
                     vv_req_arr_to :=
                        f_get_arr_par (vn_bs_seq_to,
                                       vn_ext_beam_to,
                                       vv_hml_to,
                                       vv_rest_to
                                      );
                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_req_arr_to=' || vv_req_arr_to,
                                 'INFO'
                                );

                     --Se continúa el proceso si la información de booking existe con
                     --estatus BKD
                     IF     vr_bk_rqsts_from.seq IS NOT NULL
                        AND vr_bk_rqsts_to.seq IS NOT NULL
                     THEN
                        --Determinar secuencia de booking del registro del que el buque
                        --origen (from) tuviera un swap si aplicara
                        vi_br_seq_from_son :=
                             f_bk_find_swap (vr_wb_bk_request_from.org_bk_id);
                        --DBMS
                        pkg_plog.sec_implementation
                               (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                || p_bk_wbp_seq
                                || ')',
                                   'vi_br_seq_from_son='
                                || TO_CHAR (vi_br_seq_from_son),
                                'INFO'
                               );
                        pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vr_wb_bk_request_from.org_bk_id='
                                 || TO_CHAR (vr_wb_bk_request_from.org_bk_id),
                                 'INFO'
                                );
                        --Determinar secuencia de booking del registro del que el buque
                        --destino (to) tuviera un swap si aplicara
                        vi_br_seq_to_son :=
                                f_bk_find_swap (vr_wb_bk_request_to.org_bk_id);
                        --DBMS
                        pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vi_br_seq_to_son='
                                 || TO_CHAR (vi_br_seq_to_son),
                                 'INFO'
                                );
                        pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vr_wb_bk_request_to.org_bk_id='
                                 || TO_CHAR (vr_wb_bk_request_to.org_bk_id),
                                 'INFO'
                                );

                        --Validación con respecto a Swap previamente realizados
                        --con estas reservaciones

                        /*Changed on October 24, 2006 -- GNV
                        IF ( ( vi_br_seq_from_son IS NOT NULL ) AND ( vi_br_seq_to_son IS NULL )          AND
                             ( vi_br_seq_from_son               !=    vr_wb_bk_request_to.org_bk_id ) )    OR
                           ( ( vi_br_seq_from_son IS NULL     ) AND ( vi_br_seq_to_son IS NOT NULL )      AND
                             ( vi_br_seq_to_son                 !=    vr_wb_bk_request_from.org_bk_id ) )  OR
                           ( ( vr_wb_bk_request_from.org_bk_id IS NULL )     AND ( vr_wb_bk_request_to.org_bk_id  IS NOT NULL ) ) OR
                           ( ( vr_wb_bk_request_from.org_bk_id IS NOT NULL ) AND ( vr_wb_bk_request_to.org_bk_id  IS NULL) )
                        THEN
                        */
                        IF    (    (vi_br_seq_from_son IS NOT NULL)
                               AND (vi_br_seq_to_son IS NULL)
                              )
                           OR (    (vi_br_seq_from_son IS NULL)
                               AND (vi_br_seq_to_son IS NOT NULL)
                              )
                        THEN
                           RAISE e_bk_swap_prev_one;
                        -- Caso que se refiere a cuando ambos buques tienen un swap previo
                        /*Changed on October 24, 2006 -- GNV
                        ELSIF ( vi_br_seq_to_son != vr_wb_bk_request_from.org_bk_id ) AND
                              ( vi_br_seq_from_son != vr_wb_bk_request_to.org_bk_id )  THEN
                        */
                        ELSIF (    (vi_br_seq_from_son IS NOT NULL)
                               AND (vi_br_seq_to_son IS NOT NULL)
                              )
                        THEN
                           RAISE e_bk_swap_prev_both;
                        END IF;

                        --S15142: Added on July 05, 2007 -- GNV
                        --        Igualar hora de arribo de puque Passenger a la
                        --        hora de arribo del otro buque (no Passenger) para
                        --        obviar validación de Same Required Arrival Time

                        --Si tipos de buques son desiguales
                        IF NVL (vv_vsl_typ_from, ' ') <>
                                                      NVL (vv_vsl_typ_to, ' ')
                        THEN
                           --Si alguno de los dos buques corresponde a un Passenger Ship
                           IF    NVL (vv_vsl_typ_from, ' ') = '11'
                              OR NVL (vv_vsl_typ_to, ' ') = '11'
                           THEN
                              --Si las horas requeridas de arribo son desiguales
                              IF NVL (vv_req_arr_from, ' ') <>
                                                     NVL (vv_req_arr_to, ' ')
                              THEN
                                 --Si buque FROM es un Passenger Ship
                                 IF NVL (vv_vsl_typ_from, ' ') = '11'
                                 THEN
                                    --Si hora de arribo de buque TO es NOT NULL
                                    IF vv_req_arr_to IS NOT NULL
                                    THEN
                                       --Asignar hora de arribo de buque TO a buque FROM
                                       vv_req_arr_from := vv_req_arr_to;
                                    --Si hora de arribo de buque TO es NULL
                                    ELSE
                                       --Si hora de arribo de buque FROM es NOT NULL
                                       IF vv_req_arr_from IS NOT NULL
                                       THEN
                                          --Asignar hora de arribo de buque FROM a buque TO
                                          vv_req_arr_to := vv_req_arr_from;
                                       END IF;
                                    --IF vv_req_arr_from IS NOT NULL THEN
                                    END IF;
                                           --IF vv_req_arr_to IS NOT NULL THEN
                                 --Si buque TO es un Passenger Ship
                                 ELSIF NVL (vv_vsl_typ_to, ' ') = '11'
                                 THEN
                                    --Si hora de arribo de buque FROM es NOT NULL
                                    IF vv_req_arr_from IS NOT NULL
                                    THEN
                                       --Asignar hora de arribo de buque FROM a buque TO
                                       vv_req_arr_to := vv_req_arr_from;
                                    --Si hora de arribo de buque FROM es NULL
                                    ELSE
                                       --Si hora de arribo de buque TO es NOT NULL
                                       IF vv_req_arr_to IS NOT NULL
                                       THEN
                                          --Asignar hora de arribo de buque TO a buque FROM
                                          vv_req_arr_from := vv_req_arr_to;
                                       END IF;
                                    --IF vv_req_arr_to IS NOT NULL THEN
                                    END IF;
                                 --IF vv_req_arr_from IS NOT NULL THEN
                                 END IF;
                              --IF NVL(vv_vsl_typ_from,' ') = '11' THEN
                              END IF;
                           --IF NVL(vv_req_arr_from,' ') <> NVL(vv_req_arr_to,' ') THEN
                           END IF;     --IF NVL(vv_vsl_typ_from,' ') = '11' OR
                        END IF;

                        --IF NVL(vv_vsl_typ_from,' ')  <> NVL(vv_vsl_typ_to,' ') THEN

                        --Cambio del criterio de restricción del buque ORIGEN (Y/N)
                        IF INSTR(PKG_BOOK_DATADICT.f_hml_rest_list, vv_hml_from) > 0 THEN
                            vv_hml_rest_from := 'Y';
                        ELSE
                            vv_hml_rest_from := 'N';
                        END IF;
                         --Cambio del criterio de restricción del buque DESTINO (Y/N)
                        IF INSTR(PKG_BOOK_DATADICT.f_hml_rest_list, vv_hml_to) > 0 THEN
                            vv_hml_rest_to := 'Y';
                        ELSE
                            vv_hml_rest_to := 'N';
                        END IF;
                        --

                        --Evaluación de Reglas Generales por tipo de transacción
                        p_eval_bk_tran_rules
                                           (p_bk_wbp_typ,
                                            vr_wb_bk_request_from.ship_no,
                                            vr_wb_bk_request_from.cust_cd,
                                            vr_wb_bk_request_from.trn_dir,
                                            vv_rest_from,
                                            vv_iti_trn_dir_from,
                                            vv_iti_cust_cd_from,
                                            vv_req_arr_from,
                                            vr_wb_bk_request_from.org_bk_date,
                                            vr_bk_rqsts_from.fix_amt,
                                            vr_wb_bk_request_to.ship_no,
                                            vr_wb_bk_request_to.cust_cd,
                                            vr_wb_bk_request_to.trn_dir,
                                            vv_rest_to,
                                            vv_iti_trn_dir_to,
                                            vv_iti_cust_cd_to,
                                            vv_req_arr_to,
                                            vr_wb_bk_request_to.org_bk_date,
                                            vr_bk_rqsts_to.fix_amt,
                                            vv_error_cd,
                                            vv_hml_rest_from,
                                            vv_hml_rest_to,
                                            sf_get_bk_typ(vn_ext_beam_from),
                                            sf_get_bk_typ(vn_ext_beam_to)
                                           );
                        --DBMS
                        pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Regrese de p_eval_bk_tran con error'
                                 || vv_error_cd,
                                 'INFO'
                                );

                        IF vv_error_cd IS NULL
                        THEN
                           --Si no sucedio ningún error, actualización de la
                           --información para completar swap de booking

                           --Determinación de cargo mayor de Booking tomando en cuenta subasta

                           --Validación de si el booking del buque FROM
                           --contenia un FIX AMT (cargo de booking por subasta)
                           --con el cual se comparará el cargo de booking a
                           --traves de tarifas o subasta (FIX_AMT) del buque TO

                           --Evaluación de BUQUE FROM para determinar si cargo de booking
                           --corresponde a cantidad de subasta o fee por tarifas
                           IF NVL (vr_bk_rqsts_from.fix_amt, 0) > 0
                           THEN
                              vn_bk_wb_fee_from := vr_bk_rqsts_from.fix_amt;
                              vv_bk_fix_amt_from := 'Y';
                              --DBMS
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                     'Subasta vn_bk_wb_fee_from='
                                  || TO_CHAR (vn_bk_wb_fee_from),
                                  'INFO'
                                 );
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'Vv_bk_fix_amt_from=' || vv_bk_fix_amt_from,
                                  'INFO'
                                 );
                           ELSE
                              vn_bk_wb_fee_from :=
                                                f_get_bk_fee (vn_bs_seq_from);
                              --DBMS
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                     'Tarifa vn_bk_wb_fee_from='
                                  || TO_CHAR (vn_bk_wb_fee_from),
                                  'INFO'
                                 );
                           END IF;

                           --Evaluación de BUQUE TO para determinar si cargo de booking
                           --corresponde a cantidad de subasta o fee por tarifas
                           IF NVL (vr_bk_rqsts_to.fix_amt, 0) > 0
                           THEN
                              vn_bk_wb_fee_to := vr_bk_rqsts_to.fix_amt;
                              vv_bk_fix_amt_to := 'Y';
                              --DBMS
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                     'Subasta vn_bk_wb_fee_to='
                                  || TO_CHAR (vn_bk_wb_fee_to),
                                  'INFO'
                                 );
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'vv_bk_fix_amt_to=' || vv_bk_fix_amt_to,
                                  'INFO'
                                 );
                           ELSE
                              vn_bk_wb_fee_to := f_get_bk_fee (vn_bs_seq_to);
                              --DBMS
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                     'Tarifa vn_bk_wb_fee_to='
                                  || TO_CHAR (vn_bk_wb_fee_to),
                                  'INFO'
                                 );
                           END IF;

                           --Se determinará si del fee entre los dos buques
                           --es mayor la cantidad de subasta si alguno de los
                           --dos la presenta. De ser así se actualizará la
                           --cantidad mayor en la columna FIX_AMT del registro
                           --de BK_RQSTS de ambos buques
                           IF NVL (vn_bk_wb_fee_from, 0) >=
                                                      NVL (vn_bk_wb_fee_to, 0)
                           THEN
                              vn_bk_wb_fee := vn_bk_wb_fee_from;
                           ELSE
                              vn_bk_wb_fee := vn_bk_wb_fee_to;
                           END IF;

                           --IF NVL(vn_bk_wb_fee_from,0) >= NVL(vn_bk_wb_fee_to,0) THEN

                           --DBMS
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Vn_bk_wb_fee_final='
                                 || TO_CHAR (vn_bk_wb_fee),
                                 'INFO'
                                );
                           --Construcción de remarks
                           vv_bk_rmk_from :=
                                 'Vessel has been ''swapped'' with SIN '
                              || TO_CHAR (vr_wb_bk_request_to.ship_no);
                           vv_bk_rmk_to :=
                                 'Vessel has been ''swapped'' with SIN '
                              || TO_CHAR (vr_wb_bk_request_from.ship_no);
                           --DBMS
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'Rmk From ' || vv_bk_rmk_from,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'Rmk to ' || vv_bk_rmk_to,
                                 'INFO'
                                );

                           --Actualizando información en registros de BK_RQSTS del buque TO
                           --Se actualizan la información directamente a la tabla sin la vista
                           --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                           --UPDATE inclusive la columna de STAT, y en estos casos no se está
                           --haciendo update a dicha columna, por lo que el trigger de la
                           --misma restringe el uso de esta forma.
                           UPDATE bk_rqsts
                              SET bd_seq = vr_bk_rqsts_from.bd_seq,
                                  rqst_bk_date =
                                             vr_wb_bk_request_from.org_bk_date,
                                  stat_rmk = vv_bk_rmk_to
                            WHERE seq = vr_wb_bk_request_to.org_bk_id
                              AND stat = PKG_BOOKING.cv_bk_bkd_stat;

                           /*       br_seq_swap_from =
                                               vr_wb_bk_request_from.org_bk_id,
                                  bs_seq_swap = vn_bs_seq_from */
                           IF SQL%FOUND
                           THEN
                              --DBMS
                              DBMS_OUTPUT.put_line
                                                ('Hecho update de BK_RQST TO');

                              --
                              -- S61519
                              -- VJaen 15Jan2008.
                              -- Inserta el registro detalle de SWAP para el buque TO.
                              DECLARE
                                 vr_to_det   vw_bk_rqst_dets%ROWTYPE;
                              BEGIN
                                 pkg_plog.sec_implementation
                                    (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                     || p_bk_wbp_seq
                                     || ')',
                                     'Inserta el registro detalle de SWAP para el buque TO.',
                                     'INFO'
                                    );
                                 -- Llena la variable de registro.
                                 vr_to_det.br_seq :=
                                                 vr_wb_bk_request_to.org_bk_id;
                                 vr_to_det.bs_seq := vr_bk_rqsts_to.bs_seq;
                                 vr_to_det.trns_type :=
                                                  pkg_book_datadict.f_bkp_swap;
                                 vr_to_det.br_seq_dest :=
                                               vr_wb_bk_request_from.org_bk_id;
                                 vr_to_det.bs_seq_dest := vn_bs_seq_from;
                                 vr_to_det.amount := vn_bk_wb_fee;
                                 -- Solicita la inserción del registro.
                                 p_ins_bk_rqst_dets (vr_to_det);
                              END;

/*--------------------------------------*/
/*MANEJO DE SWAP PARA BUQUES SUBASTADOS */
/*--------------------------------------*/

                              --Update de fix amount de subasta para monto mayor de booking fee si aplica
                              --El Fix Amount será trasladado de un buque a otro ya que el slot swappeado
                              --permanece con la condición de subasta (si la tiene). En este caso el monto
                              --final grabado en el Fix Amount será el fee mayor entre dos buques (dependiendo
                              --si se comparan los Fix Amount o los fee por tarifa)

                              /*MANEJO DEL BUQUE TO*/
                              IF vv_bk_fix_amt_from = 'Y'
                              THEN
                                 /*Traslado Buque TO a Buque FROM*/
                                 --Caso #1
                                 --Buque From = Subasta
                                 --Buque To   = Normal
                                 --Se actualiza Fix Amount a Buque TO con el mayor fee
                                 --Debido a que el Traslado del Slot hereda condicion
                                 --Subasta. El Fix Amount del Buque From será blanqueado

                                 --Caso #2
                                 --Buque From = Subasta
                                 --Buque To   = Subasta
                                 --Se actualiza Fix Amount a Buque TO con el mayor fee
                                 --Debido a que el Traslado del Slot hereda condicion
                                 --Subasta. El Fix Amount del Buque From no será blanqueado
                                 --si no actualizado también al mayor fee

                                 --Se actualizan la información directamente a la tabla sin la vista
                                 --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                                 --UPDATE inclusive la columna de STAT, y en estos casos no se está
                                 --haciendo update a dicha columna, por lo que el trigger de la
                                 --misma restringe el uso de esta forma.
                                 UPDATE bk_rqsts
                                    SET fix_amt = vn_bk_wb_fee
                                  WHERE seq = vr_wb_bk_request_to.org_bk_id
                                    AND stat =
                                              PKG_BOOKING.cv_bk_bkd_stat;

                                 --DBMS
                                 DBMS_OUTPUT.put_line
                                    ('Hecho update de FIX AMT en BK_RQSTS TO # 1'
                                    );
                              ELSE
                                 IF vv_bk_fix_amt_to = 'Y'
                                 THEN
                                    /*Traslado Buque TO a Buque FROM*/
                                    --Caso #3
                                    --Buque From = Normal
                                    --Buque To   = Subasta
                                    --Se actualiza Fix Amount a NULL de Buque TO
                                    --Debido a que el Traslado del Slot hereda condicion
                                    --Subasta. El Fix Amount del Buque From será llenado
                                    --con el monto mayor de fee debido a que heredará la
                                    --condición del Subasta del Slot del Buque TO

                                    --Caso #4
                                    --Buque From = Normal
                                    --Buque To   = Normal
                                    --No se actualiza Fix Amount de Buque TO ni buque FROM
                                    --ya que ninguno fue subastado

                                    --Se actualizan la información directamente a la tabla sin la vista
                                    --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                                    --UPDATE inclusive la columna de STAT, y en estos casos no se está
                                    --haciendo update a dicha columna, por lo que el trigger de la
                                    --misma restringe el uso de esta forma.
                                    UPDATE bk_rqsts
                                       SET fix_amt = NULL
                                     WHERE seq = vr_wb_bk_request_to.org_bk_id
                                       AND stat =
                                              PKG_BOOKING.cv_bk_bkd_stat;

                                    --DBMS
                                    DBMS_OUTPUT.put_line
                                       ('Hecho update de FIX AMT en BK_RQSTS TO #2'
                                       );
                                 END IF;      --IF vv_bk_fix_amt_to = 'Y' THEN
                              END IF;       --IF vv_bk_fix_amt_from = 'Y' THEN

                              --Update de Daylight Transit Bookings Garantizados
                              --de la reservación original (si aplica) para el buque TO
                              vv_error_cd :=
                                 f_void_bk_dlt (vr_wb_bk_request_to.org_bk_id,
                                                p_bk_wbp_typ
                                               );
                              --DBMS
                              DBMS_OUTPUT.put_line
                                     (   'Error Cd retorno de UPD Daylight 2='
                                      || vv_error_cd
                                     );

                              IF vv_error_cd IS NULL
                              THEN
                                 --Update de buque TO se pudo completar
                                 vv_upd_to := 'Y';
                              END IF;            --IF vv_error_cd IS NULL THEN
                           END IF;                         --IF SQL%FOUND THEN

                           --DBMS
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_upd_to retorno=' || vv_upd_to,
                                 'INFO'
                                );

                           --Actualizando información en registros de BK_RQSTS del buque FROM
                           --Se actualizan la información directamente a la tabla sin la vista
                           --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                           --UPDATE inclusive la columna de STAT, y en estos casos no se está
                           --haciendo update a dicha columna, por lo que el trigger de la
                           --misma restringe el uso de esta forma.
                           UPDATE bk_rqsts
                              SET bd_seq = vr_bk_rqsts_to.bd_seq,
                                  rqst_bk_date =
                                               vr_wb_bk_request_to.org_bk_date,
                                  --bs_seq_swap = vn_bs_seq_to,
                                  stat_rmk = vv_bk_rmk_from
                            WHERE seq = vr_wb_bk_request_from.org_bk_id
                              AND stat = PKG_BOOKING.cv_bk_bkd_stat;

                           IF SQL%FOUND
                           THEN
                              --DBMS
                              DBMS_OUTPUT.put_line
                                  ('Hecho update de FIX AMT en BK_RQSTS FROM');

                              --
                              -- S61519
                              -- VJaen 15Jan2008.
                              -- Inserta el registro detalle de SWAP para el buque TO.
                              DECLARE
                                 vr_from_det   vw_bk_rqst_dets%ROWTYPE;
                              BEGIN
                                 pkg_plog.sec_implementation
                                    (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                     || p_bk_wbp_seq
                                     || ')',
                                     'Inserta el registro detalle de SWAP para el buque FROM.',
                                     'INFO'
                                    );
                                 -- Llena la variable de registro.
                                 vr_from_det.br_seq :=
                                               vr_wb_bk_request_from.org_bk_id;
                                 vr_from_det.bs_seq := vr_bk_rqsts_from.bs_seq;
                                 vr_from_det.trns_type :=
                                                  pkg_book_datadict.f_bkp_swap;
                                 vr_from_det.br_seq_dest :=
                                                 vr_wb_bk_request_to.org_bk_id;
                                 vr_from_det.bs_seq_dest := vn_bs_seq_to;
                                 vr_from_det.amount := vn_bk_wb_fee;
                                 -- Solicita la inserción del registro.
                                 p_ins_bk_rqst_dets (vr_from_det);
                              END;

                              /*MANEJO DEL BUQUE FROM*/
                              IF vv_bk_fix_amt_to = 'Y'
                              THEN
                                 /*Traslado Buque FROM a Buque TO*/
                                 --Caso #1
                                 --Buque From = Normal
                                 --Buque To   = Subasta
                                 --Se actualiza Fix Amount a Buque From con el mayor fee
                                 --Debido a que el Traslado del Slot hereda condicion
                                 --Subasta. El Fix Amount del Buque To será blanqueado

                                 --Caso #2
                                 --Buque From = Subasta
                                 --Buque To   = Subasta
                                 --Se actualiza Fix Amount a Buque From con el mayor fee
                                 --Debido a que el Traslado del Slot hereda condicion
                                 --Subasta. El Fix Amount del Buque To no será blanqueado
                                 --si no actualizado también al mayor fee

                                 --Se actualizan la información directamente a la tabla sin la vista
                                 --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                                 --UPDATE inclusive la columna de STAT, y en estos casos no se está
                                 --haciendo update a dicha columna, por lo que el trigger de la
                                 --misma restringe el uso de esta forma.
                                 UPDATE bk_rqsts
                                    SET fix_amt = vn_bk_wb_fee
                                  WHERE seq = vr_wb_bk_request_from.org_bk_id
                                    AND stat =
                                              PKG_BOOKING.cv_bk_bkd_stat;

                                 --DBMS
                                 DBMS_OUTPUT.put_line
                                    ('Hecho update de FIX AMT en BK_RQSTS FROM  #3'
                                    );
                              ELSE
                                 IF vv_bk_fix_amt_from = 'Y'
                                 THEN
                                     /*Traslado Buque FROM a Buque To*/
                                    --Caso #3
                                    --Buque From = Subasta
                                    --Buque To   = Normal
                                    --Se actualiza Fix Amount a NULL de Buque FROM
                                    --Debido a que el Traslado del Slot hereda condicion
                                    --Subasta. El Fix Amount del Buque TO será llenado
                                    --con el monto mayor de fee debido a que heredará la
                                    --condición del Subasta del Slot del Buque FROM

                                    --Caso #4
                                    --Buque From = Normal
                                    --Buque To   = Normal
                                    --No se actualiza Fix Amount de Buque FROM ni buque TO
                                    --ya que ninguno fue subastado

                                    --Se actualizan la información directamente a la tabla sin la vista
                                    --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                                    --UPDATE inclusive la columna de STAT, y en estos casos no se está
                                    --haciendo update a dicha columna, por lo que el trigger de la
                                    --misma restringe el uso de esta forma.
                                    UPDATE bk_rqsts
                                       SET fix_amt = NULL
                                     WHERE seq =
                                               vr_wb_bk_request_from.org_bk_id
                                       AND stat =
                                              PKG_BOOKING.cv_bk_bkd_stat;

                                    --DBMS
                                    DBMS_OUTPUT.put_line
                                       ('Hecho update de FIX AMT en BK_RQSTS FROM #4'
                                       );
                                 END IF;      --IF vv_bk_fix_amt_to = 'Y' THEN
                              END IF;         --IF vv_bk_fix_amt_to = 'Y' THEN

                              --Update de Daylight Transit Bookings Garantizados
                              --de la reservación original (si aplica) para el buque FROM
                              vv_error_cd :=
                                 f_void_bk_dlt
                                             (vr_wb_bk_request_from.org_bk_id,
                                              p_bk_wbp_typ
                                             );
                              --DBMS
                              DBMS_OUTPUT.put_line
                                     (   'Error Cd retorno de UPD Daylight 3='
                                      || vv_error_cd
                                     );

                              IF vv_error_cd IS NULL
                              THEN
                                 --Update de buque FROM se pudo completar
                                 vv_upd_from := 'Y';
                              END IF;            --IF vv_error_cd IS NULL THEN
                           END IF;                         --IF SQL%FOUND THEN

                           --DBMS
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_upd_from retorno=' || vv_upd_from,
                                 'INFO'
                                );

                           IF vv_upd_from = 'N' OR vv_upd_to = 'N'
                           THEN
                              vn_br_wb_seq_first :=
                                              vr_wb_bk_request_from.org_bk_id;
                              vn_br_wb_seq_sec :=
                                                vr_wb_bk_request_to.org_bk_id;
                              vn_bk_wb_fee := NULL;
                              vv_bk_wb_stat :=
                                        PKG_BOOKING.cv_trans_stat_false;
                           ELSE
                              vn_br_wb_seq_first :=
                                              vr_wb_bk_request_from.org_bk_id;
                              vn_br_wb_seq_sec :=
                                                vr_wb_bk_request_to.org_bk_id;
                              /* Booking fee debio haber sido calculado ya desde
                              la variable vn_bk_wb_fee*/
                              vv_bk_wb_stat :=
                                         PKG_BOOKING.cv_trans_stat_true;
                           END IF;                --IF vv_no_upd_from = 'N' OR
                        ELSE
                           vn_br_wb_seq_first :=
                                              vr_wb_bk_request_from.org_bk_id;
                           vn_br_wb_seq_sec := vr_wb_bk_request_to.org_bk_id;
                           vn_bk_wb_fee := NULL;
                           vv_bk_wb_stat :=
                                        PKG_BOOKING.cv_trans_stat_false;
                        END IF;                  --IF vv_error_cd IS NULL THEN
                     ELSE
                        vn_br_wb_seq_first := vr_wb_bk_request_from.org_bk_id;
                        vn_br_wb_seq_sec := vr_wb_bk_request_to.org_bk_id;
                        vn_bk_wb_fee := NULL;
                        vv_bk_wb_stat :=
                                        PKG_BOOKING.cv_trans_stat_false;
                     END IF;         --IF vr_bk_rqsts_from.seq IS NOT NULL AND
                  ELSE
                     vn_br_wb_seq_first := vr_wb_bk_request_from.org_bk_id;
                     vn_br_wb_seq_sec := vr_wb_bk_request_to.org_bk_id;
                     vn_bk_wb_fee := NULL;
                     vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
                  END IF;                --IF c_web_bk_requests_to%ISOPEN THEN
               ELSE
                  vn_br_wb_seq_first := vr_wb_bk_request_from.org_bk_id;
                  vn_br_wb_seq_sec := vr_wb_bk_request_to.org_bk_id;
                  vn_bk_wb_fee := NULL;
                  vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
               END IF;           --IF vr_web_request_from.seq IS NOT NULL THEN
            ELSE
               vn_br_wb_seq_first := vr_wb_bk_request_from.org_bk_id;
               vn_br_wb_seq_sec := vr_wb_bk_request_to.org_bk_id;
               vn_bk_wb_fee := NULL;
               vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            END IF;                         --IF c_web_bk_requests%ISOPEN THEN
         ELSE
            vn_br_wb_seq_first := vr_wb_bk_request_from.org_bk_id;
            vn_br_wb_seq_sec := vr_wb_bk_request_to.org_bk_id;
            vn_bk_wb_fee := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
         END IF;                                 --IF p_bk_wbp_seq IS NOT NULL
      EXCEPTION
         WHEN e_bk_swap_prev_one
         THEN
            vn_br_wb_seq_first := vr_wb_bk_request_from.org_bk_id;
            vn_br_wb_seq_sec := vr_wb_bk_request_to.org_bk_id;
            vn_bk_wb_fee := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            vv_error_cd := 'USR-30303';
         --Para enviar el código de error se desactiva estas programacion
         --Pkg_Evtms_Db_Util.p_generic_exception('USR-30303','This vessel has been swapped previously.');
         WHEN e_bk_swap_prev_both
         THEN
            vn_br_wb_seq_first := vr_wb_bk_request_from.org_bk_id;
            vn_br_wb_seq_sec := vr_wb_bk_request_to.org_bk_id;
            vn_bk_wb_fee := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            vv_error_cd := 'USR-30304';
         --Para enviar el código de error se desactiva estas programacion
         --Pkg_Evtms_Db_Util.p_generic_exception('USR-30304','Both Vessels have been swapped previously.');
         WHEN e_avoid_oper_for_spec_booking
         THEN
            vn_br_wb_seq_first := vr_wb_bk_request_from.org_bk_id;
            vn_br_wb_seq_sec := vr_wb_bk_request_to.org_bk_id;
            vn_bk_wb_fee := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            pkg_plog.sec_implementation
               ('PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ=' || p_bk_wbp_seq
                || ')',
                'Current version of the system does not allow SWAP/SUBSTITUTION/CHANGE DATE operations for Auction, Best Offer or X-Piece Bookings.',
                'INFO'
               );
            -- Current version of the system does not allow SWAP/SUBSTITUTION/CHANGE DATE operations for Auction, Best Offer or X-Piece Bookings.
            vv_error_cd := 'USR-02031';
         --Para enviar el código de error se desactiva estas programacion
         --Pkg_Evtms_Db_Util.p_generic_exception('USR-30304','Both Vessels have been swapped previously.');
         WHEN e_book_typ_must_be_equal
         THEN
            vn_br_wb_seq_first := vr_wb_bk_request_from.org_bk_id;
            vn_br_wb_seq_sec := vr_wb_bk_request_to.org_bk_id;
            vn_bk_wb_fee := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            pkg_plog.sec_implementation
               ('PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ=' || p_bk_wbp_seq
                || ')',
                'Booking types must be the same for both vessels when doing SWAP or SUBSTITUTION.',
                'INFO'
               );
            -- Booking types must be the same for both vessels when doing SWAP or SUBSTITUTION.
            vv_error_cd := 'USR-02032';
      END;

      p_br_wb_seq_first := vn_br_wb_seq_first;
      p_br_wb_seq_sec := vn_br_wb_seq_sec;
      p_bk_wb_fee := vn_bk_wb_fee;
      p_bk_wb_stat := vv_bk_wb_stat;
      p_error_cd := vv_error_cd;
      --DBMS
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_SWAP p_br_wb_seq_first ='
                                 || TO_CHAR (vn_br_wb_seq_first),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_SWAP p_br_wb_seq_sec ='
                                 || TO_CHAR (vn_br_wb_seq_sec),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_SWAP p_bk_wb_fee ='
                                 || TO_CHAR (vn_bk_wb_fee),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'BOOK_SWAP p_bk_wb_stat =' || vv_bk_wb_stat,
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'BOOK_SWAP p_error_cd =' || vv_error_cd,
                                 'INFO'
                                );
   END p_book_trans_swap;
/* Generate the booking substitution */
PROCEDURE P_BOOK_TRANS_SUBS
 (P_BK_WBP_SEQ IN WEB_BK_PROCS.SEQ%TYPE
 ,P_BK_WBP_TYP IN WEB_BK_PROCS.TYP%TYPE
 ,P_BK_WB_SEQ_FIRST OUT BK_RQSTS.SEQ%TYPE
 ,P_BK_WB_FEE OUT CHARGES.CHRG_AMT%TYPE
 ,P_CAN_WB_DY OUT WEB_BK_RQST_TRANS.BR_CANCL_DY%TYPE
 ,P_CAN_WB_HR OUT WEB_BK_RQST_TRANS.BR_CANCL_HR%TYPE
 ,P_CAN_WB_FEE OUT CHARGES.CHRG_AMT%TYPE
 ,P_BK_WB_STAT OUT WEB_BK_REQUESTS.TRANS_STAT%TYPE
 ,P_BK_COND OUT BK_CONDS.LEV%TYPE
 ,P_ERROR_CD OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
--   Este procedimiento se encarga realizar la transacción de  Substitution  de Reservaciones
--  (Bookings) de acuerdo a un TBR (Transit Booking Requests)  de un buque para --   sustituir la reservación de otro generado desde EDCS
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador    Fecha                  Cambios realizados
-- -------------   --------------------  -------------------     -----------------------------
-- S15142      GVillarreal         01-SEP-2006      Versión Inicial
-- S61519      VJaen            15-Jan-2008    Prevencion de procesamiento para
--                                             booking especiales y grabado en tabla
--                                             de detalle.
--=============================================================================
      CURSOR c_web_bk_requests (p_bk_wbp_seq IN web_bk_procs.seq%TYPE)
      IS
         SELECT seq, visit_seq, ship_no, vty_cd, cust_cd, int_mov_seq,
                trn_dir, trn_br_date, trn_br_rqst_date, org_bk_date,
                org_bk_id, primary_ref, sec_ref, AGENT, fix_amt,
                new_extm_beam, dtu_int
           FROM web_bk_requests
          WHERE wbp_seq = p_bk_wbp_seq
            AND rqst_status = PKG_BOOKING.cv_rqst_status_pend;

      CURSOR c_web_bk_rqst_trans (
         p_bk_wbp_seq   IN   web_bk_procs.seq%TYPE,
         p_br_seq       IN   bk_rqsts.seq%TYPE
      )
      IS
         SELECT seq, visit_seq, ship_no, br_seq, cust_cd, trn_dir,
                br_cancl_date, trn_br_date, rqst_status
           FROM web_bk_rqst_trans
          WHERE wbp_seq = p_bk_wbp_seq
            AND br_seq = p_br_seq
            AND option_rqst = 'T'
            AND rqst_status = PKG_BOOKING.cv_rqst_status_pend;

      vr_wb_bk_requests               PKG_BOOKING.tr_web_bk_requests;
      vr_wb_bk_rqst_trans             PKG_BOOKING.tr_web_bk_rqst_trans;
      vr_bk_rqsts_from                PKG_BOOKING.tr_bk_rqsts;
      vn_br_wb_seq_first              bk_rqsts.seq%TYPE;
      vn_bk_wb_fee                    charges.chrg_amt%TYPE;
      vn_can_wb_dy                    web_bk_rqst_trans.br_cancl_dy%TYPE;
      vn_can_wb_hr                    web_bk_rqst_trans.br_cancl_hr%TYPE;
      vn_can_wb_fee                   charges.chrg_amt%TYPE;
      vn_can_dlt_fee                  web_bk_rqst_trans.daylight_trn_cancl_fee%TYPE;
      vv_bk_wb_stat                   web_bk_rqst_trans.trans_stat%TYPE;
      vn_iti_seq_from                 itin_items.seq%TYPE;
      vn_br_seq_from                  bk_rqsts.seq%TYPE;
      vn_visit_seq_from               cust_sched_needs.seq%TYPE;
      vv_trn_dir_from                 itin_items.tii_trn_dir%TYPE;
      vv_cust_cd_from                 VARCHAR2 (218);
      -- From Definition on function SF_GET_CUST_NAME
      vn_ship_no_from                 ship_id_no.ship_no%TYPE;
      vn_bs_seq_from                  bill_sets.seq%TYPE;
      vv_rest_from                    rest_usr_cds.cd%TYPE;
      -- From Definition on function SF_GET_REST_USER_BK
      vv_iti_trn_dir_from             itin_items.tii_trn_dir%TYPE;
      vv_iti_cust_cd_from             VARCHAR2 (218);
      -- From Definition on function SF_GET_CUST_NAME
      vv_iti_pde_cd_from              itin_items.pde_cd%TYPE;
      vv_hml_from                     cust_vsl_chars.lvc_hml_qualified%TYPE;
      vn_ext_beam_from                cust_vsl_chars.extm_beam%TYPE;
      vv_gas_free_from                cust_visit_dets.tblc_gas_free_ind%TYPE;
      vv_req_arr_from                 VARCHAR2 (5);
      -- From Definition de funcion PKG_BOOKING.F_GET_ARR_PAR;
      vv_stat_rmk_from                bk_rqsts.stat_rmk%TYPE;
      vn_bk_fee_from                  charges.chrg_amt%TYPE;
      vn_bk_fee_work_from             charges.chrg_amt%TYPE;
      vn_iti_seq_to                   itin_items.seq%TYPE;
      vn_bs_seq_to                    bill_sets.seq%TYPE;
      vv_rest_to                      rest_usr_cds.cd%TYPE;
      -- From Definition on function SF_GET_REST_USER_BK
      vv_iti_trn_dir_to               itin_items.tii_trn_dir%TYPE;
      vv_iti_cust_cd_to               VARCHAR2 (218);
      -- From Definition on function SF_GET_CUST_NAME
      vv_iti_pde_cd_to                itin_items.pde_cd%TYPE;
      vv_hml_to                       cust_vsl_chars.lvc_hml_qualified%TYPE;
      vn_ext_beam_to                  cust_vsl_chars.extm_beam%TYPE;
      vv_gas_free_to                  cust_visit_dets.tblc_gas_free_ind%TYPE;
      vv_req_arr_to                   VARCHAR2 (5);
      -- From Definition de funcion PKG_BOOKING.F_GET_ARR_PAR;
      vv_stat_rmk_to                  bk_rqsts.stat_rmk%TYPE;
      vv_rej_stat_rmk_to              bk_rqsts.stat_rmk%TYPE;
      vn_br_seq_to                    bk_rqsts.seq%TYPE;
      vv_br_stat_to                   bk_rqsts.stat%TYPE;
      vn_bk_fee_to                    charges.chrg_amt%TYPE;
      vn_bk_cond                      bk_conds.lev%TYPE;
      vv_temp_status                  web_bk_rqst_trans.rqst_status%TYPE;
      vv_success_tbc                  web_bk_requests.trans_stat%TYPE;
      vv_success_bkd                  web_bk_requests.trans_stat%TYPE;
      vv_success_bkd_to               web_bk_requests.trans_stat%TYPE;
      vv_bkd_fix_amt                  VARCHAR2 (1)                      := 'N';
      vv_upd_fix_amt                  VARCHAR2 (1)                      := 'N';
      vv_error_cd                     VARCHAR2 (30);
      e_subs_prev                     EXCEPTION;
      v_spc_bk_ind                    VARCHAR2 (5);
      --
      -- S61519
      -- VJaen 15Ene2008
      vr_bk_rqsts_to                  PKG_BOOKING.tr_bk_rqsts;
                                            -- Para obtener el BSPS_SEQ nuevo.
      e_avoid_oper_for_spec_booking   EXCEPTION;
      e_book_typ_must_be_equal        EXCEPTION;
--
--
vv_hml_rest_from                       VARCHAR2(1);
vv_hml_rest_to                       VARCHAR2(1);
BEGIN
      p_bk_wb_seq_first := NULL;
      p_bk_wb_fee := NULL;
      p_can_wb_dy := NULL;
      p_can_wb_hr := NULL;
      p_can_wb_fee := NULL;
      p_bk_wb_stat := NULL;
      p_bk_cond := NULL;
      p_error_cd := NULL;
      --S15142.T15 Added on August 08, 2007 -- GNV
      --Clear global variables before use
      pkg_global_vars.p_set_char4 (NULL);
      pkg_global_vars.p_set_char5 (NULL);

      BEGIN
         IF p_bk_wbp_seq IS NOT NULL
         THEN
            --Búsqueda de información del TBR del buque que va a sustituir (TO)
            OPEN c_web_bk_requests (p_bk_wbp_seq);

            IF c_web_bk_requests%ISOPEN
            THEN
               FETCH c_web_bk_requests
                INTO vr_wb_bk_requests;

               IF c_web_bk_requests%NOTFOUND
               THEN
                  vr_wb_bk_requests := NULL;
               END IF;                    --IF c_web_bk_requests%NOTFOUND THEN

               CLOSE c_web_bk_requests;

               --DBMS
               pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Secuencia Registro TBR buque TO='
                                 || TO_CHAR (vr_wb_bk_requests.seq),
                                 'INFO'
                                );
               pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Original BK ID buque FROM='
                                 || TO_CHAR (vr_wb_bk_requests.org_bk_id),
                                 'INFO'
                                );
               pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Original BK DATE buque FROM='
                                 || TO_CHAR (vr_wb_bk_requests.org_bk_date),
                                 'INFO'
                                );

               --Si se encontró información de TBR de buque que va a sustituir
               IF vr_wb_bk_requests.seq IS NOT NULL
               THEN
                  --Búsqueda de información del TBC del buque sustituido
                  OPEN c_web_bk_rqst_trans (p_bk_wbp_seq,
                                            vr_wb_bk_requests.org_bk_id
                                           );

                  IF c_web_bk_rqst_trans%ISOPEN
                  THEN
                     FETCH c_web_bk_rqst_trans
                      INTO vr_wb_bk_rqst_trans;

                     IF c_web_bk_rqst_trans%NOTFOUND
                     THEN
                        vr_wb_bk_rqst_trans := NULL;
                     END IF;

                     CLOSE c_web_bk_rqst_trans;
                  ELSE
                     vr_wb_bk_rqst_trans := NULL;
                  END IF;                 --IF c_web_bk_rqst_trans%ISOPEN THEN

                  --DBMS
                  DBMS_OUTPUT.put_line
                                      (   'Secuencia Registro TBC buque FROM='
                                       || TO_CHAR (vr_wb_bk_rqst_trans.seq)
                                      );
                  DBMS_OUTPUT.put_line
                           (   'Secuencia Registro BK_RQST buque FROM en TBC='
                            || TO_CHAR (vr_wb_bk_rqst_trans.br_seq)
                           );

                  --Si se encuentra TBC del buque a sustituir (FROM)
                  IF vr_wb_bk_rqst_trans.br_seq IS NOT NULL
                  THEN
                     vn_br_seq_from := vr_wb_bk_rqst_trans.br_seq;
                     --Se asigna secuencia de visita del TBC a variable de trabajo
                     vn_visit_seq_from := vr_wb_bk_rqst_trans.visit_seq;
                     --Se asigna dirección del tránsito del TBC a variable de trabajo
                     vv_trn_dir_from := vr_wb_bk_rqst_trans.trn_dir;
                     --Se asigna Customer Code del TBC a variable de trabajo
                     vv_cust_cd_from := vr_wb_bk_rqst_trans.cust_cd;
                     --Se asigna Ship Number del TBC a variable de trabajo
                     vn_ship_no_from := vr_wb_bk_rqst_trans.ship_no;
                  --Si no se encuentra TBC, el booking original se tomara del campo
                  --org_bk_id del registro TBR
                  ELSE
                     vn_br_seq_from := vr_wb_bk_requests.org_bk_id;
                  END IF;     --IF vr_wb_bk_rqst_trans.br_seq IS NOT NULL THEN

                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_br_seq_from=' || TO_CHAR (vn_br_seq_from),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vn_visit_seq_from='
                                 || TO_CHAR (vn_visit_seq_from),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_trn_dir_from=' || vv_trn_dir_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_cust_cd_from=' || vv_cust_cd_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vn_ship_no_from='
                                 || TO_CHAR (vn_ship_no_from),
                                 'INFO'
                                );
                  --Búsqueda de información de BK_RQSTS del buque FROM
                  p_get_bk_rqst_info (vn_br_seq_from, vr_bk_rqsts_from);

                  --
                  -- Evalua si el request es de Auction, Best Offer o X-Piece.
                  IF    vr_bk_rqsts_from.fix_amt IS NOT NULL
                     OR pkg_book_reftab_util.f_is_bestoffer
                                                    (vr_bk_rqsts_from.bsps_seq) =
                                                                           'Y'
                     OR pkg_book_reftab_util.f_is_xpiece
                                                    (vr_bk_rqsts_from.bsps_seq) =
                                                                           'Y'
                  THEN
                     --
                     -- S61519
                     -- VJaen 15Ene2008
                     -- Evita que se procese un swap/subs/chgd para un buque subastado, best offer o x-piece.
                     RAISE e_avoid_oper_for_spec_booking;
                  END IF;

                  --Búsqueda del itinerario real de tránsito del billing set
                  --de la reservación original del buque FROM. No se
                  --tomará como referencia el itinerario de tránsito en el campo
                  --INT_MOV_SEQ del TBR para búsqueda de información del
                  --itinerario porque el mismo puede haber sido variado en
                  --EVTMS
                  vn_iti_seq_from :=
                                sf_get_first_iti_tra (vr_bk_rqsts_from.bs_seq);
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Vn_iti_seq_From='
                                 || TO_CHAR (vn_iti_seq_from),
                                 'INFO'
                                );

                  --Búsqueda de secuencia de visita del buque FROM si aplica (no existe TBC)
                  IF vn_visit_seq_from IS NULL
                  THEN
                     vn_visit_seq_from := sf_get_visit_seq (vn_iti_seq_from);
                  END IF;                  --IF vn_visit_seq_from IS NULL THEN

                  --Búsqueda de ship Number del buque FROM si aplica (no existe TBC)
                  IF vn_ship_no_from IS NULL
                  THEN
                     vn_ship_no_from := sf_get_ship_no (vn_visit_seq_from);
                  END IF;                    --IF vn_ship_no_from IS NULL THEN

                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vn_visit_seq_from mod='
                                 || TO_CHAR (vn_visit_seq_from),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vn_ship_no_from mod ='
                                 || TO_CHAR (vn_ship_no_from),
                                 'INFO'
                                );
                  --Búsqueda de información de ITIN_ITEMS del barco FROM
                  p_get_bk_itin_info (vn_visit_seq_from,
                                      vn_iti_seq_from,
                                      vn_bs_seq_from,
                                      vv_rest_from,
                                      vv_iti_trn_dir_from,
                                      vv_iti_cust_cd_from,
                                      vv_iti_pde_cd_from
                                     );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_bs_seq_from=' || TO_CHAR (vn_bs_seq_from),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_rest_from=' || vv_rest_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_trn_dir_from=' || vv_iti_trn_dir_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_cust_cd_from=' || vv_iti_cust_cd_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_pde_cd_from=' || vv_iti_pde_cd_from,
                                 'INFO'
                                );
                  --Búsqueda de información de CUST_VSL_CHARS del buque FROM
                  p_get_csn_vsl_chrs (vn_ship_no_from,
                                      vn_visit_seq_from,
                                      vv_hml_from,
                                      vn_ext_beam_from,
                                      vv_gas_free_from
                                     );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_hml_from=' || vv_hml_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vn_ext_beam_from='
                                 || TO_CHAR (vn_ext_beam_from),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_gas_free_from=' || vv_gas_free_from,
                                 'INFO'
                                );
                  --Búsqueda de arrival time del buque FROM de acuerdo al booking
                  vv_req_arr_from :=
                     f_get_arr_par (vn_bs_seq_from,
                                    vn_ext_beam_from,
                                    vv_hml_from,
                                    vv_rest_from
                                   );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_req_arr_from=' || vv_req_arr_from,
                                 'INFO'
                                );

                  --Homologación de valores de TRN_DIR con información del itinerario
                  --si no existe TBC
                  IF vv_trn_dir_from IS NULL
                  THEN
                     vv_trn_dir_from := vv_iti_trn_dir_from;
                  END IF;                    --IF vv_trn_dir_from IS NULL THEN

                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_trn_dir_from mod=' || vv_trn_dir_from,
                                 'INFO'
                                );

                  --Homologación de valores de CUST_CD con información del itinerario
                  --si no existe TBC
                  IF vv_cust_cd_from IS NULL
                  THEN
                     vv_cust_cd_from := vv_iti_cust_cd_from;
                  END IF;                    --IF vv_cust_cd_from IS NULL THEN

                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_cust_cd_from mod=' || vv_cust_cd_from,
                                 'INFO'
                                );
                  --Búsqueda del itinerario real de tránsito del billing set
                  --de la reservación original del buque TO. No se
                  --tomará como referencia el itinerario de tránsito en el campo
                  --INT_MOV_SEQ del TBR para búsqueda de información del
                  --itinerario porque el mismo puede haber sido variado en
                  --EVTMS
                  vn_iti_seq_to :=
                     sf_get_first_iti_tra
                                 (sf_get_bs_iti (vr_wb_bk_requests.int_mov_seq)
                                 );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'Vn_iti_seq_to=' || TO_CHAR (vn_iti_seq_to),
                                 'INFO'
                                );
                  --Búsqueda de información de ITIN_ITEMS del barco TO
                  p_get_bk_itin_info (vr_wb_bk_requests.visit_seq,
                                      vn_iti_seq_to,
                                      vn_bs_seq_to,
                                      vv_rest_to,
                                      vv_iti_trn_dir_to,
                                      vv_iti_cust_cd_to,
                                      vv_iti_pde_cd_to
                                     );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_bs_seq_to=' || TO_CHAR (vn_bs_seq_to),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_rest_to=' || vv_rest_to,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_trn_dir_to=' || vv_iti_trn_dir_to,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_cust_cd_to=' || vv_iti_cust_cd_to,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_pde_cd_to=' || vv_iti_pde_cd_to,
                                 'INFO'
                                );
                  --Búsqueda de información de CUST_VSL_CHARS del buque TO
                  p_get_csn_vsl_chrs (vr_wb_bk_requests.ship_no,
                                      vr_wb_bk_requests.visit_seq,
                                      vv_hml_to,
                                      vn_ext_beam_to,
                                      vv_gas_free_to
                                     );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_hml_to=' || vv_hml_to,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_ext_beam_to=' || TO_CHAR (vn_ext_beam_to),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_gas_free_to=' || vv_gas_free_to,
                                 'INFO'
                                );
                  --Búsqueda de arrival time del buque TO de acuerdo al booking
                  vv_req_arr_to :=
                     f_get_arr_par (vn_bs_seq_to,
                                    vn_ext_beam_to,
                                    vv_hml_to,
                                    vv_rest_to
                                   );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_req_arr_to=' || vv_req_arr_to,
                                 'INFO'
                                );

                  --Si existe la información de BK_RQSTS del buque FROM, continua el
                  --proceso
                  IF vr_bk_rqsts_from.seq IS NOT NULL
                  THEN
                     --Validar si el buque sustituyó a alguien previamente
                     --S15142: A Septiembre de 2006 se determinó validar la previa
                     --existencia de una substitución mediante la búsqueda de una
                     --cadena en el remark del registro de BK_RQSTS hasta que se
                     --realice la implementación para múltiples Substitutions
                     IF INSTR (vr_bk_rqsts_from.stat_rmk,
                               PKG_BOOKING.cv_subs_rmk
                              ) > 0
                     THEN
                        --DBMS
                        DBMS_OUTPUT.put_line
                                       ('Tengo remark de substitution previo');
                        RAISE e_subs_prev;
                     END IF;
                     --

                     --Cambio del criterio de restricción del buque ORIGEN (Y/N)
                        IF INSTR(PKG_BOOK_DATADICT.f_hml_rest_list, vv_hml_from) > 0 THEN
                            vv_hml_rest_from := 'Y';
                        ELSE
                            vv_hml_rest_from := 'N';
                        END IF;
                         --Cambio del criterio de restricción del buque DESTINO (Y/N)
                        IF INSTR(PKG_BOOK_DATADICT.f_hml_rest_list, vv_hml_to) > 0 THEN
                            vv_hml_rest_to := 'Y';
                        ELSE
                            vv_hml_rest_to := 'N';
                        END IF;
                     --
                     --IF INSTR(vr_bk_rqsts_from.stat_rmk, PKG_BOOKING.cv_subs_rmk) > 0 THEN
                     --Evaluación de Reglas Generales por tipo de transacción
                     p_eval_bk_tran_rules (p_bk_wbp_typ,
                                           vn_visit_seq_from,
                                           vv_cust_cd_from,
                                           vv_trn_dir_from,
                                           vv_rest_from,
                                           vv_iti_trn_dir_from,
                                           vv_iti_cust_cd_from,
                                           vv_req_arr_from,
                                           NULL,
                                           vr_bk_rqsts_from.fix_amt,
                                           vr_wb_bk_requests.ship_no,
                                           vr_wb_bk_requests.cust_cd,
                                           vr_wb_bk_requests.trn_dir,
                                           vv_rest_to,
                                           vv_iti_trn_dir_to,
                                           vv_iti_cust_cd_to,
                                           vv_req_arr_to,
                                           NULL,
                                           vr_wb_bk_requests.fix_amt,
                                           vv_error_cd,
                                           vv_hml_rest_from,
                                           vv_hml_rest_to,
                                           sf_get_bk_typ(vn_ext_beam_from),
                                           sf_get_bk_typ(vn_ext_beam_to)
                                          );
                     --DBMS
                     DBMS_OUTPUT.put_line
                                     (   'Regrese de p_eval_bk_tran con error'
                                      || vv_error_cd
                                     );

                     --Si no sucedió ningún error en las validaciones de reglas del negocio
                     IF vv_error_cd IS NULL
                     THEN
                        --Cálculo del booking fee del buque FROM antes
                        --de cambio de estatus de dicho booking
                        vn_bk_fee_work_from := f_get_bk_fee (vn_bs_seq_from);

                        --Anulación del Booking del Buque FROM
                        --Si existe TBC se hará una cancelación del Booking
                        --La cancelación del Booking incluirá la cancelación
                        --de un Daylight Transit
                        IF vr_wb_bk_rqst_trans.br_seq IS NOT NULL
                        THEN
                           p_cancel_book (vr_wb_bk_rqst_trans.seq,
                                          vr_wb_bk_rqst_trans.rqst_status,
                                          vr_wb_bk_rqst_trans.br_cancl_date,
                                          vr_wb_bk_rqst_trans.br_seq,
                                          vn_bs_seq_from,
                                          vn_ext_beam_from,
                                          vv_hml_from,
                                          vv_rest_from,
                                          vv_temp_status,
                                          vv_error_cd,
                                          vv_success_tbc
                                         );

                           --Si cancelación fue exitosa se hace la búsqueda
                           --de la información de cancellation
                           IF NVL (vv_success_tbc, ' ') =
                                          PKG_BOOKING.cv_trans_stat_true
                           THEN
                              --Update del Stat Rmk del booking cancelado
                              vv_stat_rmk_from :=
                                    'Cancelled by a '
                                 || sf_get_ref_meaning (p_bk_wbp_typ,
                                                        'BOOKING PROCESS'
                                                       )
                                 || ' transaction.';

                              --Se actualizan la información directamente a la tabla sin la vista
                              --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                              --UPDATE inclusive la columna de STAT, y en estos casos no se está
                              --haciendo update a dicha columna, por lo que el trigger de la
                              --misma restringe el uso de esta forma.
                              UPDATE bk_rqsts
                                 SET stat_rmk = vv_stat_rmk_from
                               WHERE seq = vr_bk_rqsts_from.seq;

                              p_rtv_can_fee
                                           (vr_wb_bk_rqst_trans.br_seq,
                                            vr_wb_bk_rqst_trans.br_cancl_date,
                                            vn_ext_beam_from,
                                            vv_hml_from,
                                            vv_rest_from,
                                            vn_can_wb_dy,
                                            vn_can_wb_hr,
                                            vn_can_wb_fee,
                                            vn_can_dlt_fee
                                           );
                                          --Parámetro no aplica a este proceso
                              --DBMS
                              DBMS_OUTPUT.put_line
                                 ('Registro actualizado con cancelación de booking'
                                 );
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'Vn_can_wb_dy=' || TO_CHAR (vn_can_wb_dy),
                                  'INFO'
                                 );
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'Vn_can_wb_hr=' || TO_CHAR (vn_can_wb_hr),
                                  'INFO'
                                 );
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'Vn_can_wb_fee=' || TO_CHAR (vn_can_wb_fee),
                                  'INFO'
                                 );
                           ELSE
                              --Si cancelación no fue exitosa se rechaza la transacción
                              IF vv_success_tbc IS NULL
                              THEN
                                 vv_success_tbc :=
                                        PKG_BOOKING.cv_trans_stat_false;
                                 --DBMS
                                 DBMS_OUTPUT.put_line
                                                ('Cancelación no fue exitosa');
                              END IF;        -- IF vv_success_tbc IS NULL THEN

                              vn_can_wb_dy := NULL;
                              vn_can_wb_hr := NULL;
                              vn_can_wb_fee := NULL;
                              vn_can_dlt_fee := NULL;
                           END IF;               --IF vv_success = 'TRUE' THEN
                        --Si no existe TBC de este buque se hará un VOID del Booking original
                        ELSE
                           vv_stat_rmk_from :=
                                 'This booking is REQUEST VOID for being Substituted by vessel with SIN '
                              || TO_CHAR (vr_wb_bk_requests.ship_no);

                           --Se actualizan la información directamente a la tabla sin la vista
                           --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                           --UPDATE inclusive la columna de STAT, y en estos casos no se está
                           --haciendo update a dicha columna, por lo que el trigger de la
                           --misma restringe el uso de esta forma.
                           UPDATE bk_rqsts
                              SET stat =
                                        PKG_BOOKING.cv_bk_void_rqst_stat,
                                  stat_date = SYSDATE,
                                  stat_rmk = vv_stat_rmk_from
                            WHERE seq = vr_bk_rqsts_from.seq
                              AND stat = PKG_BOOKING.cv_bk_bkd_stat;

                           --Si se pudo hacer la actualización del status a VBKD RQSTS
                           --se procederá a hacer el cambio a VOID BKD
                           IF SQL%FOUND
                           THEN
                              vv_stat_rmk_from :=
                                    'This booking was VOIDED for a '
                                 || sf_get_ref_meaning (p_bk_wbp_typ,
                                                        'BOOKING PROCESS'
                                                       )
                                 || ' Transaction from vessel with SIN '
                                 || TO_CHAR (vr_wb_bk_requests.ship_no);

                              --Se actualizan la información directamente a la tabla sin la vista
                              --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                              --UPDATE inclusive la columna de STAT, y en estos casos no se está
                              --haciendo update a dicha columna, por lo que el trigger de la
                              --misma restringe el uso de esta forma.
                              UPDATE bk_rqsts
                                 SET stat = PKG_BOOKING.cv_bk_void_stat,
                                     stat_date = SYSDATE,
                                     stat_rmk = vv_stat_rmk_from
                               WHERE seq = vr_bk_rqsts_from.seq
                                 AND stat =
                                        PKG_BOOKING.cv_bk_void_rqst_stat;

                              --DBMS
                              DBMS_OUTPUT.put_line
                                 ('Registro actualizado con remarks hasta VOID BKD'
                                 );

                              IF SQL%FOUND
                              THEN
                                 --VOID daylight booking (if it applies)
                                 vv_error_cd :=
                                    f_void_bk_dlt (vr_bk_rqsts_from.seq,
                                                   p_bk_wbp_typ
                                                  );
                                 --DBMS
                                 DBMS_OUTPUT.put_line
                                    (   'Resultado de actualizacion de daylight='
                                     || vv_error_cd
                                    );

                                 IF vv_error_cd IS NULL
                                 THEN
                                    vv_success_bkd :=
                                         PKG_BOOKING.cv_trans_stat_true;
                                 ELSE
                                    vv_success_bkd :=
                                        PKG_BOOKING.cv_trans_stat_false;
                                 END IF;         --IF vv_error_cd IS NULL THEN
                              ELSE
                                 vv_success_bkd :=
                                        PKG_BOOKING.cv_trans_stat_false;
                              END IF;                      --IF SQL%FOUND THEN
                           ELSE
                              vv_success_bkd :=
                                        PKG_BOOKING.cv_trans_stat_false;
                           END IF;                         --IF SQL%FOUND THEN

                           --DBMS
                           DBMS_OUTPUT.put_line
                                    (   'Estatus de transaccion VOID booking='
                                     || vv_success_bkd
                                    );
                        END IF;

                        --IF vr_wb_bk_rqst_trans.br_seq IS NOT NULL THEN

                        --Si transacción de CANCEL BOOKING o VOID BOOKING del
                        --buque FROM fue exitosa se procede a generar el
                        --booking para el buque que está sustituyendo
                        IF    vv_success_tbc =
                                          PKG_BOOKING.cv_trans_stat_true
                           OR vv_success_bkd =
                                          PKG_BOOKING.cv_trans_stat_true
                        THEN
                           DBMS_OUTPUT.put_line
                                       ('VOY A GENERAR NUEVO BOOKING REQUEST');
                           pkg_plog.sec_implementation
                               (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                || p_bk_wbp_seq
                                || ')',
                                'Parámetros Proceso Booking',
                                'INFO'
                               );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_vessel_type=' || vr_wb_bk_requests.vty_cd,
                                 'INFO'
                                );
                           DBMS_OUTPUT.put_line
                                         (   'P_Visit_Seq='
                                          || TO_CHAR
                                                  (vr_wb_bk_requests.visit_seq)
                                         );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Bs_Seq=' || TO_CHAR (vn_bs_seq_to),
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Iti_Seq=' || TO_CHAR (vn_iti_seq_to),
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Cust_Cd=' || vr_wb_bk_requests.cust_cd,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Real_Cust_cd=' || vv_iti_cust_cd_to,
                                 'INFO'
                                );
                           DBMS_OUTPUT.put_line
                                 (   'P_Date_Created='
                                  || TO_CHAR
                                          (vr_wb_bk_requests.trn_br_rqst_date,
                                           'DD-MON-YYYY HH24MI'
                                          )
                                 );
                           DBMS_OUTPUT.put_line
                                      (   'P_Bk Date='
                                       || TO_CHAR
                                               (vr_wb_bk_requests.trn_br_date,
                                                'DD-MON-YYYY HH24MI'
                                               )
                                      );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Trn_Dir =' || vr_wb_bk_requests.trn_dir,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Iti_trn_dir=' || vv_iti_trn_dir_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Extm_Beam=' || TO_CHAR (vn_ext_beam_to),
                                 'INFO'
                                );
                           DBMS_OUTPUT.put_line
                                     (   'P_Beam='
                                      || TO_CHAR
                                              (vr_wb_bk_requests.new_extm_beam)
                                     );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_HML=' || vv_hml_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_REST=' || vv_rest_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_GAS FREE=' || vv_gas_free_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_PDE CODE=' || vv_iti_pde_cd_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Agent=' || vr_wb_bk_requests.AGENT,
                                 'INFO'
                                );
                           --Generar un nuevo booking para el buque que va a sustituir
                           p_process_booking
                                          (vr_wb_bk_requests.vty_cd,
                                           vr_wb_bk_requests.visit_seq,
                                           vn_bs_seq_to,
                                           vn_iti_seq_to,
                                           vr_wb_bk_requests.cust_cd,
                                           vv_iti_cust_cd_to,
                                           vr_wb_bk_requests.trn_br_rqst_date,
                                           vr_wb_bk_requests.trn_br_date,
                                           vr_wb_bk_requests.trn_dir,
                                           vv_iti_trn_dir_to,
                                           vn_ext_beam_to,
                                           vr_wb_bk_requests.new_extm_beam,
                                           vv_hml_to,
                                           vv_rest_to,
                                           vv_gas_free_to,
                                           vv_iti_pde_cd_to,
                                           vr_wb_bk_requests.dtu_int,
                                           vr_wb_bk_requests.AGENT,
                                           NULL,
                                           NULL,
                                           NULL,
                                           vn_br_seq_to,
                                           vv_br_stat_to,
                                           vn_bk_cond,
                                           vv_error_cd,
                                           vv_success_bkd_to,
                                           NULL,
                                           NULL,
                                           v_spc_bk_ind,
                                           NULL
                                          );
                           --DBMS
                           DBMS_OUTPUT.put_line
                                          (   'Resultado de Process Booking ='
                                           || vv_br_stat_to
                                          );
                           DBMS_OUTPUT.put_line
                              (   'Secuencia nuevo registro Process Booking ='
                               || TO_CHAR (vn_br_seq_to)
                              );
                           DBMS_OUTPUT.put_line
                                             (   'Condicion Process Booking ='
                                              || TO_CHAR (vn_bk_cond)
                                             );
                           DBMS_OUTPUT.put_line
                                 (   'Resultado de Error de Process Booking ='
                                  || vv_error_cd
                                 );

                           --Si booking fue generado con estatus BKD
                           IF NVL (vv_br_stat_to, ' ') = 'BKD'
                           THEN
                              --
                              -- S61519
                              -- VJaen 16Ene2008
                              -- Búsqueda de información del nuevo BK_RQSTS, para obtener el BSPS_SEQ
                              -- y poder validar el tipo de booking.
                              p_get_bk_rqst_info (vn_br_seq_to,
                                                  vr_bk_rqsts_to
                                                 );

                              --
                              -- S61519
                              -- VJaen 16Ene2008
                              -- Evalúa que el tipo de booking sea igual para ambos buques.
                              IF pkg_book_reftab_util.f_get_bk_size_typ
                                                    (vr_bk_rqsts_from.bsps_seq) <>
                                    pkg_book_reftab_util.f_get_bk_size_typ
                                                      (vr_bk_rqsts_to.bsps_seq)
                              THEN
                                 RAISE e_book_typ_must_be_equal;
                              END IF;

                              --Validación de si el booking del buque FROM
                              --contenia un FIX AMT (cargo de booking por subasta)
                              --con el cual se comparará el cargo de booking a
                              --traves de tarifas del buque TO
                              IF vr_bk_rqsts_from.fix_amt IS NOT NULL
                              THEN
                                 vn_bk_fee_from := vr_bk_rqsts_from.fix_amt;
                                 vv_bkd_fix_amt := 'Y';
                                 --DBMS
                                 DBMS_OUTPUT.put_line
                                             (   'Subasta vn_bk_wb_fee_from='
                                              || TO_CHAR (vn_bk_fee_from)
                                             );
                                 DBMS_OUTPUT.put_line
                                                     (   'Vv_bk_fix_amt_from='
                                                      || vv_bkd_fix_amt
                                                     );
                              ELSE
                                 --En caso de que el buque FROM no tenga una cifra
                                 --de cargo por subasta, se tomará el cargo de
                                 --booking fee a través de tarifas y se comparará
                                 --con el del buque TO

                                 --El cálculo del booking calculation se hace antes del VOID/CANCEL
                                 --de la reservación del buque FROM
                                 vn_bk_fee_from := vn_bk_fee_work_from;
                                 --DBMS
                                 DBMS_OUTPUT.put_line
                                              (   'Tarifa vn_bk_wb_fee_from='
                                               || TO_CHAR (vn_bk_fee_from)
                                              );
                              END IF;

                              --IF vr_bk_rqsts_from.fix_amt IS NOT NULL THEN
                              vn_bk_fee_to := f_get_bk_fee (vn_bs_seq_to);

                              IF NVL (vn_bk_fee_from, 0) >=
                                                         NVL (vn_bk_fee_to, 0)
                              THEN
                                 vn_bk_wb_fee := vn_bk_fee_from;

                                 --Se determinará si del fee entre los dos buques
                                 --es mayor la cantidad de subasta si alguno de los
                                 --dos la presenta. De ser así se actualizará la
                                 --cantidad mayor en la columna FIX_AMT del registro
                                 --de BK_RQSTS del nuevo buque
                                 IF NVL (vv_bkd_fix_amt, ' ') = 'Y'
                                 THEN
                                    vv_upd_fix_amt := 'Y';
                                 END IF;
                              -- IF NVL(vv_bkd_fix_amt,' ') = 'Y' THEN
                              ELSE
                                 vn_bk_wb_fee := vn_bk_fee_to;
                              END IF;

                              --IF NVL(vn_bk_fee_from,0) >= NVL(vn_bk_fee_to,0) THEN

                              --DBMS
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                     'Vn_bk_wb_fee_final='
                                  || TO_CHAR (vn_bk_wb_fee),
                                  'INFO'
                                 );
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'vv_upd_fix_amt=' || vv_upd_fix_amt,
                                  'INFO'
                                 );
                              --Actualización del Stat Rmk para booking recien generado
                              vv_stat_rmk_to :=
                                    'This booking was generated from a Substitution Transaction for SIN '
                                 || TO_CHAR (vn_ship_no_from);
                              --DBMS
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'Stat Rmk To=' || vv_stat_rmk_to,
                                  'INFO'
                                 );

                              --Se actualizan la información directamente a la tabla sin la vista
                              --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                              --UPDATE inclusive la columna de STAT, y en estos casos no se está
                              --haciendo update a dicha columna, por lo que el trigger de la
                              --misma restringe el uso de esta forma.
                              UPDATE bk_rqsts
                                 SET stat_rmk = vv_stat_rmk_to
                               WHERE seq = vn_br_seq_to
                                 AND stat = PKG_BOOKING.cv_bk_bkd_stat;

                              IF SQL%FOUND
                              THEN
                                 --DBMS
                                 DBMS_OUTPUT.put_line
                                                ('Hecho update de BK_RQST TO');

                                 --
                                 -- S61519
                                 -- VJaen 15Jan2008.
                                 -- Inserta el registro detalle de SUBSTITUTION para el buque TO.
                                 DECLARE
                                    vr_to_det   vw_bk_rqst_dets%ROWTYPE;
                                 BEGIN
                                    pkg_plog.sec_implementation
                                       (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                        || p_bk_wbp_seq
                                        || ')',
                                        'Inserta el registro detalle de SUBSTITUTION para el buque TO.',
                                        'INFO'
                                       );
                                    -- Llena la variable de registro.
                                    vr_to_det.br_seq := vn_br_seq_to;
                                    vr_to_det.bs_seq := vn_bs_seq_to;
                                    vr_to_det.trns_type :=
                                                  pkg_book_datadict.f_bkp_subs;
                                    vr_to_det.br_seq_dest :=
                                                          vr_bk_rqsts_from.seq;
                                    vr_to_det.bs_seq_dest :=
                                                       vr_bk_rqsts_from.bs_seq;
                                    vr_to_det.amount := vn_bk_wb_fee;
                                    -- Solicita la inserción del registro.
                                    p_ins_bk_rqst_dets (vr_to_det);
                                    --
                                    pkg_plog.sec_implementation
                                       (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                        || p_bk_wbp_seq
                                        || ')',
                                        'Registro detalle de SUBSTITUTION insertado.',
                                        'INFO'
                                       );
                                 END;

                                 --Actualización del FIX_AMT en el nuevo registro
                                 --de BK_RQSTS del buque TO si el buque FROM
                                 --tenía cargo de booking por subasta y el cargo
                                 --de booking por tarifa del buque TO era menor
                                 IF vv_upd_fix_amt = 'Y'
                                 THEN
                                    --Se actualizan la información directamente a la tabla sin la vista
                                    --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                                    --UPDATE inclusive la columna de STAT, y en estos casos no se está
                                    --haciendo update a dicha columna, por lo que el trigger de la
                                    --misma restringe el uso de esta forma.
                                    UPDATE bk_rqsts
                                       SET fix_amt = vn_bk_wb_fee
                                     WHERE seq = vn_br_seq_to
                                       AND stat =
                                              PKG_BOOKING.cv_bk_bkd_stat;

                                    IF SQL%FOUND
                                    THEN
                                       vv_success_bkd_to :=
                                          PKG_BOOKING.cv_trans_stat_true;
                                       --DBMS
                                       DBMS_OUTPUT.put_line
                                          ('Hecho update de FIX AMT en BK_RQSTS TO'
                                          );
                                    ELSE
                                       vv_success_bkd_to :=
                                          PKG_BOOKING.cv_trans_stat_false;
                                    END IF;               -- IF SQL%FOUND THEN
                                 END IF;       -- IF vv_upd_fix_amt = 'Y' THEN
                              ELSE
                                 vv_success_bkd_to :=
                                        PKG_BOOKING.cv_trans_stat_false;
                              END IF;                     -- IF SQL%FOUND THEN
                           ELSE
                              vv_success_bkd_to :=
                                        PKG_BOOKING.cv_trans_stat_false;

                              --S15142.T15: Added on August 08, 2007 -- GNV
                              --Set the status and message if the booking request was rejected
                              IF NVL (vv_br_stat_to, ' ') = 'REJ'
                              THEN
                                 --Get the rejection remark to send it to global variable
                                 --It will be used to display the rejection reason on the
                                 --Web Booking Request Transactions Module (VE5060FM)
                                 --This will be done because all the transaction will be
                                 --reversed
                                 BEGIN
                                    SELECT stat_rmk
                                      INTO vv_rej_stat_rmk_to
                                      FROM bk_rqsts
                                     WHERE seq = vn_br_seq_to AND stat = 'REJ';
                                 EXCEPTION
                                    WHEN OTHERS
                                    THEN
                                       vv_rej_stat_rmk_to := NULL;
                                 END;

                                 pkg_global_vars.p_set_char4 (vv_br_stat_to);
                                 pkg_global_vars.p_set_char5
                                                           (vv_rej_stat_rmk_to);
                              END IF; --IF NVL(vv_br_stat_to,' ') = 'REJ' THEN
                           END IF;    --IF NVL(vv_br_stat_to,' ') = 'BKD' THEN

                           --DBMS
                           DBMS_OUTPUT.put_line
                                  (   'Resultado de generación nuevo booking='
                                   || vv_success_bkd_to
                                  );
                           vn_br_wb_seq_first := vn_br_seq_to;

                           IF NVL (vv_success_bkd_to, ' ') =
                                          PKG_BOOKING.cv_trans_stat_true
                           THEN
                              vv_bk_wb_stat :=
                                         PKG_BOOKING.cv_trans_stat_true;
                           ELSE
                              vv_bk_wb_stat :=
                                        PKG_BOOKING.cv_trans_stat_false;
                           END IF;
                        -- IF NVL(vv_success_bkd_to,' ') = PKG_BOOKING.cv_trans_stat_true THEN
                        ELSE
                           vn_br_wb_seq_first := NULL;
                           vn_bk_wb_fee := NULL;
                           vn_can_wb_dy := NULL;
                           vn_can_wb_hr := NULL;
                           vn_can_wb_fee := NULL;
                           vn_bk_cond := NULL;
                           vv_bk_wb_stat :=
                                        PKG_BOOKING.cv_trans_stat_false;
                        END IF;
                     --IF vv_success_tbc = PKG_BOOKING.cv_trans_stat_true OR
                     ELSE
                        vn_br_wb_seq_first := NULL;
                        vn_bk_wb_fee := NULL;
                        vn_can_wb_dy := NULL;
                        vn_can_wb_hr := NULL;
                        vn_can_wb_fee := NULL;
                        vn_bk_cond := NULL;
                        vv_bk_wb_stat :=
                                        PKG_BOOKING.cv_trans_stat_false;
                     END IF;                     --IF vv_error_cd IS NULL THEN
                  ELSE
                     vn_br_wb_seq_first := NULL;
                     vn_bk_wb_fee := NULL;
                     vn_can_wb_dy := NULL;
                     vn_can_wb_hr := NULL;
                     vn_can_wb_fee := NULL;
                     vn_bk_cond := NULL;
                     vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
                  END IF;           --IF vr_bk_rqsts_from.seq IS NOT NULL THEN
               ELSE
                  vn_br_wb_seq_first := NULL;
                  vn_bk_wb_fee := NULL;
                  vn_can_wb_dy := NULL;
                  vn_can_wb_hr := NULL;
                  vn_can_wb_fee := NULL;
                  vn_bk_cond := NULL;
                  vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
               END IF;             --IF vr_wb_bk_requests.seq IS NOT NULL THEN
            ELSE
               vn_br_wb_seq_first := NULL;
               vn_bk_wb_fee := NULL;
               vn_can_wb_dy := NULL;
               vn_can_wb_hr := NULL;
               vn_can_wb_fee := NULL;
               vn_bk_cond := NULL;
               vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            END IF;                         --IF c_web_bk_requests%ISOPEN THEN
         ELSE
            vn_br_wb_seq_first := NULL;
            vn_bk_wb_fee := NULL;
            vn_can_wb_dy := NULL;
            vn_can_wb_hr := NULL;
            vn_can_wb_fee := NULL;
            vn_bk_cond := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
         END IF;                            --IF p_bk_wbp_seq IS NOT NULL THEN
      EXCEPTION
         WHEN e_subs_prev
         THEN
            vn_br_wb_seq_first := NULL;
            vn_bk_wb_fee := NULL;
            vn_can_wb_dy := NULL;
            vn_can_wb_hr := NULL;
            vn_can_wb_fee := NULL;
            vn_bk_cond := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            vv_error_cd := 'USR-30305';
         --Para enviar el código de error se desactiva estas programacion
         --Pkg_Evtms_Db_Util.p_generic_exception('USR-30305','This Vessel substituted a booking previously.It can not be substituted by another vessel.');
         WHEN e_avoid_oper_for_spec_booking
         THEN
            vn_br_wb_seq_first := NULL;
            vn_bk_wb_fee := NULL;
            vn_can_wb_dy := NULL;
            vn_can_wb_hr := NULL;
            vn_can_wb_fee := NULL;
            vn_bk_cond := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            pkg_plog.sec_implementation
               ('PKG_BOOKING.P_BOOK_TRANS_SWAP (WBP_SEQ=' || p_bk_wbp_seq
                || ')',
                'Current version of the system does not allow SWAP/SUBSTITUTION/CHANGE DATE operations for Auction, Best Offer or X-Piece Bookings.',
                'INFO'
               );
            -- Current version of the system does not allow SWAP/SUBSTITUTION/CHANGE DATE operations for Auction, Best Offer or X-Piece Bookings.
            vv_error_cd := 'USR-02031';
         WHEN e_book_typ_must_be_equal
         THEN
            vn_br_wb_seq_first := NULL;
            vn_bk_wb_fee := NULL;
            vn_can_wb_dy := NULL;
            vn_can_wb_hr := NULL;
            vn_can_wb_fee := NULL;
            vn_bk_cond := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            pkg_plog.sec_implementation
               ('PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ=' || p_bk_wbp_seq
                || ')',
                'Booking types must be the same for both vessels when doing SWAP or SUBSTITUTION.',
                'INFO'
               );
            -- Booking types must be the same for both vessels when doing SWAP or SUBSTITUTION.
            vv_error_cd := 'USR-02032';
      END;

      p_bk_wb_seq_first := vn_br_wb_seq_first;
      p_bk_wb_fee := vn_bk_wb_fee;
      p_can_wb_dy := vn_can_wb_dy;
      p_can_wb_hr := vn_can_wb_hr;
      p_can_wb_fee := vn_can_wb_fee;
      p_bk_cond := vn_bk_cond;
      p_bk_wb_stat := vv_bk_wb_stat;
      p_error_cd := vv_error_cd;
      --DBMS
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_SUBS p_bk_wb_seq_first ='
                                 || TO_CHAR (vn_br_wb_seq_first),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_SUBS p_bk_wb_fee ='
                                 || TO_CHAR (vn_bk_wb_fee),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_SUBS p_can_wb_dy ='
                                 || TO_CHAR (vn_can_wb_dy),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_SUBS p_can_wb_hr ='
                                 || TO_CHAR (vn_can_wb_hr),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_SUBS p_can_wb_fee ='
                                 || TO_CHAR (vn_can_wb_fee),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'BOOK_SUBS p_bk_wb_stat =' || vv_bk_wb_stat,
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_SUBS p_bk_cond ='
                                 || TO_CHAR (vn_bk_cond),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_SUBS (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'BOOK_SUBS p_error_cd =' || vv_error_cd,
                                 'INFO'
                                );
   END p_book_trans_subs;
/* Generate the Booking Change Date */
PROCEDURE P_BOOK_TRANS_CHGD
 (P_BK_WBP_SEQ IN WEB_BK_PROCS.SEQ%TYPE
 ,P_BK_WBP_TYP IN WEB_BK_PROCS.TYP%TYPE
 ,P_BK_WB_SEQ_FIRST OUT BK_RQSTS.SEQ%TYPE
 ,P_BK_WB_FEE OUT CHARGES.CHRG_AMT%TYPE
 ,P_CAN_WB_DY OUT WEB_BK_RQST_TRANS.BR_CANCL_DY%TYPE
 ,P_CAN_WB_HR OUT WEB_BK_RQST_TRANS.BR_CANCL_HR%TYPE
 ,P_CAN_WB_FEE OUT CHARGES.CHRG_AMT%TYPE
 ,P_BK_WB_STAT OUT WEB_BK_REQUESTS.TRANS_STAT%TYPE
 ,P_BK_COND OUT BK_CONDS.LEV%TYPE
 ,P_ERROR_CD OUT VARCHAR2
 )
 IS
--============================================================================
-- DESCRIPCION:
--   Este procedimiento se encarga realizar la transacción de  Change Date  de Reservaciones
--  (Bookings) de acuerdo a un TBR (Transit Booking Requests)  de un buque para cambiar r la --   -- fecha de la reservación generado  desde EDCS
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador    Fecha                  Cambios realizados
-- -------------   --------------------  -------------------     -----------------------------
-- S15142      GVillarreal         01-SEP-2006      Versión Inicial
-- S61519      VJaen            16-Jan-2008    Prevencion de procesamiento para
--                                             booking especiales y grabado en tabla
--                                             de detalle.
--=============================================================================
      CURSOR c_web_bk_requests (p_bk_wbp_seq IN web_bk_procs.seq%TYPE)
      IS
         SELECT seq, visit_seq, ship_no, vty_cd, cust_cd, int_mov_seq,
                trn_dir, trn_br_date, trn_br_rqst_date, org_bk_date,
                org_bk_id, primary_ref, sec_ref, AGENT, fix_amt,
                new_extm_beam, dtu_int
           FROM web_bk_requests
          WHERE wbp_seq = p_bk_wbp_seq
            AND rqst_status = PKG_BOOKING.cv_rqst_status_pend;

      CURSOR c_web_bk_rqst_trans (
         p_bk_wbp_seq   IN   web_bk_procs.seq%TYPE,
         p_br_seq       IN   bk_rqsts.seq%TYPE
      )
      IS
         SELECT seq, visit_seq, ship_no, br_seq, cust_cd, trn_dir,
                br_cancl_date, trn_br_date, rqst_status
           FROM web_bk_rqst_trans
          WHERE wbp_seq = p_bk_wbp_seq
            AND br_seq = p_br_seq
            AND option_rqst = 'T'
            AND rqst_status = PKG_BOOKING.cv_rqst_status_pend;

      vr_wb_bk_requests               PKG_BOOKING.tr_web_bk_requests;
      vr_wb_bk_rqst_trans             PKG_BOOKING.tr_web_bk_rqst_trans;
      vr_bk_rqsts_from                PKG_BOOKING.tr_bk_rqsts;
      vn_br_wb_seq_first              bk_rqsts.seq%TYPE;
      vn_bk_wb_fee                    charges.chrg_amt%TYPE;
      vn_can_wb_dy                    web_bk_rqst_trans.br_cancl_dy%TYPE;
      vn_can_wb_hr                    web_bk_rqst_trans.br_cancl_hr%TYPE;
      vn_can_wb_fee                   web_bk_rqst_trans.br_cancl_fee%TYPE;
      vn_can_dlt_fee                  web_bk_rqst_trans.daylight_trn_cancl_fee%TYPE;
      vv_bk_wb_stat                   web_bk_rqst_trans.trans_stat%TYPE;
      vn_iti_seq_from                 itin_items.seq%TYPE;
      vn_br_seq_from                  bk_rqsts.seq%TYPE;
      vn_visit_seq_from               cust_sched_needs.seq%TYPE;
      vv_trn_dir_from                 itin_items.tii_trn_dir%TYPE;
      vv_cust_cd_from                 VARCHAR2 (218);
      -- From Definition on function SF_GET_CUST_NAME
      vn_bs_seq_from                  bill_sets.seq%TYPE;
      vv_rest_from                    rest_usr_cds.cd%TYPE;
      -- From Definition on function SF_GET_REST_USER_BK
      vv_iti_trn_dir_from             itin_items.tii_trn_dir%TYPE;
      vv_iti_cust_cd_from             VARCHAR2 (218);
      -- From Definition on function SF_GET_CUST_NAME
      vv_iti_pde_cd_from              itin_items.pde_cd%TYPE;
      vv_hml_from                     cust_vsl_chars.lvc_hml_qualified%TYPE;
      vn_ext_beam_from                cust_vsl_chars.extm_beam%TYPE;
      vv_gas_free_from                cust_visit_dets.tblc_gas_free_ind%TYPE;
      vv_req_arr_from                 VARCHAR2 (5);
      -- From Definition de funcion PKG_BOOKING.F_GET_ARR_PAR;
      vv_stat_rmk_from                bk_rqsts.stat_rmk%TYPE;
      vn_iti_seq_to                   itin_items.seq%TYPE;
      vn_bs_seq_to                    bill_sets.seq%TYPE;
      vv_rest_to                      rest_usr_cds.cd%TYPE;
      -- From Definition on function SF_GET_REST_USER_BK
      vv_iti_trn_dir_to               itin_items.tii_trn_dir%TYPE;
      vv_iti_cust_cd_to               VARCHAR2 (218);
      -- From Definition on function SF_GET_CUST_NAME
      vv_iti_pde_cd_to                itin_items.pde_cd%TYPE;
      vv_hml_to                       cust_vsl_chars.lvc_hml_qualified%TYPE;
      vn_ext_beam_to                  cust_vsl_chars.extm_beam%TYPE;
      vv_gas_free_to                  cust_visit_dets.tblc_gas_free_ind%TYPE;
      vv_req_arr_to                   VARCHAR2 (5);
      -- From Definition de funcion PKG_BOOKING.F_GET_ARR_PAR;
      vv_stat_rmk_to                  bk_rqsts.stat_rmk%TYPE;
      vv_rej_stat_rmk_to              bk_rqsts.stat_rmk%TYPE;
      vn_br_seq_to                    bk_rqsts.seq%TYPE;
      vv_br_stat_to                   bk_rqsts.stat%TYPE;
      vn_bk_cond                      bk_conds.lev%TYPE;
      vv_temp_status                  web_bk_rqst_trans.rqst_status%TYPE;
      vv_success_tbc                  web_bk_requests.trans_stat%TYPE;
      vv_success_bkd                  web_bk_requests.trans_stat%TYPE;
      vv_success_bkd_to               web_bk_requests.trans_stat%TYPE;
      vv_error_cd                     VARCHAR2 (30);
      v_spc_bk_ind                    VARCHAR2 (5);
      --
      -- S61519
      -- VJaen 15Ene2008
      e_avoid_oper_for_spec_booking   EXCEPTION;
BEGIN
      p_bk_wb_seq_first := NULL;
      p_bk_wb_fee := NULL;
      p_can_wb_dy := NULL;
      p_can_wb_hr := NULL;
      p_can_wb_fee := NULL;
      p_bk_wb_stat := NULL;
      p_bk_cond := NULL;
      p_error_cd := NULL;
      --S15142.T15 Added on August 08, 2007 -- GNV
      --Clear global variables before use
      pkg_global_vars.p_set_char4 (NULL);
      pkg_global_vars.p_set_char5 (NULL);

      BEGIN
         IF p_bk_wbp_seq IS NOT NULL
         THEN
            --Búsqueda de información del TBR del buque que va a cambiar la fecha
            OPEN c_web_bk_requests (p_bk_wbp_seq);

            IF c_web_bk_requests%ISOPEN
            THEN
               FETCH c_web_bk_requests
                INTO vr_wb_bk_requests;

               IF c_web_bk_requests%NOTFOUND
               THEN
                  vr_wb_bk_requests := NULL;
               END IF;                    --IF c_web_bk_requests%NOTFOUND THEN

               CLOSE c_web_bk_requests;

               --DBMS
               pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Secuencia Registro TBR buque TO='
                                 || TO_CHAR (vr_wb_bk_requests.seq),
                                 'INFO'
                                );
               pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Original BK ID buque FROM='
                                 || TO_CHAR (vr_wb_bk_requests.org_bk_id),
                                 'INFO'
                                );
               pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Original BK DATE buque FROM='
                                 || TO_CHAR (vr_wb_bk_requests.org_bk_date),
                                 'INFO'
                                );

               --Si se encontró información de TBR de buque que va a cambiar la fecha
               IF vr_wb_bk_requests.seq IS NOT NULL
               THEN
                  --Búsqueda de información del TBC del buque que va a cambiar la fecha
                  OPEN c_web_bk_rqst_trans (p_bk_wbp_seq,
                                            vr_wb_bk_requests.org_bk_id
                                           );

                  IF c_web_bk_rqst_trans%ISOPEN
                  THEN
                     FETCH c_web_bk_rqst_trans
                      INTO vr_wb_bk_rqst_trans;

                     IF c_web_bk_rqst_trans%NOTFOUND
                     THEN
                        vr_wb_bk_rqst_trans := NULL;
                     END IF;

                     CLOSE c_web_bk_rqst_trans;
                  ELSE
                     vr_wb_bk_rqst_trans := NULL;
                  END IF;                 --IF c_web_bk_rqst_trans%ISOPEN THEN

                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Secuencia Registro TBC buque FROM='
                                 || TO_CHAR (vr_wb_bk_rqst_trans.seq),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                       (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                        || p_bk_wbp_seq
                        || ')',
                           '** Secuencia Registro BK_RQST buque FROM en TBC='
                        || TO_CHAR (vr_wb_bk_rqst_trans.br_seq),
                        'INFO'
                       );

                  --Si se encuentra TBC del buque para la fecha de booking
                  --original a cancelar
                  IF vr_wb_bk_rqst_trans.br_seq IS NOT NULL
                  THEN
                     vn_br_seq_from := vr_wb_bk_rqst_trans.br_seq;
                     --Se asigna secuencia de visita del TBC a variable de trabajo
                     vn_visit_seq_from := vr_wb_bk_rqst_trans.visit_seq;
                     --Se asigna dirección del tránsito del TBC a variable de trabajo
                     vv_trn_dir_from := vr_wb_bk_rqst_trans.trn_dir;
                     --Se asigna Customer Code del TBC a variable de trabajo
                     vv_cust_cd_from := vr_wb_bk_rqst_trans.cust_cd;
                  --Si no se encuentra TBC, el sequence del registro de booking
                  --original se tomara del campo org_bk_id del registro TBR
                  ELSE
                     vn_br_seq_from := vr_wb_bk_requests.org_bk_id;
                  END IF;     --IF vr_wb_bk_rqst_trans.br_seq IS NOT NULL THEN

                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_br_seq_from=' || TO_CHAR (vn_br_seq_from),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vn_visit_seq_from='
                                 || TO_CHAR (vn_visit_seq_from),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_trn_dir_from=' || vv_trn_dir_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_cust_cd_from=' || vv_cust_cd_from,
                                 'INFO'
                                );
                  --Búsqueda de información de BK_RQSTS original del buque
                  p_get_bk_rqst_info (vn_br_seq_from, vr_bk_rqsts_from);

                  --
                  -- Evalua si el request es de Auction, Best Offer o X-Piece.
                  IF    vr_bk_rqsts_from.fix_amt IS NOT NULL
                     OR pkg_book_reftab_util.f_is_bestoffer
                                                    (vr_bk_rqsts_from.bsps_seq) =
                                                                           'Y'
                     OR pkg_book_reftab_util.f_is_xpiece
                                                    (vr_bk_rqsts_from.bsps_seq) =
                                                                           'Y'
                  THEN
                     --
                     -- S61519
                     -- VJaen 15Ene2008
                     -- Evita que se procese un swap/subs/chgd para un buque subastado, best offer o x-piece.
                     RAISE e_avoid_oper_for_spec_booking;
                  END IF;

                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    '** vr_bk_rqsts_from.bs_seq='
                                 || vr_bk_rqsts_from.bs_seq,
                                 'INFO'
                                );
                  --Búsqueda del itinerario real de tránsito del billing set
                  --de la reservación original del buque FROM. No se
                  --tomará como referencia el itinerario de tránsito en el campo
                  --INT_MOV_SEQ del TBR para búsqueda de información del
                  --itinerario porque el mismo puede haber sido variado en
                  --EVTMS
                  vn_iti_seq_from :=
                                sf_get_first_iti_tra (vr_bk_rqsts_from.bs_seq);
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Vn_iti_seq_From='
                                 || TO_CHAR (vn_iti_seq_from),
                                 'INFO'
                                );

                  --Búsqueda de secuencia de visita del booking original del buque si aplica (no existe TBC)
                  IF vn_visit_seq_from IS NULL
                  THEN
                     vn_visit_seq_from := sf_get_visit_seq (vn_iti_seq_from);
                  END IF;

                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vn_visit_seq_from mod='
                                 || TO_CHAR (vn_visit_seq_from),
                                 'INFO'
                                );
                  --Búsqueda de información de ITIN_ITEMS del buque que corresponde
                  --al booking original
                  p_get_bk_itin_info (vr_wb_bk_requests.visit_seq,
                                      vn_iti_seq_from,
                                      vn_bs_seq_from,
                                      vv_rest_from,
                                      vv_iti_trn_dir_from,
                                      vv_iti_cust_cd_from,
                                      vv_iti_pde_cd_from
                                     );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_bs_seq_from=' || TO_CHAR (vn_bs_seq_from),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_rest_from=' || vv_rest_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_trn_dir_from=' || vv_iti_trn_dir_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_cust_cd_from=' || vv_iti_cust_cd_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_pde_cd_from=' || vv_iti_pde_cd_from,
                                 'INFO'
                                );
                  --Búsqueda de información de CUST_VSL_CHARS del buque para el tránsito del
                  --Booking original
                  p_get_csn_vsl_chrs (vr_wb_bk_requests.ship_no,
                                      vn_visit_seq_from,
                                      vv_hml_from,
                                      vn_ext_beam_from,
                                      vv_gas_free_from
                                     );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_hml_from=' || vv_hml_from,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'vn_ext_beam_from='
                                 || TO_CHAR (vn_ext_beam_from),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_gas_free_from=' || vv_gas_free_from,
                                 'INFO'
                                );
                  --Búsqueda de arrival time del buque de acuerdo al booking original
                  vv_req_arr_from :=
                     f_get_arr_par (vn_bs_seq_from,
                                    vn_ext_beam_from,
                                    vv_hml_from,
                                    vv_rest_from
                                   );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_req_arr_from=' || vv_req_arr_from,
                                 'INFO'
                                );

                  --Homologación de valores de TRN_DIR con información del itinerario
                  --del booking original si no existe TBC
                  IF vv_trn_dir_from IS NULL
                  THEN
                     vv_trn_dir_from := vv_iti_trn_dir_from;
                  END IF;

                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_trn_dir_from mod=' || vv_trn_dir_from,
                                 'INFO'
                                );

                  --Homologación de valores de CUST_CD con información del itinerario
                  --del booking original si no existe TBC
                  IF vv_cust_cd_from IS NULL
                  THEN
                     vv_cust_cd_from := vv_iti_cust_cd_from;
                  END IF;

                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_cust_cd_from mod=' || vv_cust_cd_from,
                                 'INFO'
                                );
                  --Búsqueda del itinerario real de tránsito del billing set
                  --de la reservación original del buque TO. No se
                  --tomará como referencia el itinerario de tránsito en el campo
                  --INT_MOV_SEQ del TBR para búsqueda de información del
                  --itinerario porque el mismo puede haber sido variado en
                  --EVTMS
                  vn_iti_seq_to :=
                     sf_get_first_iti_tra
                                 (sf_get_bs_iti (vr_wb_bk_requests.int_mov_seq)
                                 );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'Vn_iti_seq_to=' || TO_CHAR (vn_iti_seq_to),
                                 'INFO'
                                );
                  --Búsqueda de información de ITIN_ITEMS del buque correspondiente
                  --al nuevo tránsito o nueva fecha que se va a tomar para generar
                  --un nuevo registro de booking
                  p_get_bk_itin_info (vr_wb_bk_requests.visit_seq,
                                      vn_iti_seq_to,
                                      vn_bs_seq_to,
                                      vv_rest_to,
                                      vv_iti_trn_dir_to,
                                      vv_iti_cust_cd_to,
                                      vv_iti_pde_cd_to
                                     );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_bs_seq_to=' || TO_CHAR (vn_bs_seq_to),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_rest_to=' || vv_rest_to,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_trn_dir_to=' || vv_iti_trn_dir_to,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_cust_cd_to=' || vv_iti_cust_cd_to,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_iti_pde_cd_to=' || vv_iti_pde_cd_to,
                                 'INFO'
                                );
                  --Búsqueda de información de CUST_VSL_CHARS del buque con
                  --respecto a la posible nueva visita del buque para el nuevo booking
                  p_get_csn_vsl_chrs (vr_wb_bk_requests.ship_no,
                                      vr_wb_bk_requests.visit_seq,
                                      vv_hml_to,
                                      vn_ext_beam_to,
                                      vv_gas_free_to
                                     );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_hml_to=' || vv_hml_to,
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vn_ext_beam_to=' || TO_CHAR (vn_ext_beam_to),
                                 'INFO'
                                );
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_gas_free_to=' || vv_gas_free_to,
                                 'INFO'
                                );
                  --Búsqueda de arrival time del buque respecto a la posible nueva visita del buque
                  --para el nuevo booking
                  vv_req_arr_to :=
                     f_get_arr_par (vn_bs_seq_to,
                                    vn_ext_beam_to,
                                    vv_hml_to,
                                    vv_rest_to
                                   );
                  --DBMS
                  pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'vv_req_arr_to=' || vv_req_arr_to,
                                 'INFO'
                                );

                  --Si existe la información de BK_RQSTS original del buque , continua el
                  --proceso
                  IF vr_bk_rqsts_from.seq IS NOT NULL
                  THEN
                     --Evaluación de Reglas Generales por tipo de transacción
                     p_eval_bk_tran_rules (p_bk_wbp_typ,
                                           vn_visit_seq_from,
                                           vv_cust_cd_from,
                                           vv_trn_dir_from,
                                           vv_rest_from,
                                           vv_iti_trn_dir_from,
                                           vv_iti_cust_cd_from,
                                           vv_req_arr_from,
                                           NULL,
                                           vr_bk_rqsts_from.fix_amt,
                                           vr_wb_bk_requests.ship_no,
                                           vr_wb_bk_requests.cust_cd,
                                           vr_wb_bk_requests.trn_dir,
                                           vv_rest_to,
                                           vv_iti_trn_dir_to,
                                           vv_iti_cust_cd_to,
                                           vv_req_arr_to,
                                           NULL,
                                           vr_wb_bk_requests.fix_amt,
                                           vv_error_cd
                                          );
                     --DBMS
                     pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Regrese de p_eval_bk_tran con error'
                                 || vv_error_cd,
                                 'INFO'
                                );

                     --Si no sucedió ningún error en las validaciones de reglas del negocio
                     IF vv_error_cd IS NULL
                     THEN
                        --Anulación del Booking original
                        --Si existe TBC se hará una cancelación del Booking original
                        IF vr_wb_bk_rqst_trans.br_seq IS NOT NULL
                        THEN
                           p_cancel_book (vr_wb_bk_rqst_trans.seq,
                                          vr_wb_bk_rqst_trans.rqst_status,
                                          vr_wb_bk_rqst_trans.br_cancl_date,
                                          vr_wb_bk_rqst_trans.br_seq,
                                          vn_bs_seq_from,
                                          vn_ext_beam_from,
                                          vv_hml_from,
                                          vv_rest_from,
                                          vv_temp_status,
                                          vv_error_cd,
                                          vv_success_tbc
                                         );

                           --Si cancelación fue exitosa se hace la búsqueda
                           --de la información de cancellation
                           IF NVL (vv_success_tbc, ' ') =
                                          PKG_BOOKING.cv_trans_stat_true
                           THEN
                              --Update del Stat Rmk del booking cancelado
                              vv_stat_rmk_from :=
                                    'Cancelled by a '
                                 || sf_get_ref_meaning (p_bk_wbp_typ,
                                                        'BOOKING PROCESS'
                                                       )
                                 || ' transaction.';

                              --Se actualizan la información directamente a la tabla sin la vista
                              --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                              --UPDATE inclusive la columna de STAT, y en estos casos no se está
                              --haciendo update a dicha columna, por lo que el trigger de la
                              --misma restringe el uso de esta forma.
                              UPDATE bk_rqsts
                                 SET stat_rmk = vv_stat_rmk_from
                               WHERE seq = vr_bk_rqsts_from.seq;

                              p_rtv_can_fee
                                           (vr_wb_bk_rqst_trans.br_seq,
                                            vr_wb_bk_rqst_trans.br_cancl_date,
                                            vn_ext_beam_from,
                                            vv_hml_from,
                                            vv_rest_from,
                                            vn_can_wb_dy,
                                            vn_can_wb_hr,
                                            vn_can_wb_fee,
                                            vn_can_dlt_fee
                                           );
                                             --Parámetro no aplica a este proceso
                              --DBMS
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'Registro actualizado con cancelación de booking',
                                  'INFO'
                                 );
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'Vn_can_wb_dy=' || TO_CHAR (vn_can_wb_dy),
                                  'INFO'
                                 );
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'Vn_can_wb_hr=' || TO_CHAR (vn_can_wb_hr),
                                  'INFO'
                                 );
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'Vn_can_wb_fee=' || TO_CHAR (vn_can_wb_fee),
                                  'INFO'
                                 );
                           ELSE
                              --Si proceso de cancelación no fue exitoso se rechaza la transacción
                              IF vv_success_tbc IS NULL
                              THEN
                                 vv_success_tbc :=
                                        PKG_BOOKING.cv_trans_stat_false;
                                 --DBMS
                                 pkg_plog.sec_implementation
                                    (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                     || p_bk_wbp_seq
                                     || ')',
                                     'Cancelación no fue exitosa',
                                     'INFO'
                                    );
                              END IF;         --IF vv_success_tbc IS NULL THEN

                              vn_can_wb_dy := NULL;
                              vn_can_wb_hr := NULL;
                              vn_can_wb_fee := NULL;
                              vn_can_dlt_fee := NULL;
                           END IF;               --IF vv_success = 'TRUE' THEN
                        --Si no existe TBC de este buque se hará un VOID del Booking original
                        ELSE
                           vv_stat_rmk_from :=
                                 'This booking is REQUEST VOID for a '
                              || sf_get_ref_meaning (p_bk_wbp_typ,
                                                     'BOOKING PROCESS'
                                                    )
                              || ' Transaction to new date '
                              || TO_CHAR (vr_wb_bk_requests.trn_br_date,
                                          'DD-MON-YYYY'
                                         );

                           --Se actualizan la información directamente a la tabla sin la vista
                           --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                           --UPDATE inclusive la columna de STAT, y en estos casos no se está
                           --haciendo update a dicha columna, por lo que el trigger de la
                           --misma restringe el uso de esta forma.
                           UPDATE bk_rqsts
                              SET stat =
                                        PKG_BOOKING.cv_bk_void_rqst_stat,
                                  stat_date = SYSDATE,
                                  stat_rmk = vv_stat_rmk_from
                            WHERE seq = vr_bk_rqsts_from.seq
                              AND stat = PKG_BOOKING.cv_bk_bkd_stat;

                           --Si se pudo hacer la actualización del status a VBKD RQSTS
                           --se procederá a hacer el cambio a VOID BKD
                           IF SQL%FOUND
                           THEN
                              vv_stat_rmk_from :=
                                    'This booking was VOIDED for a '
                                 || sf_get_ref_meaning (p_bk_wbp_typ,
                                                        'BOOKING PROCESS'
                                                       )
                                 || ' Transaction to new date '
                                 || TO_CHAR (vr_wb_bk_requests.trn_br_date,
                                             'DD-MON-YYYY'
                                            );

                              --Se actualizan la información directamente a la tabla sin la vista
                              --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                              --UPDATE inclusive la columna de STAT, y en estos casos no se está
                              --haciendo update a dicha columna, por lo que el trigger de la
                              --misma restringe el uso de esta forma.
                              UPDATE bk_rqsts
                                 SET stat = PKG_BOOKING.cv_bk_void_stat,
                                     stat_date = SYSDATE,
                                     stat_rmk = vv_stat_rmk_from
                               WHERE seq = vr_bk_rqsts_from.seq
                                 AND stat =
                                        PKG_BOOKING.cv_bk_void_rqst_stat;

                              IF SQL%FOUND
                              THEN
                                 --DBMS
                                 pkg_plog.sec_implementation
                                    (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                     || p_bk_wbp_seq
                                     || ')',
                                     'Registro actualizado con remarks hasta VOID BKD',
                                     'INFO'
                                    );
                                 --VOID daylight booking (if it applies)
                                 vv_error_cd :=
                                    f_void_bk_dlt (vr_bk_rqsts_from.seq,
                                                   p_bk_wbp_typ
                                                  );
                                 --DBMS
                                 pkg_plog.sec_implementation
                                    (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                     || p_bk_wbp_seq
                                     || ')',
                                        'Resultado de actualizacion de daylight='
                                     || vv_error_cd,
                                     'INFO'
                                    );

                                 IF vv_error_cd IS NULL
                                 THEN
                                    vv_success_bkd :=
                                         PKG_BOOKING.cv_trans_stat_true;
                                 ELSE
                                    vv_success_bkd :=
                                        PKG_BOOKING.cv_trans_stat_false;
                                 END IF;         --IF vv_error_cd IS NULL THEN
                              ELSE
                                 vv_success_bkd :=
                                        PKG_BOOKING.cv_trans_stat_false;
                              END IF;                      --IF SQL%FOUND THEN
                           ELSE
                              vv_success_bkd :=
                                        PKG_BOOKING.cv_trans_stat_false;
                           END IF;                         --IF SQL%FOUND THEN

                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Estatus de transaccion VOID booking='
                                 || vv_success_bkd,
                                 'INFO'
                                );
                        END IF;
                              --IF vr_wb_bk_rqst_trans.br_seq IS NOT NULL THEN

                        --Si proceso de CANCEL BOOK o VOID BOOK fue exitoso
                        --se generará un nuevo registro de booking para la nueva
                        --fecha
                        IF    vv_success_tbc =
                                          PKG_BOOKING.cv_trans_stat_true
                           OR vv_success_bkd =
                                          PKG_BOOKING.cv_trans_stat_true
                        THEN
                           pkg_plog.sec_implementation
                               (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                || p_bk_wbp_seq
                                || ')',
                                'VOY A GENERAR NUEVO BOOKING REQUEST',
                                'INFO'
                               );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'Parámetros Proceso Booking',
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_vessel_type=' || vr_wb_bk_requests.vty_cd,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'P_Visit_Seq='
                                 || TO_CHAR (vr_wb_bk_requests.visit_seq),
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Bs_Seq=' || TO_CHAR (vn_bs_seq_to),
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Iti_Seq=' || TO_CHAR (vn_iti_seq_to),
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Cust_Cd=' || vr_wb_bk_requests.cust_cd,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Real_Cust_cd=' || vv_iti_cust_cd_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'P_Date_Created='
                                 || TO_CHAR
                                          (vr_wb_bk_requests.trn_br_rqst_date,
                                           'DD-MON-YYYY HH24MI'
                                          ),
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'P_Bk Date='
                                 || TO_CHAR (vr_wb_bk_requests.trn_br_date,
                                             'DD-MON-YYYY HH24MI'
                                            ),
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Trn_Dir =' || vr_wb_bk_requests.trn_dir,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Iti_trn_dir=' || vv_iti_trn_dir_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Extm_Beam=' || TO_CHAR (vn_ext_beam_to),
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'P_Beam='
                                 || TO_CHAR (vr_wb_bk_requests.new_extm_beam),
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_HML=' || vv_hml_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_REST=' || vv_rest_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_GAS FREE=' || vv_gas_free_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_PDE CODE=' || vv_iti_pde_cd_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'P_Agent=' || vr_wb_bk_requests.AGENT,
                                 'INFO'
                                );
                           --Generar un nuevo booking para la fecha respectiva
                           p_process_booking
                                          (vr_wb_bk_requests.vty_cd,
                                           vr_wb_bk_requests.visit_seq,
                                           vn_bs_seq_to,
                                           vn_iti_seq_to,
                                           vr_wb_bk_requests.cust_cd,
                                           vv_iti_cust_cd_to,
                                           vr_wb_bk_requests.trn_br_rqst_date,
                                           vr_wb_bk_requests.trn_br_date,
                                           vr_wb_bk_requests.trn_dir,
                                           vv_iti_trn_dir_to,
                                           vn_ext_beam_to,
                                           vr_wb_bk_requests.new_extm_beam,
                                           vv_hml_to,
                                           vv_rest_to,
                                           vv_gas_free_to,
                                           vv_iti_pde_cd_to,
                                           vr_wb_bk_requests.dtu_int,
                                           vr_wb_bk_requests.AGENT,
                                           NULL,
                                           NULL,
                                           NULL,
                                           vn_br_seq_to,
                                           vv_br_stat_to,
                                           vn_bk_cond,
                                           vv_error_cd,
                                           vv_success_bkd_to,
                                           NULL,
                                           NULL,
                                           v_spc_bk_ind,
                                           NULL
                                          );
                           --DBMS
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Resultado de Process Booking ='
                                 || vv_br_stat_to,
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                              (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                               || p_bk_wbp_seq
                               || ')',
                                  'Secuencia nuevo registro Process Booking ='
                               || TO_CHAR (vn_br_seq_to),
                               'INFO'
                              );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Condicion Process Booking ='
                                 || TO_CHAR (vn_bk_cond),
                                 'INFO'
                                );
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Resultado de Error de Process Booking ='
                                 || vv_error_cd,
                                 'INFO'
                                );

                           IF NVL (vv_br_stat_to, ' ') = 'BKD'
                           THEN
                              vn_bk_wb_fee := f_get_bk_fee (vn_bs_seq_to);
                              --DBMS
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                     'Vn_bk_wb_fee_final='
                                  || TO_CHAR (vn_bk_wb_fee),
                                  'INFO'
                                 );
                              --Actualización del Stat Rmk para booking recien generado
                              vv_stat_rmk_to :=
                                    'This booking was generated due to a Change Date Transaction from date '
                                 || TO_CHAR (vr_wb_bk_requests.org_bk_date,
                                             'DD-MON-YYYY'
                                            );
                              --DBMS
                              pkg_plog.sec_implementation
                                 (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                  || p_bk_wbp_seq
                                  || ')',
                                  'Stat Rmk To=' || vv_stat_rmk_to,
                                  'INFO'
                                 );

                              --Se actualizan la información directamente a la tabla sin la vista
                              --VW_BOOKING_RQSTS ya que la misma fue diseñada para incluir en el
                              --UPDATE inclusive la columna de STAT, y en estos casos no se está
                              --haciendo update a dicha columna, por lo que el trigger de la
                              --misma restringe el uso de esta forma.
                              UPDATE bk_rqsts
                                 SET stat_rmk = vv_stat_rmk_to
                               WHERE seq = vn_br_seq_to
                                 AND stat = PKG_BOOKING.cv_bk_bkd_stat;

                              IF SQL%FOUND
                              THEN
                                 --DBMS
                                 DBMS_OUTPUT.put_line
                                                ('Hecho update de BK_RQST TO');
                                 vv_success_bkd_to :=
                                         PKG_BOOKING.cv_trans_stat_true;

                                 --
                                 -- S61519
                                 -- VJaen 16Jan2008.
                                 -- Inserta el registro detalle de CHGD para el nuevo booking.
                                 DECLARE
                                    vr_to_det   vw_bk_rqst_dets%ROWTYPE;
                                 BEGIN
                                    pkg_plog.sec_implementation
                                       (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                        || p_bk_wbp_seq
                                        || ')',
                                        'Inserta el registro detalle de CHGD para el nuevo booking.',
                                        'INFO'
                                       );
                                    -- Llena la variable de registro.
                                    vr_to_det.br_seq := vn_br_seq_to;
                                    vr_to_det.bs_seq := vn_bs_seq_to;
                                    vr_to_det.trns_type :=
                                                  pkg_book_datadict.f_bkp_chgd;
                                    vr_to_det.br_seq_dest :=
                                                          vr_bk_rqsts_from.seq;
                                    vr_to_det.bs_seq_dest :=
                                                       vr_bk_rqsts_from.bs_seq;
                                    vr_to_det.amount := vn_bk_wb_fee;
                                    -- Solicita la inserción del registro.
                                    p_ins_bk_rqst_dets (vr_to_det);
                                 END;
                              ELSE
                                 vv_success_bkd_to :=
                                        PKG_BOOKING.cv_trans_stat_false;
                              END IF;                      --IF SQL%FOUND THEN
                           ELSE
                              vv_success_bkd_to :=
                                        PKG_BOOKING.cv_trans_stat_false;

                              --S15142.T15: Added on August 08, 2007 -- GNV
                              --Set the status and message if the booking request was rejected
                              IF NVL (vv_br_stat_to, ' ') = 'REJ'
                              THEN
                                 --Get the rejection remark to send it to global variable
                                 --It will be used to display the rejection reason on the
                                 --Web Booking Request Transactions Module (VE5060FM)
                                 --This will be done because all the transaction will be
                                 --reversed
                                 BEGIN
                                    SELECT stat_rmk
                                      INTO vv_rej_stat_rmk_to
                                      FROM bk_rqsts
                                     WHERE seq = vn_br_seq_to AND stat = 'REJ';
                                 EXCEPTION
                                    WHEN OTHERS
                                    THEN
                                       vv_rej_stat_rmk_to := NULL;
                                 END;

                                 pkg_global_vars.p_set_char4 (vv_br_stat_to);
                                 pkg_global_vars.p_set_char5
                                                           (vv_rej_stat_rmk_to);
                              END IF; --IF NVL(vv_br_stat_to,' ') = 'REJ' THEN
                           END IF;    --IF NVL(vv_br_stat_to,' ') = 'BKD' THEN

                           --DBMS
                           pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'Resultado de generación nuevo booking='
                                 || vv_success_bkd_to,
                                 'INFO'
                                );
                           vn_br_wb_seq_first := vn_br_seq_to;

                           IF NVL (vv_success_bkd_to, ' ') =
                                          PKG_BOOKING.cv_trans_stat_true
                           THEN
                              vv_bk_wb_stat :=
                                         PKG_BOOKING.cv_trans_stat_true;
                           ELSE
                              vv_bk_wb_stat :=
                                        PKG_BOOKING.cv_trans_stat_false;
                           END IF;
                        --IF NVL(vv_success_bkd_to,' ') = PKG_BOOKING.cv_trans_stat_true THEN
                        ELSE
                           vn_br_wb_seq_first := NULL;
                           vn_bk_wb_fee := NULL;
                           vn_can_wb_dy := NULL;
                           vn_can_wb_hr := NULL;
                           vn_can_wb_fee := NULL;
                           vn_bk_cond := NULL;
                           vv_bk_wb_stat :=
                                        PKG_BOOKING.cv_trans_stat_false;
                        END IF;
                     --IF vv_success_tbc = PKG_BOOKING.cv_trans_stat_true OR
                     ELSE
                        vn_br_wb_seq_first := NULL;
                        vn_bk_wb_fee := NULL;
                        vn_can_wb_dy := NULL;
                        vn_can_wb_hr := NULL;
                        vn_can_wb_fee := NULL;
                        vn_bk_cond := NULL;
                        vv_bk_wb_stat :=
                                        PKG_BOOKING.cv_trans_stat_false;
                     END IF;                     --IF vv_error_cd IS NULL THEN
                  ELSE
                     vn_br_wb_seq_first := NULL;
                     vn_bk_wb_fee := NULL;
                     vn_can_wb_dy := NULL;
                     vn_can_wb_hr := NULL;
                     vn_can_wb_fee := NULL;
                     vn_bk_cond := NULL;
                     vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
                  END IF;           --IF vr_bk_rqsts_from.seq IS NOT NULL THEN
               ELSE
                  vn_br_wb_seq_first := NULL;
                  vn_bk_wb_fee := NULL;
                  vn_can_wb_dy := NULL;
                  vn_can_wb_hr := NULL;
                  vn_can_wb_fee := NULL;
                  vn_bk_cond := NULL;
                  vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
               END IF;             --IF vr_wb_bk_requests.seq IS NOT NULL THEN
            ELSE
               vn_br_wb_seq_first := NULL;
               vn_bk_wb_fee := NULL;
               vn_can_wb_dy := NULL;
               vn_can_wb_hr := NULL;
               vn_can_wb_fee := NULL;
               vn_bk_cond := NULL;
               vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            END IF;                         --IF c_web_bk_requests%ISOPEN THEN
         ELSE
            vn_br_wb_seq_first := NULL;
            vn_bk_wb_fee := NULL;
            vn_can_wb_dy := NULL;
            vn_can_wb_hr := NULL;
            vn_can_wb_fee := NULL;
            vn_bk_cond := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
         END IF;                            --IF p_bk_wbp_seq IS NOT NULL THEN
      EXCEPTION
         WHEN e_avoid_oper_for_spec_booking
         THEN
            vn_br_wb_seq_first := NULL;
            vn_bk_wb_fee := NULL;
            vn_can_wb_dy := NULL;
            vn_can_wb_hr := NULL;
            vn_can_wb_fee := NULL;
            vn_bk_cond := NULL;
            vv_bk_wb_stat := PKG_BOOKING.cv_trans_stat_false;
            pkg_plog.sec_implementation
               ('PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ=' || p_bk_wbp_seq
                || ')',
                'Current version of the system does not allow SWAP/SUBSTITUTION/CHANGE DATE operations for Auction, Best Offer or X-Piece Bookings.',
                'INFO'
               );
            -- Current version of the system does not allow SWAP/SUBSTITUTION/CHANGE DATE operations for Auction, Best Offer or X-Piece Bookings.
            vv_error_cd := 'USR-02031';
      END;

      --
      p_bk_wb_seq_first := vn_br_wb_seq_first;
      p_bk_wb_fee := vn_bk_wb_fee;
      p_can_wb_dy := vn_can_wb_dy;
      p_can_wb_hr := vn_can_wb_hr;
      p_can_wb_fee := vn_can_wb_fee;
      p_bk_cond := vn_bk_cond;
      p_bk_wb_stat := vv_bk_wb_stat;
      p_error_cd := vv_error_cd;
      --DBMS
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_CHGD p_bk_wb_seq_first ='
                                 || TO_CHAR (vn_br_wb_seq_first),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_CHGD p_bk_wb_fee ='
                                 || TO_CHAR (vn_bk_wb_fee),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_CHGD p_can_wb_dy ='
                                 || TO_CHAR (vn_can_wb_dy),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_CHGD p_can_wb_hr ='
                                 || TO_CHAR (vn_can_wb_hr),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_CHGD p_can_wb_fee ='
                                 || TO_CHAR (vn_can_wb_fee),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'BOOK_CHGD p_bk_wb_stat =' || vv_bk_wb_stat,
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                    'BOOK_CHGD p_bk_cond ='
                                 || TO_CHAR (vn_bk_cond),
                                 'INFO'
                                );
      pkg_plog.sec_implementation
                                (   'PKG_BOOKING.P_BOOK_TRANS_CHGD (WBP_SEQ='
                                 || p_bk_wbp_seq
                                 || ')',
                                 'BOOK_CHGD p_error_cd =' || vv_error_cd,
                                 'INFO'
                                );
   END p_book_trans_chgd;
/* Retrieve some itinerary information need for booking swap, substitutio */
PROCEDURE P_GET_BK_ITIN_INFO
 (P_VISIT_SEQ IN CUST_SCHED_NEEDS.SEQ%TYPE
 ,P_ITI_SEQ IN ITIN_ITEMS.SEQ%TYPE
 ,P_BS_SEQ OUT BILL_SETS.SEQ%TYPE
 ,P_REST_CD OUT REST_USR_CDS.CD%TYPE
 ,P_TRN_DIR OUT ITIN_ITEMS.TII_TRN_DIR%TYPE
 ,P_CUST_CD OUT VARCHAR2
 ,P_PDE_CD OUT ITIN_ITEMS.PDE_CD%TYPE
 )
 IS
--============================================================================
-- DESCRIPCION:
--   Este procedimiento se encarga de buscar información de BILLING SET, RESTRICCIONES,
--   DIRECCION DEL TRANSITO, CODIGO DE CUSTOMER del itinerario del buque para efectos
--   de las transacciones de SWAP, SUBSTITUTION, CHANGE DATE de booking.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador    Fecha                  Cambios realizados
-- -------------   --------------------  -------------------     -----------------------------
-- S15142      GVillarreal         05-SEP-2006      Versión Inicial



CURSOR c_itinerary_info(p_visit_seq   IN CUST_SCHED_NEEDS.SEQ%TYPE,
                                                 p_iti_seq       IN ITIN_ITEMS.SEQ%TYPE) IS
SELECT bs_seq,
               sf_get_rest_user_bk(seq),
                tii_trn_dir,
                sf_get_cust_name(seq),
                pde_cd
   FROM  itin_items
WHERE  csn_seq = p_visit_seq
      AND  seq          = p_iti_seq
      AND  typ            = 'TII'
      AND  tii_first_in_bs = 'Y'
      AND  stat_cd             = 'OPEN';


vn_bs_seq                   BILL_SETS.SEQ%TYPE;
vv_iti_rest                    REST_USR_CDS.CD%TYPE;   -- From Definition on function SF_GET_REST_USER_BK
vv_iti_trn_dir               ITIN_ITEMS.TII_TRN_DIR%TYPE;
vv_iti_cust_cd            VARCHAR2(218);       -- From Definition on function SF_GET_CUST_NAME
vv_iti_pde_cd              ITIN_ITEMS.PDE_CD%TYPE;
BEGIN

 p_bs_seq   := NULL;
 p_rest_cd  := NULL;
 p_trn_dir  := NULL;
 p_cust_cd  := NULL;
 p_pde_cd   := NULL;

 BEGIN

   IF p_visit_seq IS NOT NULL AND
      p_iti_seq   IS NOT NULL THEN

      --Get the itinerary information
      OPEN c_itinerary_info(p_visit_seq,
                            p_iti_seq);

      IF c_itinerary_info%ISOPEN THEN

         FETCH c_itinerary_info INTO vn_bs_seq,
                                     vv_iti_rest,
                                     vv_iti_trn_dir,
                                     vv_iti_cust_cd,
                                     vv_iti_pde_cd;

         IF c_itinerary_info%NOTFOUND THEN

            --If cursor does not retrieved any itinerary record
            vn_bs_seq     := NULL;
            vv_iti_rest   := NULL;
            vv_iti_trn_dir := NULL;
            vv_iti_cust_cd := NULL;
            vv_iti_pde_cd  := NULL;

         END IF;

         CLOSE c_itinerary_info;

      ELSE

          --If cursor was not opened
          vn_bs_seq     := NULL;
          vv_iti_rest   := NULL;
          vv_iti_trn_dir := NULL;
          vv_iti_cust_cd := NULL;
          vv_iti_pde_cd  := NULL;

      END IF; --IF c_itinerary_info%ISOPEN THEN

   END IF;

 EXCEPTION
 WHEN OTHERS THEN

   vn_bs_seq     := NULL;
   vv_iti_rest   := NULL;
   vv_iti_trn_dir := NULL;
   vv_iti_cust_cd := NULL;
   vv_iti_pde_cd  := NULL;


 END;

 p_bs_seq     := vn_bs_seq;
 p_rest_cd    := substr(vv_iti_rest,1,4); --From use established on Procedure P_PROCESS_BOOKING_RQSTS Screen VI5060FM
 p_trn_dir    := vv_iti_trn_dir;
 p_cust_cd    := vv_iti_cust_cd;
 p_pde_cd     := vv_iti_pde_cd;

END;
/* Get the vessel characteristics for visit */
PROCEDURE P_GET_CSN_VSL_CHRS
 (P_SHIP_NO IN SHIP_ID_NO.SHIP_NO%TYPE
 ,P_VISIT_SEQ IN CUST_SCHED_NEEDS.SEQ%TYPE
 ,P_HML OUT CUST_VSL_CHARS.LVC_HML_QUALIFIED%TYPE
 ,P_EXTM_BEAM OUT CUST_VSL_CHARS.EXTM_BEAM%TYPE
 ,P_GAS_FREE OUT CUST_VISIT_DETS.TBLC_GAS_FREE_IND%TYPE
 )
 IS
--============================================================================
-- DESCRIPCION:
--   Este procedimiento se encarga de buscar información de CUST_VSL_CHARS del buque para
--   efectos de las transacciones de SWAP, SUBSTITUTION, CHANGE DATE de booking.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador    Fecha                  Cambios realizados
-- -------------   --------------------  -------------------     -----------------------------
-- S15142      GVillarreal         05-SEP-2006      Versión Inicial



vt_blck_control         PKG_CSN_VSL_CHARS.csn_cvc_table;

vv_vsl_typ                  SHIP_ID_NO.vty_cd%TYPE;

vv_hml                         CUST_VSL_CHARS.lvc_hml_qualified%TYPE;
vn_extm_beam         CUST_VSL_CHARS.extm_beam%TYPE;
vv_gas_free               CUST_VISIT_DETS.tblc_gas_free_ind%TYPE;
BEGIN

  p_hml       := NULL;
  p_extm_beam := NULL;
  p_gas_free  := NULL;

  BEGIN

     --Búsqueda de características de HML y EXTM_BEAM del buque
     --para la visita
     vt_blck_control := PKG_CSN_VSL_CHARS.f_get_csn_chars('V',
                                                          p_visit_seq,
                                                          NULL,
                                                          'L');

     --Asignación de la información de beam y hml
     vn_extm_beam    := vt_blck_control(1).extm_beam;
     vv_hml          := vt_blck_control(1).hml;

     --Asignación de vessel type para evaluación de Gas Free Indicator
     vv_vsl_typ      := vt_blck_control(1).vty_cd;

     IF vv_hml IS NULL THEN

        --Busca el HML permanente, en caso de que no exista a nivel
        --de visita
        BEGIN

           SELECT cvc.perm_hl_ind
             INTO vv_hml
             FROM cust_vsl_chars cvc
            WHERE cvc.date_to(+)  = to_date('31-DEC-4712 2359','DD-MON-YYYY HH24MI')
              AND cvc.sin_seq     = (SELECT seq
                                       FROM ship_id_no
                                      WHERE ship_no = p_ship_no);
        EXCEPTION
            WHEN OTHERS THEN

              vv_hml := NULL;

        END;

     END IF;  --IF vv_hml IS NULL THEN

     --Búsqueda de gas free indicator para buques tipo tanquero

      --SOS: S165476_T9
    --Desarrollador: Lourdes Cedeño
    --Cambio: Adicion de los códigos 28, 29, 30, 31 y 32 en la lista de VTY_CD

     --IF NVL(vv_vsl_typ,' ') in ('04','05') THEN
     IF NVL(vv_vsl_typ,' ') in ('04','05','28','29','30','31','32') THEN

        BEGIN

          SELECT tblc_gas_free_ind
            INTO vv_gas_free
            FROM cust_visit_dets
           WHERE csn_seq = p_visit_seq;

        EXCEPTION
           WHEN OTHERS THEN
                vv_gas_free := NULL;

        END;

     END IF;

  EXCEPTION
    WHEN OTHERS THEN

     vv_hml       := NULL;
     vn_extm_beam := NULL;
     vv_gas_free  := NULL;

  END;

  p_hml       := vv_hml;
  p_extm_beam := vn_extm_beam;
  p_gas_free  := vv_gas_free;

END;
/* Get information of some columns on BK_RQSTS table */
PROCEDURE P_GET_BK_RQST_INFO
 (P_BK_ID IN BK_RQSTS.SEQ%TYPE
 ,P_BK_RQST_REC OUT PKG_BOOKING.TR_BK_RQSTS
 )
 IS
--============================================================================
-- DESCRIPCION:
--   Este procedimiento se encarga de buscar información de BK_RQSTS del itinerario del buque
--   para efectos  de las transacciones de SWAP, SUBSTITUTION, CHANGE DATE de booking.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador    Fecha                  Cambios realizados
-- -------------   --------------------  -------------------     -----------------------------
-- S15142      GVillarreal         05-SEP-2006      Versión Inicial


CURSOR c_bk_rqsts_info(p_br_seq  IN  BK_RQSTS.SEQ%TYPE) IS
SELECT seq,
                stat,
                bs_seq,
                rqst_date,
                rqst_bk_date,
                stat_rmk,
                bd_seq,
                br_seq_swap_from,
                bs_seq_swap,
                fix_amt,
                bsps_seq
   FROM  bk_rqsts
WHERE  seq = p_br_seq
       AND stat = 'BKD';


vr_bk_rqsts          PKG_BOOKING.TR_BK_RQSTS;
BEGIN

   BEGIN

       --Búsqueda de información de BK_RQSTS
       --La información encontrada será retornada
       --a través de un record type
       OPEN c_bk_rqsts_info(p_bk_id);

       IF c_bk_rqsts_info%ISOPEN THEN

          FETCH c_bk_rqsts_info INTO vr_bk_rqsts;

          IF c_bk_rqsts_info%NOTFOUND THEN

             vr_bk_rqsts := NULL;

          END IF;

          CLOSE c_bk_rqsts_info;

       ELSE

          vr_bk_rqsts := NULL;

       END IF; --IF c_bk_rqsts_info%ISOPEN THEN

   EXCEPTION
     WHEN OTHERS THEN

     vr_bk_rqsts := NULL;

   END;

   p_bk_rqst_rec := vr_bk_rqsts;

END;
/* Evaluate the rules for the booking transactions */
PROCEDURE P_EVAL_BK_TRAN_RULES
 (P_BK_WBP_TYP IN WEB_BK_PROCS.TYP%TYPE
 ,P_WB_BK_FROM_SHIPNO IN WEB_BK_REQUESTS.SHIP_NO%TYPE
 ,P_WB_BK_FROM_CUST_CD IN WEB_BK_REQUESTS.CUST_CD%TYPE
 ,P_WB_BK_FROM_TRN_DIR IN WEB_BK_REQUESTS.TRN_DIR%TYPE
 ,P_ITI_REST_FROM IN REST_USR_CDS.CD%TYPE
 ,P_ITI_TRN_DIR_FROM IN ITIN_ITEMS.TII_TRN_DIR%TYPE
 ,P_ITI_CUST_CD_FROM IN VARCHAR2
 ,P_BK_REQ_ARR_FROM IN VARCHAR2
 ,P_BK_DATE_FROM IN BK_RQSTS.RQST_BK_DATE%TYPE
 ,P_BK_FIX_AMT_FROM IN BK_RQSTS.FIX_AMT%TYPE
 ,P_WB_BK_TO_SHIPNO IN WEB_BK_REQUESTS.SHIP_NO%TYPE
 ,P_WB_BK_TO_CUST_CD IN WEB_BK_REQUESTS.CUST_CD%TYPE
 ,P_WB_BK_TO_TRN_DIR IN WEB_BK_REQUESTS.TRN_DIR%TYPE
 ,P_ITI_REST_TO IN REST_USR_CDS.CD%TYPE
 ,P_ITI_TRN_DIR_TO IN ITIN_ITEMS.TII_TRN_DIR%TYPE
 ,P_ITI_CUST_CD_TO IN VARCHAR2
 ,P_BK_REQ_ARR_TO IN VARCHAR2
 ,P_BK_DATE_TO IN BK_RQSTS.RQST_BK_DATE%TYPE
 ,P_BK_FIX_AMT_TO IN BK_RQSTS.FIX_AMT%TYPE
 ,P_ERROR_CODE OUT VARCHAR2
 ,P_HML_REST_FROM IN VARCHAR2 := NULL
 ,P_HML_REST_TO IN VARCHAR2 := NULL
 ,P_SIZE_FROM IN VARCHAR2 := NULL
 ,P_SIZE_TO IN VARCHAR2 := NULL
 )
 IS
--============================================================================
-- DESCRIPCION:
--   Este procedimiento se encarga validar las reglas aplicables a las Transacciones
--  de Swapping, Substitution, Change Date para Booking generadas desde EDCS, de acuerdo al tipo --  de  transaccion a realizar.

-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador    Fecha                  Cambios realizados
-- -------------   --------------------  -------------------     -----------------------------
-- S15142      GVillarreal         05-SEP-2006      Versión Inicial

vv_error_code                  VARCHAR2(30);

e_same_ship_no              EXCEPTION;
e_invalid_cust_cd            EXCEPTION;
e_invalid_trn_dir               EXCEPTION;
e_fix_amt                            EXCEPTION;
e_days_between_bk        EXCEPTION;
e_dif_rest                            EXCEPTION;
e_dif_cust_cd                    EXCEPTION;
e_dif_trn_dir                       EXCEPTION;
e_dif_req_arr                     EXCEPTION;
e_fix_amt_chgd                EXCEPTION;
e_fix_amt_subs                EXCEPTION;
--
e_dif_size                            EXCEPTION;
BEGIN
 BEGIN

   p_error_code := NULL;

   --Validaciones de datos entre TBR y Base de Datos de EVTMS para la transaccion
   --de booking

   IF p_bk_wbp_typ IN (pkg_booking.cv_swap_cd,pkg_booking.cv_subs_cd,pkg_booking.cv_chgd_cd) THEN
      --Validar Customer Code de Itinerario y TBR de Buque ORIGEN (FROM) Y
      --Customer Code de Itinerario y TBR de Buque ORIGEN (TO)

      --DBMS
      dbms_output.put_line('Estoy en SET #1 Reglas SWAP, SUBS CHGD');


      IF ( NVL(p_wb_bk_from_cust_cd,' ') <> NVL(p_iti_cust_cd_from,'@') ) OR
         ( NVL(p_wb_bk_to_cust_cd,' ')   <> NVL(p_iti_cust_cd_to,'@') )   THEN

         RAISE e_invalid_cust_cd;

      END IF;

      --Validar Transit Direction de Itinerario y TBR de Buque ORIGEN (FROM) Y
      --Transit Direction de Itinerario y TBR de Buque ORIGEN (TO)
      IF ( NVL(p_wb_bk_from_trn_dir,' ') <> NVL(p_iti_trn_dir_from,'@') ) OR
         ( NVL(p_wb_bk_to_trn_dir,' ')   <> NVL(p_iti_trn_dir_to,'@') )   THEN

         RAISE e_invalid_trn_dir;

      END IF;

   END IF;

   ----Reglas Generales Aplicables para transacciones múltiples
   IF p_bk_wbp_typ IN (pkg_booking.cv_swap_cd,pkg_booking.cv_subs_cd) THEN

      --DBMS
      dbms_output.put_line('Estoy en SET #2 Reglas SWAP, SUBS');

      --Validar Ship Number Buque ORIGEN (FROM) igual a Ship Number DESTINO (TO)
      IF NVL(p_wb_bk_from_shipno,0) = NVL(p_wb_bk_to_shipno,0) THEN

         RAISE e_same_ship_no;

      END IF;

      --Validar diferencias en Customer Codes entre buques ORIGEN y DESTINO
      IF NVL(p_iti_cust_cd_from,' ') != NVL(p_iti_cust_cd_to,' ') THEN

         RAISE e_dif_cust_cd;

      END IF;

      --Validar direccion de transito de los itinerarios de los buques ORIGEN y DESTINO
      IF NVL(p_iti_trn_dir_from,' ') != NVL(p_iti_trn_dir_to,' ') THEN

         RAISE e_dif_trn_dir;

      END IF;

      --Validar tamaños de los buques ORIGEN y DESTINO
      IF NVL(p_size_from,' ') != NVL(p_size_to,' ') THEN

         RAISE e_dif_size;

      END IF;

   END IF;

   --Validaciones de reglas de negocio aplicables por transaccion
   --Reglas Particulares de transaccion SWAP
   IF p_bk_wbp_typ IN (pkg_booking.cv_swap_cd) THEN

      --DBMS
      dbms_output.put_line('Estoy en SET #3 Reglas SWAP');

      --Validar si Buque ORIGEN (FROM) y Buque DESTINO (TO) tienen
      --reservación por subasta.
      --S15142: A Septiembre de 2006 se determinó que en segunda fase
      --        se implementará el Swap de Buques Subastados. Por el
      --        momento se tomará como una restricción hasta que se
      --        definan los requerimientos reales para esta operación
      /*S15142: Desactivado en Septiembre 08, 2006 -- Desde ambiente
       DEV. Se determinó dado aumento del tiempo para desarrollo, suprimir
            la validación de esta regla.
      IF ( p_bk_fix_amt_from IS NOT NULL ) OR
         ( p_bk_fix_amt_to   IS NOT NULL ) THEN

         RAISE e_fix_amt;

      END IF;

      */

      --Validar cantidad de días entre ambas fechas de booking
      IF ABS(TRUNC(p_bk_date_from) - TRUNC(p_bk_date_to)) > sf_get_param_val(810,'BOOKING') THEN

         RAISE e_days_between_bk;

      END IF;

       --Validar restricciones aplicadas entre buques ORIGEN y DESTINO

      IF ( p_size_from = PKG_BOOK_DATADICT.f_bk_typ_small AND
           NVL(p_iti_rest_from,' ') != NVL(p_iti_rest_to,' ')) OR
         ( p_size_from = PKG_BOOK_DATADICT.f_bk_typ_large AND
           p_hml_rest_from <> p_hml_rest_to)
            THEN

         RAISE e_dif_rest;

      END IF;

      --Validar hora de arribo entre buques ORIGEN y DESTINO
      IF NVL(p_bk_req_arr_from,' ') != NVL(p_bk_req_arr_to,' ') THEN

         RAISE e_dif_req_arr;

      END IF;

   END IF;

   --Validaciones de reglas de negocio aplicables por transaccion
   --Reglas Particulares de transaccion SUBS
   IF p_bk_wbp_typ IN (pkg_booking.cv_subs_cd) THEN

      --DBMS
      dbms_output.put_line('Estoy en SET #4 Reglas SUBS');

      --Validación de restricción entre buque FROM y TO
      --Si buque FROM no tiene restricción, buque TO no debe tenerla
      IF ( p_size_from = PKG_BOOK_DATADICT.f_bk_typ_small AND
           p_iti_rest_from IS NULL   AND p_iti_rest_to IS NOT NULL) OR
         ( p_size_from = PKG_BOOK_DATADICT.f_bk_typ_large AND
           p_hml_rest_from = 'N' AND p_hml_rest_to = 'Y')
           THEN

         RAISE e_dif_rest;

      END IF; --IF p_iti_rest_from IS NULL THEN

      --Si cantidad de Fix Amount viene en TBR de buque TO
      --se rechaza el TBR y la transacción
      /*
         SOS 100983
         Desarrollador Abdel E. Miranda S.
         Fecha Junio 06, 2009

         Se modifica el IF de evaluacion para que no evalue
         por el valor nulo de la variable sino que evalue
         por el valor mayor a cero

      IF p_bk_fix_amt_to IS NOT NULL THEN

         RAISE e_fix_amt_subs;

      END IF;
      */
      IF p_bk_fix_amt_to > 0 THEN

         RAISE e_fix_amt_subs;

      END IF;

   END IF; --IF p_bk_wbp_typ IN ('SUBS') THEN

   --Reglas Particulares de transaccion CHANGE DATE
   IF p_bk_wbp_typ IN (pkg_booking.cv_chgd_cd) THEN

      --DBMS
      dbms_output.put_line('Estoy en SET #5 Reglas CHGD');

      --Validar si Buque ORIGEN (FROM) y Buque DESTINO (TO) tienen
      --reservación por subasta.
      --S15142: A Septiembre de 2006 se determinó que en segunda fase
      --        se implementará el Swap de Buques Subastados. Por el
      --        momento se tomará como una restricción hasta que se
      --        definan los requerimientos reales para esta operación
      /* Solo se aplicará esta regla para change date de acuerdo al
         Advisory Shipping No.A-7-2006*/
      IF ( p_bk_fix_amt_from IS NOT NULL ) OR
         ( p_bk_fix_amt_to   IS NOT NULL ) THEN

         RAISE e_fix_amt_chgd;

      END IF;

   END IF;

 EXCEPTION
  WHEN e_same_ship_no THEN

    vv_error_code := 'USR-00144';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-00144','Vessels have the same Ship Number.');

  WHEN e_invalid_cust_cd THEN

    vv_error_code := 'USR-30110';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-30110','This booking request and the current/selected itinerary have diferent Customer Codes.');

  WHEN e_invalid_trn_dir THEN

    vv_error_code := 'USR-30108';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-30108','Booking request with itinerary of different transit direction');

  WHEN e_fix_amt THEN

    vv_error_code := 'USR-30300';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-30300','Vessel with auctioned booking slot can not be included in a booking swap.');

  WHEN e_days_between_bk THEN

    vv_error_code := 'USR-30301';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-30301','The vessel must be booked within :1 consecutive days.');

  WHEN e_dif_rest THEN

    vv_error_code := 'USR-00141';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-00141','Vessels must have the same restrictions.');

  WHEN e_dif_cust_cd THEN

    vv_error_code := 'USR-30302';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-30302','Vessels must have the same customer codes.');

  WHEN e_dif_trn_dir THEN

    vv_error_code := 'USR-00142';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-00142','Vessels must have the same direction.');

  WHEN e_dif_req_arr THEN

    vv_error_code := 'USR-00143';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-00143','Vessels must have the same arrival time required.');

  WHEN e_fix_amt_chgd THEN

    vv_error_code := 'USR-30306';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-30306','Vessels with auctioned booking slots can not change the booking date.');

  WHEN e_fix_amt_subs THEN

    vv_error_code := 'USR-30307';
    --Para permitir envío de código de error a proceso pantalla Web Booking
    --Pkg_Evtms_Db_Util.p_generic_exception('USR-30307','Vessels with auction information in TBR can not be included in a booking substitution.');

  WHEN e_dif_size THEN

     vv_error_code := 'USR-30789';

 END;

 p_error_code := vv_error_code;

 --DBMS
 dbms_output.put_line('Error a Retornar = '||vv_error_code);

END;
/* Get information about a previous swap. */
FUNCTION F_BK_FIND_SWAP
 (P_BK_SEQ IN BK_RQSTS.SEQ%TYPE
 )
 RETURN NUMBER
 IS
--============================================================================
-- DESCRIPCION:
--   Este procedimiento se encarga de buscar si una reservación ha sido previamente swapped
--   para efectos de las transacciones de SWAP, SUBSTITUTION, CHANGE DATE de booking.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador    Fecha                  Cambios realizados
-- -------------   --------------------  -------------------     -----------------------------
-- S15142      GVillarreal         05-SEP-2006      Versión Inicial

vn_bk_val                 BK_RQSTS.SEQ%TYPE;

CURSOR c_find_swap_son(p_bk_seq_from IN BK_RQSTS.SEQ%TYPE) IS
 SELECT  seq
    FROM  bk_rqsts
   WHERE ( br_seq_swap_from = p_bk_seq_from   AND
                    br_seq_swap_from IS NOT NULL);


CURSOR c_find_swap_father(p_bk_seq_to IN BK_RQSTS.SEQ%TYPE) IS
SELECT br_seq_swap_from
   FROM bk_rqsts
  WHERE seq = p_bk_seq_to;
BEGIN
  -- Find sequence of son for swap of booking
  OPEN c_find_swap_son(p_bk_seq);

  IF c_find_swap_son%ISOPEN THEN

     -- If return val the p_bk_seq is father and the returned value is the son
     FETCH c_find_swap_son INTO vn_bk_val;

     IF c_find_swap_son%NOTFOUND THEN

        --No son was found
        vn_bk_val := NULL;

     END IF;

  ELSE

    --No son was found
    vn_bk_val := NULL;

  END IF;

  IF vn_bk_val IS NULL THEN

     /*if not found the previous record then the system find the father of the current
       booking if exists*/

     OPEN c_find_swap_father(p_bk_seq);

     IF c_find_swap_father%ISOPEN THEN

        FETCH c_find_swap_father INTO vn_bk_val;

        --No father was found for the booking
        IF c_find_swap_father%NOTFOUND THEN

           vn_bk_val := NULL;

        END IF;

     ELSE

        --No father was found
        vn_bk_val := NULL;

     END IF;

  END IF;

  RETURN(vn_bk_val);

EXCEPTION
  WHEN OTHERS THEN

     --Error in search for booking previous swap
     RETURN(NULL);

END;
/* Update Daylight Transit Bookings to VOID them. */
FUNCTION F_VOID_BK_DLT
 (P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_WBP_TYP IN WEB_BK_PROCS.TYP%TYPE
 )
 RETURN VARCHAR2
 IS
--===============================================================================
-- DESCRIPCION:
--   Este procedimiento se encarga de realizar una transacción de VOID a Daylight Transit Bookings  --   que pertenezcan a bookings originales involucrados en transacciones de  SWAP,
--   SUBSTITUTION, CHANGE DATE.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--===============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker     Desarrollador    Fecha                  Cambios realizados
-- -------------   --------------------  -------------------     -----------------------------
-- S15142      GVillarreal          12-SEP-2006      Versión Inicial


CURSOR c_daylight_bk(p_br_seq    IN BK_RQSTS.seq%TYPE) IS
SELECT  count(*)
   FROM   bk_dlt_rqsts
 WHERE  br_seq = p_br_seq
       AND  stat       = pkg_booking.cv_dlt_guar_stat;

vn_dlt_count          NUMBER(10);
vv_dlt_rmk             BK_DLT_RQSTS.stat_rmk%TYPE;
vv_error_cd           VARCHAR2(30);
vv_error_mes       VARCHAR2(2000);
BEGIN

   BEGIN

      --Búsqueda de información de daylight transit
      --garantizados para ser anulados
      OPEN c_daylight_bk(p_br_seq);

      IF c_daylight_bk%ISOPEN THEN

         FETCH c_daylight_bk INTO vn_dlt_count;

         IF c_daylight_bk%NOTFOUND THEN

            vn_dlt_count := NULL;

         END IF;

         CLOSE c_daylight_bk;

      ELSE

         vn_dlt_count := NULL;

      END IF;

      --Si existen daylight transit garantizados
      --se realizarán los cambios de estatus
      --en el siguiente flujo (de acuerdo a
      --aprobación MR de 12 Septiembre 2006
      --GUARANTEED -->
      --           VOID DAYLIGHT REQUEST
      --                             -->VOID DAYLIGHT ->(PENDING) (automático)
      --                                                        --> REJECT
      IF NVL(vn_dlt_count,0) > 0 THEN

         vv_dlt_rmk := 'This daylight booking is request VOID for a '||sf_get_ref_meaning(p_wbp_typ,'BOOKING PROCESS')||' transaction.';

         /*
         --Actualización de Daylight Transit a estatus VOID DAYLIGHT REQUEST
         UPDATE bk_dlt_rqsts
            SET stat     = pkg_booking.cv_dlt_void_rqst_stat,
                stat_rmk = vv_dlt_rmk
          WHERE br_seq   = p_br_seq
            AND stat     = pkg_booking.cv_dlt_guar_stat;

         */

         pkg_evtms_db_util.p_lock_table_rows('BK_DLT_RQSTS',FALSE,'BR_SEQ = :1 AND STAT = :2',p_br_seq,pkg_booking.cv_dlt_guar_stat);

         UPDATE vw_day_book_rqsts
            SET stat      = pkg_booking.cv_dlt_void_rqst_stat,
                stat_rmk  = vv_dlt_rmk,
                stat_date = sysdate
          WHERE br_seq   = p_br_seq
            AND stat     = pkg_booking.cv_dlt_guar_stat;

         IF SQL%FOUND THEN

            vv_dlt_rmk := 'This daylight booking was VOIDED for a '||sf_get_ref_meaning(p_wbp_typ,'BOOKING PROCESS')||' transaction.';

            /*
            --Actualización de Daylight Transit a estatus VOID DAYLIGHT, el cual
            --representa un pase automático del sistema a estatus DTPEND
            UPDATE bk_dlt_rqsts
               SET stat     = pkg_booking.cv_dlt_void_stat,
                   stat_rmk = vv_dlt_rmk
             WHERE br_seq   = p_br_seq
               AND stat     = pkg_booking.cv_dlt_void_rqst_stat;
            */

            UPDATE vw_day_book_rqsts
               SET stat     = pkg_booking.cv_dlt_void_stat,
                   stat_rmk = vv_dlt_rmk,
                   stat_date = sysdate
             WHERE br_seq   = p_br_seq
               AND stat     = pkg_booking.cv_dlt_void_rqst_stat;

            IF SQL%FOUND THEN

               vv_dlt_rmk := 'This daylight booking was REJECTED for a '||sf_get_ref_meaning(p_wbp_typ,'BOOKING PROCESS')||' transaction.';

               /*
               --Actualización de Daylight Transit a estatus REJECTED, desde
               --el estatus PENDING
               UPDATE bk_dlt_rqsts
                  SET stat     = pkg_booking.cv_dlt_rej_stat,
                     stat_rmk = vv_dlt_rmk
               WHERE br_seq   = p_br_seq
                 AND stat     = pkg_booking.cv_dlt_pend_stat;
               */

               UPDATE vw_day_book_rqsts
                  SET stat     = pkg_booking.cv_dlt_rej_stat,
                     stat_rmk = vv_dlt_rmk,
                     stat_date = sysdate
               WHERE br_seq   = p_br_seq
                 AND stat     = pkg_booking.cv_dlt_pend_stat;

            ELSE

               vv_error_cd := 'USR-30308';

            END IF;

         ELSE

            vv_error_cd := 'USR-30308';

         END IF;

      END IF;

   EXCEPTION
    WHEN OTHERS THEN

      vv_error_mes := SQLERRM;

      IF INSTR(vv_error_mes, 'USR-') > 0 THEN

         vv_error_cd := SUBSTR(vv_error_mes, INSTR(vv_error_mes, 'USR-'), 9);


      ELSE

         IF INSTR(vv_error_mes, 'USR-') = 0 THEN

            vv_error_cd   := SQLCODE;

         ELSE

            vv_error_cd   := SQLCODE;

         END IF;

      END IF;

   END;

   RETURN(vv_error_cd);

END;
/* Verify if exist slot for a given period,day,type */
FUNCTION F_EXIST_SLOT
 (P_PER IN BK_PERS.ID%TYPE
 ,P_BD_SEQ IN BK_RQSTS.BD_SEQ%TYPE
 ,P_SLOT_TYP IN BK_RQSTS.TYP%TYPE
 ,P_RQSTS_DATE IN DATE
 ,P_CLASS IN BK_SIZE_TYPS.CLASS%TYPE
 )
 RETURN BOOLEAN
 IS

VN_AVAIL_NUM NUMBER(2);
--===============================================================================
-- DESCRIPCION: Esta función sirve para accesar la información de slot availables
--              para una clase en particular. Version de la funcion SF_EXIST_SLOT
--
-- NOTAS: ESTA FUNCION ESTA DENTRO DEL PAQUETE PKG_BOOKING Y ES DE USO EXCLUSIVO
--        DEL PROCESO P_PROCESS_BOOKING. NO UTILIZAR PARA MAS PROCESOS NI MODULOS
--
-- REQUERIMIENTOS:
--
--
--================================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker       Desarrollador         Fecha                Cambios realizados
-- -----------   -------------------   -----------------    -----------------------------------------
-- S59588/       GVillarreal           11-Jan-2008          Version Inicial.
-- S61519
BEGIN

	SELECT slots_ava
      INTO vn_avail_num
	  FROM slot_availables
	 WHERE typ   = p_slot_typ
	   AND bp_id = p_per
	   AND seq   = p_bd_seq
	   --S59588/S61519: Added on January 10, 2007 -- GNV
	   --to get slot availables from particular class
	   AND bsps_seq = PKG_BOOK_REFTAB_UTIL.f_get_bk_bsps_seq(p_bd_seq,
	                                                         p_slot_typ,
	                                                         p_per,
	                                                         p_class)
       --Addition ends
       AND TRUNC(rqsts_date) = TRUNC(p_rqsts_date);

    IF vn_avail_num <=0 OR vn_avail_num IS NULL THEN

       RETURN FALSE;

    ELSE

       RETURN TRUE;

    END IF;--IF vn_avail_num <=0 OR vn_avail_num IS NULL THEN

EXCEPTION
  WHEN NO_DATA_FOUND THEN

    RETURN FALSE;

  WHEN OTHERS THEN

    RETURN FALSE;

END;
FUNCTION F_GET_BK_RQST_DETS
 (P_BR_SEQ BK_RQSTS.SEQ%TYPE
 ,P_TRNS_TYPE BK_RQST_DETS.TRNS_TYPE%TYPE
 ,P_INFO_IND INTEGER
 )
 RETURN NUMBER
 IS
-- ======================================================================================================
-- DESCRIPCION:
--
-- NOTAS: Esta funcion obtiene informacion registrada en la tabla BK_RQST_DETS basado en la secuencia
--        del BOOKING REQUEST y en TRANSACTION TYPE. Adicionalmente y segun el indicador de informacion
--        esta obtiene distintos tipo de datos.
--
--             1. AMOUNT -- Monto de la transaccion
--             2. FIX_AMT -- Monto de subasta de ser registrado
--             3. BR_SEQ_DEST -- Secuencia del Booking request de destino (Swap, Subts & Chrg Date)
--             4. BS_SEQ_DEST -- Secuencia del Billing set de destino (Swap, Subts & Chrg Date)
--
-- REQUERIMIENTOS:
--
-- ======================================================================================================
-- HISTORIA DE MODIFICACIONES
-- SOS       Desarrollador   Fecha         Cambios realizados
-- --------- --------------  -----------   -------------------------------------------------------
-- S61519    JADIAZ          08-JAN-2008   Initial Version
-- ======================================================================================================
--
-- Local Declarations
vn_info number;
Cursor C_dets IS
SELECT decode(P_info_ind,1,brd.amount,
                         2,brd.fix_amt,
                         3,brd.bs_seq_dest,
                         4,brd.br_seq_dest,
                           null) info
FROM   BK_RQST_DETS brd
WHERE  brd.br_seq    = p_br_seq
AND    brd.trns_type = p_trns_type
AND    brd.deleted   = 'N';
--
BEGIN
  --
  OPEN  c_dets;
  FETCH c_dets INTO vn_info;
  CLOSE c_dets;
  --
  RETURN vn_info;
  --
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    RETURN NULL;
  WHEN OTHERS THEN
    pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);
END F_GET_BK_RQST_DETS;
FUNCTION F_GET_ACT_BP_ID
 (PV_SLOT_TYP IN BK_SIZE_TYPS.TYP%TYPE
 ,PV_CLASS IN BK_SIZE_TYPS.CLASS%TYPE
 ,PD_RQST_BK_DATE IN BK_RQSTS.RQST_BK_DATE%TYPE
 ,PN_BP_ID IN BK_RQSTS.BP_ID%TYPE
 )
 RETURN NUMBER
 IS
--==================================================================================================
-- DESCRIPCION: Esta función sera utiliza para obtener el periodo de la reservacion basado en su
--              tipo y clase, comparado contra la fecha del sistema. La misma sea utiliza al realizar
--              un cambio de REG/XPIECE.
--
-- NOTAS: ESTA FUNCION ESTA DENTRO DEL PAQUETE PKG_BOOKING Y ES DE USO INTERNO
--        DEL PAQUETE.
--
-- REQUERIMIENTOS:
--
--
--===================================================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker       Desarrollador         Fecha                Cambios realizados
-- -----------   -------------------   -----------------    -----------------------------------------
-- S#####        JaDiaz                08-Feb-2008          Version Inicial.
--
-- Local Declarations
Cv_sysdate        CONSTANT DATE                       := TRUNC(Sysdate);
Vd_rqst_bk_date            BK_RQSTS.RQST_BK_DATE%TYPE := TRUNC(Pd_rqst_bk_date);
Vn_bp_id                   BK_RQSTS.BP_ID%TYPE;
Vn_days                    NUMBER;
--
CURSOR C_Bp_Id(P_slot_typ Bk_size_typs.typ%type
              ,P_class Bk_size_typs.Class%type
              ,P_days Number) Is
SELECT Bps.Bp_Id
  FROM Bk_Size_Per_Segs Bsps,
       Bk_Per_Segs      Bps,
       Bk_Size_Typs     Bstp
 WHERE Bsps.Bps_Seq  = Bps.Seq
   And Bsps.Bstp_Seq = Bstp.Seq
   And Bsps.Active   = 'Y'
   And Bstp.Typ      = P_Slot_Typ
   And Bstp.Class    = P_Class
   And P_days Between Start_Day And End_Day;
--
BEGIN
  --
  -- Se verifica si la fecha de booking es mayor a la fecha del sistema, ya que de lo contrario el resultado seria negativo o cero
  IF Vd_rqst_bk_date > Cv_Sysdate THEN
     --
     -- Se dereivan los dias
     Vn_days := Vd_rqst_bk_date - Cv_Sysdate;
     --
     -- Se obtiene el periodo basado en el tipo, clase y dias a transcurrir
     OPEN  C_Bp_Id(Pv_slot_typ,Pv_class,Vn_days);
     FETCH C_Bp_Id INTO Vn_bp_id;
     CLOSE C_Bp_Id;
     --
     -- Se valida si luego de obtener el periodo, este es nulo para entonces definirlo
	 -- con el valor original de la reservacion
     IF Vn_bp_id IS NULL THEN
        --
        Vn_bp_id := Pn_bp_id;
        --
     END IF;
     --
  ELSE
     --
     -- Si la fecha de booking es menor a la fecha del sistema o nula, entonces se
	 -- define el periodo con el valor original de la reservacion.
     Vn_bp_id := Pn_bp_id;
     --
  END IF;
  --
  RETURN Vn_bp_id;
  --
EXCEPTION
  WHEN OTHERS THEN
    pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);
END F_GET_ACT_BP_ID;
PROCEDURE P_PROCESS_SMALL_RQSTS_CHGS
 (P_BR_SEQ BK_RQSTS.SEQ%TYPE
 ,P_BSPS_SEQ BK_RQSTS.BSPS_SEQ%TYPE
 ,P_XPIECE_IND VARCHAR2
 ,P_LOA CUST_VSL_CHARS.LEN_OVERALL%TYPE
 ,P_REST REST_USR_CDS.CD%TYPE
 ,P_TRN_DIR ITIN_ITEMS.TII_TRN_DIR%TYPE
 )
 IS
 --
 -- Local Declarations
 vn_org_bsps_seq   BK_SIZE_PER_SEGS.SEQ%TYPE        := P_bsps_seq; -- Original booking size per segment sequence
 Vn_bsps_seq       BK_SIZE_PER_SEGS.SEQ%TYPE;                      -- Booking size per segment sequence
 Vv_stat           BK_RQSTS.STAT%TYPE;                             -- Status de la reservacion
 Vd_rqst_bk_date   BK_RQSTS.RQST_BK_DATE%TYPE;                     -- Booking date
 Vd_rqst_date      BK_RQSTS.RQST_DATE%TYPE;                        -- Request date
 Vn_bco_lev        BK_COND_TYPS.BCO_LEV%TYPE;                      -- Condition level
 Vv_slot_typ       BK_RQSTS.TYP%TYPE;                              -- Typ (small,large)
 Vn_bd_seq         BK_RQSTS.BD_SEQ%TYPE;                           -- Booking day sequence
 Vv_class          BK_SIZE_TYPS.CLASS%TYPE;                        -- Slot type class
 Vn_fix_amt        BK_RQSTS.FIX_AMT%TYPE;                          -- Fix amount
 Vn_bp_id          BK_RQSTS.BP_ID%TYPE;                            -- Booking period
 Vn_act_bp_id      BK_RQSTS.BP_ID%TYPE;                            -- Actual Booking period
 vn_no_max_slot    BK_COND_TYPS.NO_MAX_SLOTS%TYPE;                 -- Numero maximo de slot por condicion
 Vv_message        VARCHAR2(500);                                  -- Message return variable
 Vd_current_date   DATE := Sf_get_sys_date;                        -- Fecha del sistema
 Cv_booked_status  CONSTANT VARCHAR2(5)             := 'BKD';      -- Book status code
 Cv_xpiece_class   CONSTANT BK_SIZE_TYPS.CLASS%TYPE := PKG_BOOK_DATADICT.F_BVTC_XPIECE;
 Cv_reg_class      CONSTANT BK_SIZE_TYPS.CLASS%TYPE := PKG_BOOK_DATADICT.F_BVTC_REG;
 Cv_bestoff_class  CONSTANT BK_SIZE_TYPS.CLASS%TYPE := PKG_BOOK_DATADICT.F_BVTC_BESTOFF;
 e_loa             EXCEPTION;
 e_rest            EXCEPTION;
 e_bestoff         EXCEPTION;
 e_fixamt          EXCEPTION;
 e_small_cond      EXCEPTION;
 e_max_small_cond  EXCEPTION;
 e_invalid_vsl       EXCEPTION;
 e_small_dir       EXCEPTION;
 e_small_dir_rest  EXCEPTION;
 e_invalid_class   EXCEPTION;
 e_no_booked       EXCEPTION;
 e_no_param        EXCEPTION;
 e_bk_notfound     EXCEPTION;
 e_null_slots      EXCEPTION;
 e_error           EXCEPTION;
 --
 -- Booking request cursor data
 CURSOR C_br_data(Pn_br_seq BK_RQSTS.SEQ%TYPE,Pv_xpiece_ind VARCHAR2) IS
 SELECT bpsd.bsps_seq,br.rqst_date,br.rqst_bk_date,bctp.bco_lev,
        br.typ,br.bd_seq,bstp.class,bctp.no_max_slots,
		br.fix_amt,br.bp_id,br.stat
 FROM   bk_rqsts          br,
        bk_per_slot_defs  bpsd,
        bk_days           bd,
        bk_day_cond_assgs bdca,
        bk_size_per_segs  bsps,
        bk_size_typs      bstp,
        bk_cond_typs      bctp
 WHERE  bpsd.bsps_seq = decode(Pv_xpiece_ind,'Y',PKG_BOOK_REFTAB_UTIL.F_GET_BK_BSPS_SEQ(br.bd_seq,br.typ,br.bp_id,Cv_xpiece_class),
                                             'N',PKG_BOOK_REFTAB_UTIL.F_GET_BK_BSPS_SEQ(br.bd_seq,br.typ,br.bp_id,Cv_reg_class))
 AND    bpsd.bsps_seq = bsps.seq
 AND    br.bd_seq     = bpsd.bd_seq
 AND    br.bd_seq     = bd.seq
 AND    bpsd.bd_seq   = bd.seq
 AND    bd.bdca_seq   = bdca.seq
 AND    bdca.bco_lev  = bctp.bco_lev
 AND    bsps.bstp_seq = bstp.seq
 AND    bstp.seq      = bctp.bstp_seq
 AND    Vd_current_date between bctp.date_from and bctp.date_to
 AND    br.seq        = Pn_br_seq;
 --
BEGIN
   --
   -- Se definen las variables locales con los parametros requerido para las validaciones y
   -- procesamiento de la transaccion
   OPEN C_br_data(P_br_seq,P_xpiece_ind);
   --
   FETCH C_br_data INTO vn_bsps_seq
                       ,vd_rqst_date
                       ,vd_rqst_bk_date
                       ,vn_bco_lev
                       ,vv_slot_typ
                       ,vn_bd_seq
                       ,vv_class
                       ,vn_no_max_slot
                       ,vn_fix_amt
                       ,Vn_bp_id
                       ,vv_stat;
   --
   IF C_br_data%FOUND THEN
      --
      pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                   'P_br_seq='
                                 || P_br_seq
                                 || CHR (10)
                                 ||'vn_bsps_seq='
                                 || vn_bsps_seq
                                 || CHR (10)
                                 ||'vd_rqst_date='
                                 || vd_rqst_date
                                 || CHR (10)
                                 ||'vd_rqst_bk_date='
                                 || vd_rqst_bk_date
                                 || CHR (10)
                                 ||'vn_bco_lev='
                                 || vn_bco_lev
                                 || CHR (10)
                                 ||'vv_slot_typ='
                                 || vv_slot_typ
                                 || CHR (10)
                                 ||'vn_bd_seq='
                                 || vn_bd_seq
                                 || CHR (10)
                                 ||'vv_class='
                                 || vv_class
                                 || CHR (10)
								 ||'vn_no_max_slot='
                                 || vn_no_max_slot
                                 || CHR (10)
								 ||'vn_fix_amt='
                                 || vn_fix_amt
                                 || CHR (10)
								 ||'Vn_bp_id='
								 || Vn_bp_id
								 ||'Vv_stat='
								 || Vv_stat,
                                   'INFO');

      --
	  -- Se validan los datos preliminares para poder realizar las validaciones requeridas
      IF vn_bsps_seq IS NULL OR vd_rqst_date   IS NULL OR vd_rqst_bk_date IS NULL OR
         vn_bco_lev  IS NULL OR vv_slot_typ    IS NULL OR vn_bd_seq       IS NULL OR
         vv_class    IS NULL OR vn_no_max_slot IS NULL OR Vn_bp_id        IS NULL OR
		 vv_stat     IS NULL THEN
         --
         RAISE e_no_param;
         --
      ELSE
         --
         IF vv_stat != Cv_booked_status THEN
            --
            RAISE e_no_booked;
            --
		 END IF;
         --
      END IF;

      --
      -- Se valida que el tipo de buque basaddo en la secuencia original y
	  -- nueva de BK_SIZE_TYPS sea SMALL
      IF pkg_book_reftab_util.f_get_bk_size_typ(Vn_bsps_seq)     = pkg_book_datadict.f_bk_typ_small AND
         pkg_book_reftab_util.f_get_bk_size_typ(Vn_org_bsps_seq) = pkg_book_datadict.f_bk_typ_small THEN

         pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                      'Typ=SMALL'
                                    || CHR (10)
                                    ||'New bsps_seq='
                                    || Vn_bsps_seq
                                    ||'Original bsps_seq='
                                    || Vn_org_bsps_seq,
                                      'INFO');

         --
         -- Se valida si la clase original es REGULAR y la nueva XPIECE
         IF pkg_book_reftab_util.f_get_bk_size_class(Vn_org_bsps_seq) = Cv_reg_class    AND
            pkg_book_reftab_util.f_get_bk_size_class(Vn_bsps_seq)     = Cv_xpiece_class AND
            p_xpiece_ind = 'Y' THEN

            pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                         'Class requested=XPIECE : p_loa='||p_loa,
                                         'INFO');

            --
            -- 1. Se valida que el buque tenga una eslora menor o igual a 300 pies
            IF p_loa > pkg_book_datadict.f_bk_loa_xpiece THEN
               raise e_loa;
            END IF;

            pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                         'Class requested=XPIECE : p_rest='||p_rest,
                                         'INFO');

            --
            -- 2. Se valida que la visita no tenga restricciones
            /*
            IF p_rest is not null THEN
               raise e_rest;
            END IF;
            */

            pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                         'Class requested=XPIECE : Best Offer',
                                         'INFO');

            --
            -- 3. Se valida que no se trate de una reservacion basada en la mejor oferta
            IF pkg_book_reftab_util.f_get_bk_size_class(Vn_org_bsps_seq) = Cv_bestoff_class THEN
               raise e_bestoff;
            END IF;

            pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                         'Class requested=XPIECE : vn_fix_amt='||vn_fix_amt,
                                         'INFO');

            --
            -- 4. Se valida que no se trate de una reservacion basada en una subasta
            IF vn_fix_amt > 0 THEN
               raise e_fixamt;
            END IF;

         --
         -- Se valida si la clase es XPIECE
         ELSIF pkg_book_reftab_util.f_get_bk_size_class(Vn_org_bsps_seq) = Cv_xpiece_class AND
               pkg_book_reftab_util.f_get_bk_size_class(Vn_bsps_seq)     = Cv_reg_class    AND
               p_xpiece_ind = 'N' THEN

            --
            -- No se han definido validaciones adiciones cuando se trata de XPIECE hacia REGULAR
            --
            pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                         'Class requested=REGULAR',
                                         'INFO');

         ELSIF pkg_book_reftab_util.f_get_bk_size_class(Vn_org_bsps_seq) != Cv_xpiece_class AND
               pkg_book_reftab_util.f_get_bk_size_class(Vn_org_bsps_seq) != Cv_reg_class    THEN

           --
           -- Este tipo de transaccion solo aplica para buques clase XPIECE o REGULAR tipo SMALL
           --
            pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                         'New Class='||pkg_book_reftab_util.f_get_bk_size_class(Vn_bsps_seq),
                                         'INFO');

           raise e_invalid_class;

         END IF;

         IF pkg_book_reftab_util.f_get_bk_size_class(Vn_org_bsps_seq) = Cv_xpiece_class AND
            pkg_book_reftab_util.f_get_bk_size_class(Vn_bsps_seq)     = Cv_reg_class    THEN

            --
            vn_no_max_slot := PKG_BOOK_REFTAB_UTIL.F_GET_TOT_SLOT_COND(vv_slot_typ,Vn_bco_lev,NULL);
            --

            pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                         'Total slots='||NVL(Sf_Get_Vessel_By_Dir(9,vn_bd_seq,'',vv_slot_typ,Vv_class),0)
                                       || CHR (10)
                                       ||'vn_no_max_slot='||vn_no_max_slot,
                                         'INFO');

            --
            -- Se valida que el numero de buques permitidos
            IF NVL(Sf_Get_Vessel_By_Dir(9,vn_bd_seq,'',vv_slot_typ,Vv_class),0) >= vn_no_max_slot THEN
               raise e_small_cond;
            END IF;

         ELSE

            pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                         'Total slots='||NVL(Sf_Get_Vessel_By_Dir(6,vn_bd_seq,'',vv_slot_typ,Vv_class),0)
                                       || CHR (10)
                                       ||'vn_no_max_slot='||vn_no_max_slot,
                                         'INFO');

            --
            -- Se valida que el numero de buques permitidos
            IF NVL(Sf_Get_Vessel_By_Dir(6,vn_bd_seq,'',vv_slot_typ,Vv_class),0) >= vn_no_max_slot THEN
               raise e_small_cond;
            END IF;

         END IF;

         pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                           'vv_class='
                                         || vv_class,
                                           'INFO');

         --
         -- Se valida la cantidad de reservas por direccion y tipo vs slots totales disponibles por direccion y tipo
         BEGIN
           pkg_booking.p_message_param(Vd_rqst_bk_date   -- Booking date
                                      ,Vd_rqst_date      -- Request date
                                      ,Vn_bco_lev        -- Condition level
                                      ,Vv_slot_typ       -- Typ (small,large)
                                      ,Vn_bd_seq         -- Booking day sequence
                                      ,Null              -- HML (Only for large vessels)
                                      ,P_rest            -- Restriction
                                      ,P_trn_dir         -- Transit direction
                                      ,Vv_class          -- Slot type class
                                      ,Vv_message);      -- Message return variable

           IF Vv_message IS NOT NULL THEN
              --
              pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                           'EXCEPTIONS='
                                         || vv_message,
                                           'INFO');
              --
              IF    vv_message = 'e_max_small_cond' THEN
                    RAISE e_max_small_cond;
              ELSIF vv_message = 'e_small_dir' THEN
                    RAISE e_small_dir;
              ELSIF vv_message = 'e_small_dir_rest' THEN
                    RAISE e_small_dir_rest;
              ELSE
                    RAISE e_error;
              END IF;
              --
           ELSE
              --
              -- Modified by JaDiaz on January 31, 2008 S64634
              -- Reemplazo de variable Vd_rqst_date por Sysdate
              -- IF NOT pkg_booking.f_exist_slot(Vn_bp_id,Vn_bd_seq,Vv_slot_typ,Vd_rqst_date,Vv_class) THEN
              --
              -- Modified by JaDiaz on February 08, 2008 S65143
              -- Reemplazo de variable Vn_bp_id por Vn_act_bp_id
              -- IF NOT pkg_booking.f_exist_slot(Vn_bp_id,Vn_bd_seq,Vv_slot_typ,Sysdate,Vv_class) THEN
              --
              -- Se obtiene el periodo actual basado en su tipo y clase, comparado contra la fecha del sistema.
              Vn_act_bp_id := pkg_booking.f_get_act_bp_id(Vv_slot_typ,Vv_class,Vd_rqst_bk_date,Vn_bp_id);
              --
              IF NOT pkg_booking.f_exist_slot(Vn_act_bp_id,Vn_bd_seq,Vv_slot_typ,Sysdate,Vv_class) THEN
                 raise e_null_slots;
              END IF;

              --
              pkg_plog.sec_implementation ('PKG_BOOKING.P_PROCESS_SMALL_RQSTS_CHGS',
                                           'Vn_bp_id='
                                         || Vn_bp_id
                                         || CHR (10)
                                         ||'Vn_act_bp_id='
                                         || Vn_act_bp_id
                                         || CHR (10)
                                         ||'P_xpiece_ind='
                                         || P_xpiece_ind,
                                           'INFO');
              --
              BEGIN
                --
                UPDATE bk_rqsts
                SET    bsps_seq = vn_bsps_seq
                WHERE  seq      = P_br_seq;
                --
                IF SQL%NOTFOUND THEN
                   RAISE e_bk_notfound;
                END IF;
                --
              END;
              --
           END IF; -- Vv_message IS NOT NULL
           --
         END;
         --
      ELSE -- Not small vessel
         --
         RAISE e_invalid_vsl;
         --
      END IF; -- pkg_book_reftab_util.f_get_bk_size_typ(Vn_bsps_seq) = small
      --
   END IF; -- C_br_data%FOUND
   --
   CLOSE C_br_data;
   --
 EXCEPTION
   WHEN e_loa THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30701','Xpiece transit booking requires a minimum of 300ft. length overall.');
     --
   WHEN e_rest THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30700','Xpiece transit booking is not allowed for restricted vessels');
     --
   WHEN e_bestoff THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30702','Xpiece transit booking cannot be based on auction or best offer.');
     --
   WHEN e_fixamt THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30702','Xpiece transit booking cannot be based on auction or best offer.');
     --
   WHEN e_small_cond THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30707','No. small vessels exceeded for the condition '||Vn_bco_lev);
     --
   WHEN e_max_small_cond THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30706','Maximum restricted small vessels exceeded for the condition '||Vn_bco_lev);
     --
   WHEN e_small_dir THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30705','Number of small vessels exceeded in same direction');
     --
   WHEN e_small_dir_rest THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-01814','No. small vessels w/rest. exceeded in same dir.');
     --
   WHEN e_invalid_class THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30703','Transaction can only be process for regular or xpiece vessels.');
     --
   WHEN e_no_booked THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30711','Transaction is only available for booked reservations.');
     --
   WHEN e_no_param THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30710','Some or all small booking transaction parameters could not be found. Please verify.');
     --
   WHEN e_bk_notfound THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30708','Booking request is not found. Transaction cannot be completed.');
     --
   WHEN e_null_slots THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30714','No slots available.');
     --
   WHEN e_invalid_vsl THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30717','Transaction can only be process for small REG/XPIECE vessels.');
     --
   WHEN e_error THEN
     --
     Pkg_Evtms_Db_Util.p_generic_exception(
	 'USR-30713','Transaction cannot be completed due to an invalid action.');
     --
 END P_PROCESS_SMALL_RQSTS_CHGS;
/* Handle the processes related to booking daylight billing charges */
PROCEDURE P_BDR_BILL_CHRGS
 (P_BDR_SEQ IN BK_DLT_RQSTS.SEQ%TYPE
 ,P_BDR_STAT IN BK_DLT_RQSTS.STAT%TYPE
 ,P_BDR_STAT_DATE IN BK_DLT_RQSTS.STAT_DATE%TYPE
 ,P_BDR_STAT_RMK IN BK_DLT_RQSTS.STAT_RMK%TYPE
 ,P_BS_SEQ IN BILL_SETS.SEQ%TYPE
 ,P_BR_RQST_BK_DATE IN BK_RQSTS.RQST_BK_DATE%TYPE
 ,P_BDR_RQST_DATE IN BK_DLT_RQSTS.RQST_DATE%TYPE
 ,P_BDR_PREV_STAT IN BK_DLT_RQSTS.STAT%TYPE
 ,P_BDR_CANCL_DATE IN BK_DLT_RQSTS.RQST_CANCL_DATE%TYPE
 ,P_SBI_APPR IN VARCHAR2 := NULL
 )
 IS

VV_BDR_CHRG_TYP CONSTANT SVC_BILL_INSTRS.TYP%TYPE := PKG_OMS_DATADICT.f_oms_bdt_typ;
VV_BDR_CHRG_TYP_FEE CONSTANT SVC_BILL_INSTRS.BK_CHRG_TYP%TYPE := PKG_OMS_DATADICT.f_bk_chrg_typ_fee;
VV_BDR_CHRG_TYP_CAN CONSTANT SVC_BILL_INSTRS.BK_CHRG_TYP%TYPE := PKG_OMS_DATADICT.f_bk_chrg_typ_can;

VR_OMS_CANC_CHARGE PKG_CHRG_MANAGEMENT.VR_OMS_CANCEL_RECORD_TYPE;
VN_SBI_SEQ SVC_BILL_INSTRS.SEQ%TYPE;
VN_DAYS NUMBER;
VN_HOURS NUMBER;
VD_BK_REQ_DATE DATE;

--============================================================================
-- DESCRIPCION: Procedimiento para manejar lo concerniente a la generación de los registros
--                             de servicios y cargos de booking en EVTMS y CSMS de acuerdo a cada status
--                             de booking.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha         Cambios realizados
-- -----------  ---------------    -----------------   -----------------------------------------
-- S88293  GVillarreal    31-Dec-2008   Version Inicial.
--
--=============================================================================
BEGIN

IF p_bdr_stat = 'DTCAN' THEN

   vd_bk_req_date := TO_DATE(TO_CHAR(p_br_rqst_bk_date,'DD-MON-YYYY ')||sf_get_param_val(16,'BOOKING'),'DD-MON-YYYY HH24MI');

   IF (
      (TRUNC( vd_bk_req_date) - TRUNC(p_bdr_rqst_date)) >= TO_NUMBER(sf_get_param_val(200,'BOOKING'))  OR
      (TRUNC( vd_bk_req_date) - TRUNC(p_bdr_rqst_date)) <  TO_NUMBER(sf_get_param_val(200,'BOOKING')) AND
       p_bdr_prev_stat = 'DTGUAR'
      )
   THEN

        vn_days  := ABS(vd_bk_req_date - p_bdr_cancl_date);
        vn_hours := TRUNC(ABS(vn_days - trunc(vn_days))*24);

        IF NVL(vn_days,0) < TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN

           --Generate OMS service for CANCELLATION CHARGE
           PKG_BOOKING.p_bdr_bill_proc_sbi(p_bdr_seq,
                                           p_bdr_stat_date,
                                           p_bs_seq,
                                           'N',
                                           vv_bdr_chrg_typ,
                                           vv_bdr_chrg_typ_can,
                                           p_sbi_appr);

           --Approval process continues generation on SBI_A_I_S_OSAS trigger.

        END IF;--IF NVL(vn_days,0) < TO_NUMBER(sf_get_param_val(200,'BOOKING')) THEN

   END IF;--IF (

ELSIF p_bdr_stat = 'VDTCAN RQS' THEN

   --If OMS Service exists, Disapprove the
   --Previously Approved Booking Daylight Cancellation charge
   --until VOID CANCEL REQUEST  will be approved or disapprove finally
   PKG_BOOKING.p_br_oms_canc_chrg(p_bdr_seq,
                                  p_bdr_stat_rmk,
                                  vv_bdr_chrg_typ,
                                  vv_bdr_chrg_typ_can,
                                  NULL);

ELSIF p_bdr_stat = 'VDTCAN' THEN

   --If OMS Service exists, VOID the
   --Previously Disapproved Booking Daylight Cancellation charge
   PKG_BOOKING.p_br_oms_canc_chrg(p_bdr_seq,
                                  p_bdr_stat_rmk,
                                  vv_bdr_chrg_typ,
                                  vv_bdr_chrg_typ_can,
                                  TRUE);

ELSIF p_bdr_stat = 'NDTBGRA RQ' THEN

   --If OMS Service exists, Disapprove the
   --Previously Approved Booking Daylight charge
   --until NOT GRANT REQUEST will be approved or disapprove finally
   PKG_BOOKING.p_br_oms_canc_chrg(p_bdr_seq,
                                  p_bdr_stat_rmk,
                                  vv_bdr_chrg_typ,
                                  vv_bdr_chrg_typ_fee,
                                  NULL);

ELSIF p_bdr_stat = 'NOT DTBGRA' THEN

   --VOID the previously Disapproved Booking Daylight charge
   PKG_BOOKING.p_br_oms_canc_chrg(p_bdr_seq,
                                  p_bdr_stat_rmk,
                                  vv_bdr_chrg_typ,
                                  vv_bdr_chrg_typ_fee,
                                  TRUE);

ELSIF p_bdr_stat IN ('DTBGRA','DTFOR') THEN

   --Generate OMS service charge for Booking Daylight Granted
   PKG_BOOKING.p_bdr_bill_proc_sbi(p_bdr_seq,
                                   p_bdr_stat_date,
                                   p_bs_seq,
                                   'N',
                                   vv_bdr_chrg_typ,
                                   vv_bdr_chrg_typ_fee,
                                   p_sbi_appr);

END IF;--IF p_bdr_stat = 'DTCAN' THEN

EXCEPTION
WHEN OTHERS THEN

   -- Recording event in log table
   Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BDR_BILL_CHRGS',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'P_BDR_SEQ= '||TO_CHAR(P_BDR_SEQ)||' '||
                               'P_BDR_STAT= '||P_BDR_STAT||' '||
                                SUBSTR(SQLERRM,1,1500),
                               'ERROR');

   Pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);

END;
/* Process the approval of OMS charges for Booking Daylight Requests */
PROCEDURE P_BDR_APPR_OMS_CHRG
 (P_SBI_SEQ IN SVC_BILL_INSTRS.SEQ%TYPE
 ,P_BS_SEQ IN BILL_SETS.SEQ%TYPE
 ,P_CHRG_TYP IN SVC_BILL_INSTRS.TYP%TYPE
 ,P_CHARGE_REC IN PKG_MB621000PR.RATE_RECORD_TYPE
 )
 IS

VR_OMS_APPRV_CHARGE PKG_CHRG_MANAGEMENT.VR_OMS_CHRG_PARAM_RECTYP;

--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de procesar el PL/SQL table de cargos de booking para
--                realizar la aprobación de cargos de booking requests.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker    Desarrollador       Fecha                         Cambios realizados
-- -----------    --------------------       ------------------            -----------------------------------------
-- R88293    GVillarreal              08-JAN-2009            Version Inicial
BEGIN

     vr_oms_apprv_charge := NULL;

     --Assign the charge data to the record for OMS approval
     vr_oms_apprv_charge.amount       := p_charge_rec.amount;
     vr_oms_apprv_charge.eff_date     := p_charge_rec.eff_date;
     vr_oms_apprv_charge.descr        := p_charge_rec.description;
     vr_oms_apprv_charge.qty          := p_charge_rec.qty;
     vr_oms_apprv_charge.unit         := p_charge_rec.unit;
     vr_oms_apprv_charge.rate         := p_charge_rec.rate;
     vr_oms_apprv_charge.item_no      := p_charge_rec.item_no;
     vr_oms_apprv_charge.sbi_seq      := p_sbi_seq;
     vr_oms_apprv_charge.charge_typ   := p_chrg_typ;
     vr_oms_apprv_charge.bs_seq       := p_bs_seq;
     vr_oms_apprv_charge.Send_trx_ind := True;

     --
     -- Added by JaDiaz on November 17, 2011 S158688_T33
     vr_oms_apprv_charge.acc_grp      := Sf_get_acc_grp(vr_oms_apprv_charge.item_no,'STD',NULL,NULL,NULL,NULL,NULL,NULL,vr_oms_apprv_charge.eff_date);

     dbms_output.put_line('P_BDR_APPR_OMS-vr_oms_apprv_charge.sbi_seq='||to_char(vr_oms_apprv_charge.sbi_seq));
     dbms_output.put_line('P_BDR_APPR_OMS-vr_oms_apprv_charge.charge_typ='||vr_oms_apprv_charge.sbi_seq);
     dbms_output.put_line('P_BDR_APPR_OMS-vr_oms_apprv_charge.bs_seq='||to_char(vr_oms_apprv_charge.bs_seq));

     --Approve OMS Booking Daylight Charge
     PKG_CHRG_MANAGEMENT.P_OMS_CHARGE_APPRV(vr_oms_apprv_charge);

EXCEPTION
WHEN OTHERS THEN

  Pkg_evtms_db_util.P_generic_exception (Null, SUBSTR(SQLERRM,1,1000));

  Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BDR_APPR_OMS_CHRG',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'SBI_SEQ='||TO_CHAR(p_sbi_seq)||' '||
                                SUBSTR(SQLERRM,1,1500),
                               'ERROR');

END;
/* Handle the processes related to booking billing charges */
PROCEDURE P_BR_BILL_CHRGS
 (P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_BR_STAT IN BK_RQSTS.STAT%TYPE
 ,P_BR_STAT_DATE IN BK_RQSTS.STAT_DATE%TYPE
 ,P_BR_STAT_RMK IN BK_RQSTS.STAT_RMK%TYPE
 ,P_BS_SEQ IN BILL_SETS.SEQ%TYPE
 ,P_BR_PENALTY_IND IN BK_RQSTS.PENALTY_IND%TYPE
 ,P_BR_RQST_DATE IN BK_RQSTS.RQST_DATE%TYPE
 ,P_BR_BSPS_SEQ IN BK_RQSTS.BSPS_SEQ%TYPE
 ,P_SBI_APPR IN VARCHAR2 := NULL
 )
 IS

VV_TOLL_CHRG CONSTANT CHRG_GRP_NOS.TYP%TYPE := PKG_CHRG_MANAGEMENT.F_TOLL_CHRG;
VV_OMS_CHRG CONSTANT CHRG_GRP_NOS.TYP%TYPE := PKG_CHRG_MANAGEMENT.F_OMS_CHRG;
VV_BR_CHRG_TYP_FEE CONSTANT SVC_BILL_INSTRS.BK_CHRG_TYP%TYPE := PKG_OMS_DATADICT.f_bk_chrg_typ_fee;
VV_BR_CHRG_TYP_CAN CONSTANT SVC_BILL_INSTRS.BK_CHRG_TYP%TYPE := PKG_OMS_DATADICT.f_bk_chrg_typ_can;
VV_BR_CHRG_TYP_PEN CONSTANT SVC_BILL_INSTRS.BK_CHRG_TYP%TYPE := PKG_OMS_DATADICT.f_bk_chrg_typ_pen;
VV_BR_CHRG_TYP CONSTANT SVC_BILL_INSTRS.TYP%TYPE := PKG_OMS_DATADICT.f_oms_br_typ;
VV_BR_TOLL_CHRG_TYP CONSTANT INTERFACED_CHRGS.CHRG_TYP%TYPE := 'BOOKING';
VV_IC_APPRV CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_APPROVED;
VV_IC_DISAPPRV CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_DISAPPROVED;
VV_IC_VOID CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_VOID;
VV_IC_INFCE CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_INTERFACED;

VN_TCD_SEQ TOLL_CHRG_DETS.SEQ%TYPE;
VN_SBI_SEQ SVC_BILL_INSTRS.SEQ%TYPE;
VV_TCD_STAT TOLL_WORK_FLOW_RULES.POSBLE_STEP_RESULT%TYPE;
VD_TRANSIT_DATE DATE;
VV_BR_CHRG_STAT INTERFACED_CHRGS.STATUS%TYPE;

-- ==============================================================================================
-- DESCRIPCION: Procedimiento para manejar la generación de registros de cargos de acuerdo
--              a los status de booking.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
-- ==============================================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker      Desarrollador      Fecha               Cambios realizados
-- -----------  ---------------    -----------------   -----------------------------------------
-- S88293       GVillarreal        31-Dec-2008         Version Inicial.
-- S140880_P5   JADiaz             18-Mar-2011         Adecuacion para manejo de cargo de BOOKING
--                                                     por OMS
--
-- ==============================================================================================
BEGIN
PKG_PLOG.SEC_IMPLEMENTATION('PKG_BOOKING','p_br_stat '||p_br_stat,'DEBUG');
IF p_br_stat = 'CAN' THEN

   --
   -- Note: According to CSMS Implementation, Cancellation Charge for Best Offer Bookings
   --       will be treated as normal booking cancellation charges.

   --
   -- Generate OMS service for CANCELLATION CHARGE
   PKG_BOOKING.p_br_bill_proc_sbi(p_br_seq,
                                  p_br_stat_date,
                                  p_bs_seq,
                                  'N',
                                  vv_br_chrg_typ,
                                  vv_br_chrg_typ_can,
                                  p_sbi_appr);

   --
   -- Approval process continues generation on SBI_A_I_S_OSAS trigger.

ELSIF p_br_stat = 'VCAN RQST' THEN

   --
   -- If OMS Service and Charge exists, Disapprove the
   -- Previously Approved Booking Cancellation charge
   -- until VOID CANCEL REQUEST will be approved or disapprove finally
   PKG_BOOKING.p_br_oms_canc_chrg(p_br_seq,
                                  p_br_stat_rmk,
                                  vv_br_chrg_typ,
                                  vv_br_chrg_typ_can,
                                  NULL);

ELSIF p_br_stat = 'VCAN' THEN

   --
   -- If OMS Service and Charge exists, VOID the
   -- Previously Disapproved Booking Cancellation charge
   PKG_BOOKING.p_br_oms_canc_chrg(p_br_seq,
                                  p_br_stat_rmk,
                                  vv_br_chrg_typ,
                                  vv_br_chrg_typ_can,
                                  TRUE);

ELSIF p_br_stat = 'NGRA RQST' THEN

   vn_tcd_seq  := SF_GET_TCD_SEQ(p_bs_seq);
   vv_tcd_stat := SF_GET_TCD_STATUS(vn_tcd_Seq,1);

   --
   -- Added by JaDiaz on february 08, 2010 S140880_P2
   IF NVL(vv_tcd_stat,' ') in (vv_ic_apprv,vv_ic_infce) THEN

      --
      -- If applies booking charge for TOLL
      -- Disapprove the booking charge until NOT GRANT REQUEST will be approved or disapprove finally
      PKG_BOOKING.p_br_toll_rev_chrg(vn_tcd_seq,
                                     p_br_seq,
                                     p_bs_seq,
                                     p_br_stat_rmk,
                                     vv_br_toll_chrg_typ,
                                     vv_ic_disapprv);

      --
      -- If applies booking charge for OMS
      -- Disapprove the booking charge until NOT GRANT REQUEST will be approved or disapprove finally
      PKG_BOOKING.p_br_oms_canc_chrg(p_br_seq,
                                     p_br_stat_rmk,
                                     vv_br_chrg_typ,
                                     vv_br_chrg_typ_fee,
                                     NULL);

   END IF;

ELSIF p_br_stat = 'NOT GRA' THEN

   vn_tcd_seq  := SF_GET_TCD_SEQ(p_bs_seq);
   vv_tcd_stat := SF_GET_TCD_STATUS(vn_tcd_Seq,1);

   --
   -- Added by JaDiaz on february 08, 2010 S140880_P2
   IF NVL(vv_tcd_stat,' ') in (vv_ic_apprv,vv_ic_infce) THEN

      --If applies charge on Booking void the booking charge because NOT GRANTED status,
      PKG_BOOKING.p_br_toll_rev_chrg(vn_tcd_seq,
                                     p_br_seq,
                                     p_bs_seq,
                                     p_br_stat_rmk,
                                     vv_br_toll_chrg_typ,
                                     vv_ic_void);

      --If OMS Service exists,VOID Booking Charge because NOT GRANTED status
      PKG_BOOKING.p_br_oms_canc_chrg(p_br_seq,
                                     p_br_stat_rmk,
                                     vv_br_chrg_typ,
                                     vv_br_chrg_typ_fee,
                                     TRUE);

   END IF;

   --Generate the OMS Service for Penalty Charge
   IF p_br_penalty_ind ='Y' THEN

      --
      -- Generate OMS service for Penalty CHARGE
      -- Only if  Charge was not previously created
      PKG_BOOKING.p_br_bill_proc_sbi(p_br_seq,
                                     p_br_stat_date,
                                     p_bs_seq,
                                     'N',
                                     vv_br_chrg_typ,
                                     vv_br_chrg_typ_pen,
                                     p_sbi_appr);

      --
      -- Approval process continues generation on SBI_A_I_S_OSAS trigger.

   END IF;

ELSIF (p_br_stat = 'GRA') OR (p_br_stat = 'FOR') THEN

   vn_tcd_seq      := SF_GET_TCD_SEQ(p_bs_seq);
       --28AUG15 CMartin SR107552 Adecuaciones por canal ampliado
        --28AUG15         SR132271 Adecuaciones por entrada de nuevo aplicativo Booking
        -- SF_GET_TRANSIT_DATE  debe reemplazarse por SF_GET_ACTUAL_TRANSIT_DATE
   vd_transit_date := SF_GET_ACTUAL_TRANSIT_DATE(p_bs_seq);
   vv_tcd_stat     := SF_GET_TCD_STATUS(vn_tcd_Seq,1);
   vv_br_chrg_stat := PKG_CHRG_MANAGEMENT.F_GET_SVC_REF_STATUS(vn_tcd_seq,vv_toll_chrg,vv_br_toll_chrg_typ,2);
PKG_PLOG.SEC_IMPLEMENTATION('PKG_BOOKING','en el GRA '||vv_br_chrg_stat,'DEBUG');
   --
   -- Primero se evalua el status del cargo de BOOKING
   -- dentro de TOLLS
   IF NVL(vv_br_chrg_stat,' ') = vv_ic_disapprv THEN

      --
      -- If exists a Booking Charge Disapproved on a Toll Charge, re-approve the Charge
      -- Because the NOT GRANTED REQUEST was Disapproved
      PKG_BOOKING.p_br_toll_rev_chrg(vn_tcd_seq,
                                     p_br_seq,
                                     p_bs_seq,
                                     p_br_stat_rmk,
                                     vv_br_toll_chrg_typ,
                                     vv_ic_apprv);

   --
   -- Added by JaDiaz on March 18, 2011 S140880_P5
   -- Segundo se evalua el status del cargo de BOOKING
   -- dentro de OMS si aplica
   ELSE

      --
      Vn_sbi_seq      := SF_GET_BR_SBI_SEQ(vv_br_chrg_typ,vv_br_chrg_typ_fee,p_br_seq);
      Vv_br_chrg_stat := PKG_CHRG_MANAGEMENT.F_GET_SVC_REF_STATUS(Vn_sbi_seq,vv_oms_chrg,vv_br_chrg_typ,2);
PKG_PLOG.SEC_IMPLEMENTATION('PKG_BOOKING','en el OMS '||Vv_br_chrg_stat,'DEBUG');
      --
      -- Se evalua si el cargo se encuentra desaprobado
      IF NVL(vv_br_chrg_stat,' ') = vv_ic_disapprv THEN
     PKG_PLOG.SEC_IMPLEMENTATION('PKG_BOOKING','voy a p_br_oms_rev_chrg','DEBUG');
         --
         -- If exists a Booking Charge Disapproved on a Oms Charge, re-approve the Charge
         -- Because the NOT GRANTED REQUEST was Disapproved
         PKG_BOOKING.p_br_oms_rev_chrg(Vn_sbi_seq,
                                       p_bs_seq,
                                       p_br_stat_rmk,
                                       vv_br_chrg_typ,
                                       vv_ic_apprv);

      ELSE

         --
         -- Modified by JaDiaz on february 08, 2010 S140880_P2
         -- Se incluyo facturas en estado INTERFACED
         IF (vv_tcd_stat in (vv_ic_apprv,vv_ic_infce)) OR (vd_transit_date IS NULL AND p_br_stat ='FOR') THEN

            --
            -- Generate OMS service for Booking Fee or Forfeited Fee
            PKG_BOOKING.p_br_bill_proc_sbi(p_br_seq,
                                           p_br_stat_date,
                                           p_bs_seq,
                                           'N',
                                           vv_br_chrg_typ,
                                           vv_br_chrg_typ_fee,
                                           p_sbi_appr);

            --
            -- Approval process continues generation on SBI_A_I_S_OSAS trigger.

         END IF;

      END IF;

   END IF;

   IF p_br_penalty_ind ='Y' THEN

      --
      -- Generate OMS service for Penalty CHARGE
      -- Only if  Charge was not previously created
      PKG_BOOKING.p_br_bill_proc_sbi(p_br_seq,
                                     p_br_stat_date,
                                     p_bs_seq,
                                     'N',
                                     vv_br_chrg_typ,
                                     vv_br_chrg_typ_pen,
                                     p_sbi_appr);

      --
      -- Approval process continues generation on SBI_A_I_S_OSAS trigger.

   END IF;

END IF;

EXCEPTION
WHEN OTHERS THEN

   -- Recording event in log table
   Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BR_BILL_CHRGS',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'P_BR_SEQ= '||TO_CHAR(P_BR_SEQ)||' '||
                               'P_BR_STAT= '||P_BR_STAT||' '||
                                SUBSTR(SQLERRM,1,1500),
                               'ERROR');

   Pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);

END;
/* Process the approval of OMS charges for Booking Requests */
PROCEDURE P_BR_APPR_OMS_CHRG
 (P_SBI_SEQ IN SVC_BILL_INSTRS.SEQ%TYPE
 ,P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_BS_SEQ IN BILL_SETS.SEQ%TYPE
 ,P_CHRG_TYP IN SVC_BILL_INSTRS.TYP%TYPE
 ,P_CHARGE_TAB IN PKG_MB620000PR.CHARGE_TABLE_TYPE
 )
 IS

VR_OMS_APPRV_CHARGE PKG_CHRG_MANAGEMENT.VR_OMS_CHRG_PARAM_RECTYP;
VV_BR_STAT BK_RQSTS.STAT%TYPE;
VV_TAR_GRP TARIFF_ACC_GRP_ASSGS.TAR_GRP%TYPE;
VV_ACG_CD ACC_GRPS.CD%TYPE;
VN_BSPS_SEQ BK_RQSTS.BSPS_SEQ%TYPE;
VD_TRANSIT_DATE DATE;
VD_STAT_DATE DATE;

--============================================================================
-- DESCRIPCION:
-- 	Este procedimiento se encarga de procesar el PL/SQL table de cargos de booking para
--                realizar la aprobación de cargos de booking requests.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker    Desarrollador       Fecha                         Cambios realizados
-- -----------    --------------------       ------------------            -----------------------------------------
-- R88293    GVillarreal              08-JAN-2009            Version Inicial
BEGIN

  vr_oms_apprv_charge := NULL;

  dbms_output.put_line('registros charge tab='||to_char(P_CHARGE_TAB.count));

  FOR i IN 1..P_CHARGE_TAB.count
  LOOP

     --Assign the charge data to the record for OMS approval
     vr_oms_apprv_charge.amount       := p_charge_tab(i).amount;
     vr_oms_apprv_charge.eff_date     := p_charge_tab(i).eff_date;
     vr_oms_apprv_charge.descr        := p_charge_tab(i).description;
     vr_oms_apprv_charge.qty          := p_charge_tab(i).qty;
     vr_oms_apprv_charge.unit         := p_charge_tab(i).unit;
     vr_oms_apprv_charge.rate         := p_charge_tab(i).rate;
     vr_oms_apprv_charge.item_no      := p_charge_tab(i).item_no;
     vr_oms_apprv_charge.acc_grp      := p_charge_tab(i).acg_cd;
     vr_oms_apprv_charge.sbi_seq      := p_sbi_seq;
     vr_oms_apprv_charge.charge_typ   := p_chrg_typ;
     vr_oms_apprv_charge.br_seq       := p_br_seq;
     vr_oms_apprv_charge.bs_seq       := p_bs_seq;
     vr_oms_apprv_charge.Send_trx_ind := True;

     dbms_output.put_line('amount registro approv='||to_char(vr_oms_apprv_charge.amount));
     dbms_output.put_line('sbi_seq registro approv='||to_char(vr_oms_apprv_charge.sbi_seq));
     dbms_output.put_line('item_no registro approv='||vr_oms_apprv_charge.item_no);
     dbms_output.put_line('chrg typ registro approv='||vr_oms_apprv_charge.charge_typ);

     dbms_output.put_line('ACG_CD registro approv='|| vr_oms_apprv_charge.acc_grp);
     dbms_output.put_line('EFF DATE registro approv='||to_char(vr_oms_apprv_charge.eff_date,'DD-MON-YYYY HH24MI'));
     dbms_output.put_line('QTY registro approv='||TO_CHAR(vr_oms_apprv_charge.qty));
     dbms_output.put_line('UNIT registro approv='||vr_oms_apprv_charge.unit);
     dbms_output.put_line('rate registro approv='||to_char(vr_oms_apprv_charge.rate));
     dbms_output.put_line('bs_seq registro approv='||to_char(vr_oms_apprv_charge.bs_seq));

     --Approve OMS Charge
     --Every line of the Charges Tab will be approved as independent OMS Charge records
     PKG_CHRG_MANAGEMENT.P_OMS_CHARGE_APPRV(vr_oms_apprv_charge);

  END LOOP;--FOR i IN 1..P_CHARGE_TAB.count

EXCEPTION
WHEN OTHERS THEN

  Pkg_evtms_db_util.P_generic_exception (Null, SUBSTR(SQLERRM,1,1000));

  Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BR_APPR_OMS_CHRG',
                              '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                              'SBI_SEQ='||TO_CHAR(p_sbi_seq)||' '||
                              'BR_SEQ='||TO_CHAR(p_br_seq)||' '||
                              SUBSTR(SQLERRM,1,1400),
                              'ERROR');

END;
/* Creation of OMS Service according to booking daylight information */
PROCEDURE P_BDR_BILL_PROC_SBI
 (P_BDR_SEQ IN BK_DLT_RQSTS.SEQ%TYPE
 ,P_BDR_STAT_DATE IN BK_DLT_RQSTS.STAT_DATE%TYPE
 ,P_BS_SEQ IN BILL_SETS.SEQ%TYPE
 ,P_WAIVE_IND IN SVC_BILL_INSTRS.WAIVE_IND%TYPE
 ,P_CHRG_TYP IN SVC_BILL_INSTRS.TYP%TYPE
 ,P_BK_CHRG_TYP IN SVC_BILL_INSTRS.BK_CHRG_TYP%TYPE
 ,P_SBI_APPR IN VARCHAR2 := NULL
 )
 IS

VR_BOOK_SBI_REC PKG_CHRG_MANAGEMENT.VR_SBI_CHRG_RECORD_TYPE;
VN_SBI_SEQ SVC_BILL_INSTRS.SEQ%TYPE;
VV_SBI_STAT OMS_WORK_STEP_RULES.POSBLE_STEP_RESULT%TYPE;
VN_BR_SEQ BK_RQSTS.SEQ%TYPE;
VN_BS_SEQ BILL_SETS.SEQ%TYPE;
VD_EFF_DATE DATE;
VD_BR_RQST_BK_DATE BK_RQSTS.RQST_BK_DATE%TYPE;
VD_BDR_RQST_DATE BK_DLT_RQSTS.RQST_DATE%TYPE;
VD_BDR_RQST_CANCL_DATE BK_DLT_RQSTS.RQST_CANCL_DATE%TYPE;
VV_BDR_STAT BK_DLT_RQSTS.STAT%TYPE;
VV_BDR_PREV_STAT BK_DLT_RQSTS.STAT%TYPE;

--============================================================================
-- DESCRIPCION: Procedimiento para manejar la generación de registros de OMS de acuerdo
--                             a la información de booking daylight charge type.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha         Cambios realizados
-- -----------  ---------------    -----------------   -----------------------------------------
-- S88293  GVillarreal    31-Dec-2008   Version Inicial.
--
--=============================================================================
BEGIN

--Initialize record variables for booking
vr_book_sbi_rec       := NULL;

--Get OMS service information
vn_sbi_seq := SF_GET_BR_SBI_SEQ(p_chrg_typ,p_bk_chrg_typ,NULL,p_bdr_seq);

vv_sbi_stat :=  SF_GET_SBI_STATUS(vn_sbi_seq,1);

--If OMS Service not exists, create the OMS Service OR
--Status of a Previous SBI record is VOIDED
IF (vn_sbi_seq IS NULL )    OR
   (vn_sbi_seq IS NOT NULL AND vv_sbi_stat = 'VOIDED')
THEN


   --Set values on SBI Record Variable for booking
   vr_book_sbi_rec.stat_date   := p_bdr_stat_date;
   vr_book_sbi_rec.bdr_seq     := p_bdr_seq;
   vr_book_sbi_rec.waive_ind   := p_waive_ind;
   vr_book_sbi_rec.bk_chrg_typ := p_bk_chrg_typ;
   vr_book_sbi_rec.bs_seq      := p_bs_seq;

   --Generate OMS service
   PKG_CHRG_MANAGEMENT.p_create_svc_chrg_instr(vr_book_sbi_rec);

ELSIF (vn_sbi_seq IS NOT NULL AND vv_sbi_stat <> 'VOIDED' AND NVL(p_sbi_appr,' ') = 'Y') THEN


   --Get Booking Daylight Information
   BEGIN

      SELECT br.seq,
             bdr.stat,
             br.rqst_bk_date,
             bdr.rqst_date,
             bdr.rqst_cancl_date,
             pkg_mb621000pr.f_get_can_prev_stat(bdr.seq, bdr.stat),
             NVL(bdr.rqst_cancl_date,bdr.stat_date),
             br.bs_seq
        INTO vn_br_seq,
             vv_bdr_stat,
             vd_br_rqst_bk_date,
             vd_bdr_rqst_date,
             vd_bdr_rqst_cancl_date,
             vv_bdr_prev_stat,
             vd_eff_date,
             vn_bs_seq
        FROM BK_RQSTS br,
             BK_DLT_RQSTS bdr
       WHERE bdr.seq = p_bdr_seq
         AND br.seq  = bdr.br_seq;

   EXCEPTION
   WHEN OTHERS THEN

     vn_br_seq              := NULL;
     vv_bdr_stat            := NULL;
     vd_br_rqst_bk_date     := NULL;
     vd_bdr_rqst_date       := NULL;
     vd_bdr_rqst_cancl_date := NULL;
     vv_bdr_prev_stat       := NULL;
     vd_eff_date            := NULL;
     vn_bs_seq              := NULL;

   END;

   --Process charges calculation and re-approval
   PKG_BOOKING.p_bdr_bill_appr_sbi(vn_sbi_seq,
                                   vn_br_seq,
                                   vn_bs_seq,
                                   p_chrg_typ,
                                   p_bk_chrg_typ,
                                   vd_eff_date,
                                   vd_br_rqst_bk_date,
                                   vd_bdr_rqst_date,
                                   vd_bdr_rqst_cancl_date,
                                   vv_bdr_stat,
                                   vv_bdr_prev_stat,
                                   p_sbi_appr);

END IF;--IF vn_sbi_seq IS NULL THEN

EXCEPTION
WHEN OTHERS THEN

   -- Recording event in log table
   Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BDR_BILL_PROC_SBI',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'P_BR_SEQ= '||TO_CHAR(P_BDR_SEQ)||' '||
                               'P_CHRG_TYP= '||P_CHRG_TYP||' '||
                               'P_BK_CHRG_TYP= '||P_BK_CHRG_TYP||' '||
                                SUBSTR(SQLERRM,1,1400),
                               'ERROR');

END;
/* Creation of OMS Service according to booking information */
PROCEDURE P_BR_BILL_PROC_SBI
 (P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_BR_STAT_DATE IN BK_RQSTS.STAT_DATE%TYPE
 ,P_BS_SEQ IN BILL_SETS.SEQ%TYPE
 ,P_WAIVE_IND IN SVC_BILL_INSTRS.WAIVE_IND%TYPE
 ,P_CHRG_TYP IN SVC_BILL_INSTRS.TYP%TYPE
 ,P_BK_CHRG_TYP IN SVC_BILL_INSTRS.BK_CHRG_TYP%TYPE
 ,P_SBI_APPR IN VARCHAR2 := NULL
 )
 IS

VR_BOOK_SBI_REC PKG_CHRG_MANAGEMENT.VR_SBI_CHRG_RECORD_TYPE;
VN_SBI_SEQ SVC_BILL_INSTRS.SEQ%TYPE;
VV_SBI_STAT OMS_WORK_STEP_RULES.POSBLE_STEP_RESULT%TYPE;

-- ==============================================================================================
-- DESCRIPCION: Procedimiento para manejar la generación de registros de OMS de acuerdo
--              a la información de booking charge type.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
-- ==============================================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker      Desarrollador      Fecha               Cambios realizados
-- -----------  ---------------    -----------------   -----------------------------------------
-- S88293       GVillarreal        31-Dec-2008         Version Inicial.
-- S140880_P5   JADiaz             18-Mar-2011         Adecuacion por desuso del parametro
--                                                     P_SBI_APPR
--
-- ==============================================================================================
BEGIN

  --
  -- Initialize record variables for booking
  vr_book_sbi_rec := NULL;

  --
  -- Get OMS service information
  vn_sbi_seq  := SF_GET_BR_SBI_SEQ(p_chrg_typ,p_bk_chrg_typ,p_br_seq);
  vv_sbi_stat := SF_GET_SBI_STATUS(vn_sbi_seq,1);

  -- Create the OMS Service IF
  --   OMS Service not exists, OR
  --   Status of a Previous SBI record for the same type is VOIDED  OR
  --   Status of a Previous SBI record for the same type is different from VOIDED AND
  --   OMS Service need to be created as a Re-Approval of Booking Charge. <--Option avoided
  --   (The Same OMS Service Disapproved will be re-approved)

  IF (vn_sbi_seq IS NULL )    OR
     (vn_sbi_seq IS NOT NULL AND vv_sbi_stat = 'VOIDED')
  THEN

     --
     -- Set values on SBI Record Variable for booking
     vr_book_sbi_rec.stat_date   := p_br_stat_date;
     vr_book_sbi_rec.br_seq      := p_br_seq;
     vr_book_sbi_rec.waive_ind   := p_waive_ind;
     vr_book_sbi_rec.bk_chrg_typ := p_bk_chrg_typ;
     vr_book_sbi_rec.bs_seq      := p_bs_seq;

     /*Desactivated due to another option, because the same OMS Service Record with status
       REVISED will be re-approved with status CHRG APP
     --Activate Global Variable to indicate the approval of Penalty Charge
     --(due to a REVISED DISAPPROVE Transaction of OMS Serivce on Service Worklist Module)
     --to the trigger SBI_A_I_OSAS when the new OMS record for Charge Approval were created
     IF vn_sbi_seq IS NOT NULL AND vv_sbi_stat <> 'VOIDED' AND NVL(p_sbi_appr,' ') = 'Y') THEN

        pkg_global_vars.P_SET_CHAR5('BR_APPR_SBI_PEN');

     END IF;--IF vn_sbi_seq IS NOT NULL AND vv_sbi_stat <> 'VOIDED' AND NVL(p_sbi_appr,' ') = 'Y') THEN
     */

     --
     -- Generate OMS service
     PKG_CHRG_MANAGEMENT.p_create_svc_chrg_instr(vr_book_sbi_rec);

     /*Desactivated due to another option, because the same OMS Service Record with status
       REVISED will be re-approved with status CHRG APP
     --Desactivate Global Variable to indicate the approval of Penalty Charge
     IF vn_sbi_seq IS NOT NULL AND vv_sbi_stat <> 'VOIDED' AND NVL(p_sbi_appr,' ') = 'Y') THEN

        pkg_global_vars.P_SET_CHAR5(NULL);

     END IF;--IF vn_sbi_seq IS NOT NULL AND vv_sbi_stat <> 'VOIDED' AND NVL(p_sbi_appr,' ') = 'Y') THEN
     */

  --
  -- Modified by JaDiaz on March 17, 2011 S140880_P5
  -- ELSIF (vn_sbi_seq IS NOT NULL AND vv_sbi_stat <> 'VOIDED' AND NVL(p_sbi_appr,' ') = 'Y') THEN
  ELSIF (vn_sbi_seq IS NOT NULL AND vv_sbi_stat != 'VOIDED') THEN

     --
     -- Process charges calculation and approval
     PKG_BOOKING.p_br_bill_appr_sbi(vn_sbi_seq,
                                    p_br_seq,
                                    p_bs_seq,
                                    p_chrg_typ,
                                    p_bk_chrg_typ,
                                    p_sbi_appr);

  END IF;

EXCEPTION
WHEN OTHERS THEN

   -- Recording event in log table
   Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BR_BILL_PROC_SBI',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'P_BR_SEQ= '||TO_CHAR(P_BR_SEQ)||' '||
                               'P_CHRG_TYP= '||P_CHRG_TYP||' '||
                               'P_BK_CHRG_TYP= '||P_BK_CHRG_TYP||' '||
                                SUBSTR(SQLERRM,1,1400),
                               'ERROR');

END;
/* Handle the processes related to OMS cancellation charges */
PROCEDURE P_BR_OMS_CANC_CHRG
 (P_BR_SEQ IN NUMBER
 ,P_BR_STAT_RMK IN VARCHAR2
 ,P_CHRG_TYP IN SVC_BILL_INSTRS.TYP%TYPE
 ,P_BK_CHRG_TYP IN SVC_BILL_INSTRS.BK_CHRG_TYP%TYPE
 ,P_VOID IN BOOLEAN
 )
 IS

VV_IC_DISAPPRV CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_DISAPPROVED;
VV_IC_APPRV CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_APPROVED;
VV_OMS_CHRG CONSTANT CHRG_GRP_NOS.TYP%TYPE := PKG_CHRG_MANAGEMENT.F_OMS_CHRG;
VV_BR_CHRG_TYP CONSTANT SVC_BILL_INSTRS.TYP%TYPE := PKG_OMS_DATADICT.f_oms_br_typ;
VV_BDT_CHRG_TYP CONSTANT SVC_BILL_INSTRS.TYP%TYPE := PKG_OMS_DATADICT.f_oms_bdt_typ;

VV_BR_CHRG_STAT INTERFACED_CHRGS.STATUS%TYPE;
VR_OMS_CANC_CHARGE PKG_CHRG_MANAGEMENT.VR_OMS_CANCEL_RECORD_TYPE;
VN_SBI_SEQ SVC_BILL_INSTRS.SEQ%TYPE;

--============================================================================
-- DESCRIPCION: Procedimiento para manejar la desaprobación o anulación de cargos de OMS
--                             relacionados a booking
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha         Cambios realizados
-- -----------  ---------------    -----------------   -----------------------------------------
-- S88293  GVillarreal    31-Dec-2008   Version Inicial.
--
--=============================================================================
BEGIN

--Initialize record variables for booking
vr_oms_canc_charge := NULL;

--Get OMS service information, depending if it is a BOOKING REQUEST
--OR Booking Daylight
IF p_chrg_typ = 'BR' THEN

   vn_sbi_seq := SF_GET_BR_SBI_SEQ(p_chrg_typ,p_bk_chrg_typ,p_br_seq);

   --Get status of the charge
   vv_br_chrg_stat := PKG_CHRG_MANAGEMENT.F_GET_SVC_REF_STATUS(vn_sbi_seq,vv_oms_chrg,vv_br_chrg_typ,2);

ELSIF p_chrg_typ = 'BDT' THEN

   vn_sbi_seq := SF_GET_BR_SBI_SEQ(p_chrg_typ,p_bk_chrg_typ,NULL,p_br_seq);

   --Get status of the charge
   vv_br_chrg_stat := PKG_CHRG_MANAGEMENT.F_GET_SVC_REF_STATUS(vn_sbi_seq,vv_oms_chrg,vv_bdt_chrg_typ,2);

END IF;--IF p_chrg_typ = 'BR' THEN

dbms_output.put_line('P_BR_OMS_CANC--> vv_br_chrg_stat= '||vv_br_chrg_stat);

--GAIMIL quitar
IF p_void IS NULL THEN

   dbms_output.put_line('P_BR_OMS_CANC--> p_void es null');

else

  dbms_output.put_line('P_BR_OMS_CANC--> p_void es not null');

end if;

/*Desactivated until BDT confirmation previous DISAPPROVED status
  on PKG_CHRG_MANAGEMENT.P_OMS_CHARGE_CANCEL
--If OMS Service exists, Disapprove or
IF (vv_br_chrg_stat = vv_ic_apprv AND p_void IS NULL) OR
--VOID Booking  charge
   (vv_br_chrg_stat = vv_ic_disapprv AND p_void IS NOT NULL)
THEN
*/

   dbms_output.put_line('IF de vv_br_chrg_stat');

   vr_oms_canc_charge.sbi_seq  := vn_sbi_seq;
   vr_oms_canc_charge.rmk      := p_br_stat_rmk;
   vr_oms_canc_charge.voided   := p_void;
   vr_oms_canc_charge.Send_trx_ind := True;

   --Disapprove or VOID the booking charge Request Status will be approved or disapprove finally
   PKG_CHRG_MANAGEMENT.P_OMS_CHARGE_CANCEL(vr_oms_canc_charge);

--END IF;--IF (vv_br_chrg_stat = vv_ic_apprv AND p_void IS NULL) OR

EXCEPTION
WHEN OTHERS THEN

   -- Recording event in log table
   Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BR_OMS_CANC_CHRG',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'P_BR_SEQ= '||TO_CHAR(P_BR_SEQ)||' '||
                               'P_CHRG_TYP= '||P_CHRG_TYP||' '||
                               'P_BK_CHRG_TYP= '||P_BK_CHRG_TYP||' '||
                                SUBSTR(SQLERRM,1,1400),
                               'ERROR');

END;
/* Handle the processes related to review booking charges on toll charge */
PROCEDURE P_BR_TOLL_REV_CHRG
 (P_TCD_SEQ IN TOLL_CHRG_DETS.SEQ%TYPE
 ,P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_BS_SEQ IN BILL_SETS.SEQ%TYPE
 ,P_BR_STAT_RMK IN BK_RQSTS.STAT_RMK%TYPE
 ,P_CHRG_TYP IN INTERFACED_CHRGS.CHRG_TYP%TYPE
 ,P_STEP_RESULT IN TOLL_WORK_FLOW_RULES.POSBLE_STEP_RESULT%TYPE
 )
 IS

CV_TOLL_CHRG CONSTANT CHRG_GRP_NOS.TYP%TYPE := PKG_CHRG_MANAGEMENT.F_TOLL_CHRG;
CV_IC_APPRV CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_APPROVED;
CV_IC_DISAPPRV CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_DISAPPROVED;
CV_IC_VOID CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_VOID;

VR_TOLL_REVISE_CHARGE PKG_CHRG_MANAGEMENT.VR_TOLL_CHARGE_REVIEW_RECTYP;
VV_BR_CHRG_STAT INTERFACED_CHRGS.STATUS%TYPE;
VT_CHARGE_TAB PKG_CHRG_MANAGEMENT.VT_CHRG_TABLE_TYP;

--============================================================================
-- DESCRIPCION: Procedimiento para manejar la generación de registros de cargos de acuerdo
--                             a los status de booking en la factura de tolls.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha         Cambios realizados
-- -----------  ---------------    -----------------   -----------------------------------------
-- S88293  GVillarreal    31-Dec-2008   Version Inicial.
--
--=============================================================================
BEGIN

   --
   -- Get status of the Booking Charge
   Vv_br_chrg_stat := PKG_CHRG_MANAGEMENT.F_GET_SVC_REF_STATUS(P_tcd_seq,Cv_toll_chrg,P_chrg_typ,2);

   --
   -- Disapprove the Booking Charge on Toll Charge Details
   -- Void or Re-approve the booking charge on Toll Charge Details
   IF (Vv_br_chrg_stat = Cv_ic_apprv    AND P_step_result = Cv_ic_disapprv)
   OR (Vv_br_chrg_stat = Cv_ic_disapprv AND P_step_result IN (Cv_ic_apprv, Cv_ic_void))
   THEN

       --
       -- Get action for Booking Charge on Toll Charge Details
       -- REDO (APPROVE) = Reapprove Booking Charge
       -- UNDO (APPROVE) = Restore Booking Charge
       -- UNDO (DISAPPROVE, VOID) = Disapprove or VOID Booking Charge

       --
       -- Booking Charge Information on Toll Charges
       vr_toll_revise_charge.action_typ         := 'UNDO';
       vr_toll_revise_charge.bs_seq             := P_bs_seq;
       vr_toll_revise_charge.chrg_typ           := P_chrg_typ;
       vr_toll_revise_charge.Posble_step_result := P_step_result;
       vr_toll_revise_charge.rmk                := P_br_stat_rmk;
       vr_toll_revise_charge.Send_trx_ind       := True;

       --
       -- Review Charge on Toll Charge
       PKG_CHRG_MANAGEMENT.P_toll_charge_review(Vr_toll_revise_charge,Vt_charge_tab);

   END IF;

   --
   -- Clear variable tables
   vt_charge_tab.delete;

EXCEPTION
WHEN OTHERS THEN

   -- Recording event in log table
   Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BR_TOLL_REV_CHRG',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'P_BS_SEQ= '||TO_CHAR(P_BS_SEQ)||' '||
                               'P_CHRG_TYP= '||P_CHRG_TYP||' '||
                               'P_STEP_RESULT= '||P_STEP_RESULT||' '||
                                SUBSTR(SQLERRM,1,1400),
                               'ERROR');

END;
/* Handle the processes related to review booking charges on OMS charge */
PROCEDURE P_BR_OMS_REV_CHRG
 (P_SBI_SEQ IN SVC_BILL_INSTRS.SEQ%TYPE
 ,P_BS_SEQ IN BILL_SETS.SEQ%TYPE
 ,P_BR_STAT_RMK IN BK_RQSTS.STAT_RMK%TYPE
 ,P_CHRG_TYP IN INTERFACED_CHRGS.CHRG_TYP%TYPE
 ,P_STEP_RESULT IN TOLL_WORK_FLOW_RULES.POSBLE_STEP_RESULT%TYPE
 )
 IS

CV_IC_VOID CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_VOID;
CV_IC_DISAPPRV CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_DISAPPROVED;
CV_IC_APPRV CONSTANT INTERFACED_CHRGS.STATUS%TYPE := PKG_CHRG_MANAGEMENT.F_IC_APPROVED;
CV_OMS_CHRG CONSTANT CHRG_GRP_NOS.TYP%TYPE := PKG_CHRG_MANAGEMENT.F_OMS_CHRG;

VR_OMS_REVISE_CHARGE PKG_CHRG_MANAGEMENT.VR_OMS_CHARGE_REVIEW_RECTYP;
VV_BR_CHRG_STAT INTERFACED_CHRGS.STATUS%TYPE;

-- =========================================================================================
-- DESCRIPCION: Procedimiento para manejar la generación de registros de
--              cargos de acuerdo a los status de booking en la factura
--              de OMS.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
-- =========================================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker      Desarrollador   Fecha              Cambios realizados
-- -----------  --------------- -----------------  -----------------------------------------
-- S140880_P    JaDiaz          17-Mar-2011        Version Inicial.
--
-- =========================================================================================
BEGIN

   --
   -- Get status of the Booking Charge
   Vv_br_chrg_stat := PKG_CHRG_MANAGEMENT.F_GET_SVC_REF_STATUS(P_sbi_seq,Cv_oms_chrg,P_chrg_typ,2);
  PKG_PLOG.SEC_IMPLEMENTATION('PKG_BOOKING','en el p_br_oms_rev_chrg '||Vv_br_chrg_stat,'DEBUG');
    PKG_PLOG.SEC_IMPLEMENTATION('PKG_BOOKING','en el p_br_oms_rev_chrg '||P_step_result,'DEBUG');
   --
   -- Disapprove the Booking Charge on Toll Charge Details
   -- Void or Re-approve the booking charge on Toll Charge Details
   IF (Vv_br_chrg_stat = Cv_ic_apprv    AND P_step_result = Cv_ic_disapprv)
   OR (Vv_br_chrg_stat = Cv_ic_disapprv AND P_step_result IN (Cv_ic_apprv, Cv_ic_void))
   THEN

       --
       -- Get action for Booking Charge on Toll Charge Details
       -- REDO (APPROVE) = Reapprove Booking Charge
       -- UNDO (APPROVE) = Restore Booking Charge
       -- UNDO (DISAPPROVE, VOID) = Disapprove or VOID Booking Charge

       --
       -- Definicion de parametros de revision
            -------------------------------------------------------------------
       Vr_oms_revise_charge.Action_typ         := 'UNDO';
       Vr_oms_revise_charge.Bs_seq             := P_bs_seq;
       Vr_oms_revise_charge.Sbi_seq            := P_sbi_seq;
       Vr_oms_revise_charge.Chrg_typ           := P_chrg_typ;
       Vr_oms_revise_charge.rmk                := P_br_stat_rmk;
       Vr_oms_revise_charge.Send_trx_ind       := TRUE;
       Vr_oms_revise_charge.Posble_step_result := P_step_result;

       --
       -- Ejecucion del proceso de revision
        PKG_PLOG.SEC_IMPLEMENTATION('PKG_BOOKING','voy al pkg_chrg_mana','DEBUG');
       -------------------------------------------------------------------
       PKG_CHRG_MANAGEMENT.P_OMS_CHARGE_REVIEW(Vr_oms_revise_charge);

   END IF;

EXCEPTION
WHEN OTHERS THEN

   -- Recording event in log table
   Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BR_OMS_REV_CHRG',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'P_BS_SEQ= '||TO_CHAR(P_BS_SEQ)||' '||
                               'P_CHRG_TYP= '||P_CHRG_TYP||' '||
                               'P_STEP_RESULT= '||P_STEP_RESULT||' '||
                                SUBSTR(SQLERRM,1,1400),
                               'ERROR');

END P_BR_OMS_REV_CHRG;
/* Update the Booking Penalty Indicator due to Service Worklist Action. */
PROCEDURE P_BR_UPD_PEN_IND
 (P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_ERROR_CODE OUT VARCHAR2
 ,P_ERROR_MESS OUT VARCHAR2
 )
 IS

VV_ERROR_MESS VARCHAR2(2000);

--============================================================================
-- DESCRIPCION: Procedimiento para manejar el UPDATE al Penalty Indicator de un registro de
--                             Booking debido al DISAPPROVE (a través de la pantalla de Service Worklist)
--                            de una acción de VOID a un OMS Service en estatus REVISED.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha         Cambios realizados
-- -----------  ---------------    -----------------   -----------------------------------------
-- S88293  GVillarreal    20-Ene-2009   Version Inicial.
--
--=============================================================================
BEGIN

--Update to the Penalty Indicator due to a Service Worklist Disapprove Action
--of a VOID option from a OMS Service Revised Status

IF p_br_seq IS NOT NULL THEN

   UPDATE BK_RQSTS
      SET Penalty_Ind = 'Y'
    WHERE seq = p_br_seq;

END IF;--IF p_br_seq IS NOT NULL THEN

EXCEPTION
WHEN OTHERS THEN

   /*Error de BD Oracle*/
   vv_error_mess     := SQLERRM;
   p_error_mess      := vv_error_mess;

   IF INSTR(vv_error_mess, 'USR-') > 0 THEN

      p_error_code := SUBSTR(vv_error_mess, INSTR(vv_error_mess, 'USR-'), 9);

   ELSE

      p_error_code   := SQLCODE;

   END IF;--IF INSTR(vv_error_mess, 'USR-') > 0 THEN

   -- Recording event in log table
   Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BR_UPD_PEN_IND',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'P_BR_SEQ= '||TO_CHAR(P_BR_SEQ)||' '||
                                SUBSTR(SQLERRM,1,1500),
                               'ERROR');



END;
/* Approval of OMS Service according to booking information */
PROCEDURE P_BR_BILL_APPR_SBI
 (P_SBI_SEQ IN SVC_BILL_INSTRS.SEQ%TYPE
 ,P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_BS_SEQ IN BILL_SETS.SEQ%TYPE
 ,P_CHRG_TYP IN SVC_BILL_INSTRS.TYP%TYPE
 ,P_BK_CHRG_TYP IN SVC_BILL_INSTRS.BK_CHRG_TYP%TYPE
 ,P_SBI_APPR IN VARCHAR2 := NULL
 )
 IS

VV_OMS_CHRG CONSTANT CHRG_GRP_NOS.TYP%TYPE := PKG_CHRG_MANAGEMENT.F_OMS_CHRG;

CHARGE_TAB PKG_MB620000PR.CHARGE_TABLE_TYPE;
VV_BR_CHRG_STAT INTERFACED_CHRGS.STATUS%TYPE;

--============================================================================
-- DESCRIPCION: Procedimiento para manejar la aprobación de registros de OMS de acuerdo
--                             a la información de booking charge type.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha         Cambios realizados
-- -----------  ---------------    -----------------   -----------------------------------------
-- S88293       GVillarreal          31-Dec-2008         Version Inicial.
-- S100983.T1   Abdel E. Miranda S.  26-JUN-2008         SE MODIFICO EL PROCESO PARA LA APROBACION
--                                                       DE CARGOS, PARA QUE SE PROCESE LOS CAFGOS
--                                                       DE LOS PENALTIES (LATE ARRIVALS) DE MANERA
--                                                       MANERA DIFERENTE DEL RESTO DE LOS DEMAS.
--=============================================================================
BEGIN

IF p_br_seq IS NOT NULL AND p_sbi_seq IS NOT NULL THEN

   vv_br_chrg_stat := PKG_CHRG_MANAGEMENT.F_GET_SVC_REF_STATUS(p_sbi_seq,vv_oms_chrg,p_chrg_typ,2);

   --Get Booking Fee charge
   IF p_bk_chrg_typ = 'FEE' THEN

      charge_tab := PKG_MB620000PR.F_GET_BOOK_CHARGES(p_br_seq,'BF','O','S',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);

   --Get Cancellation charge
   ELSIF p_bk_chrg_typ = 'CAN' THEN

      --Get the Charges Table from booking charges
      charge_tab := PKG_MB620000PR.F_GET_BOOK_CHARGES(p_br_seq,'CF','O','S',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);

   --Get Penalty charge
   ELSIF p_bk_chrg_typ = 'PEN' THEN

      charge_tab := PKG_MB620000PR.F_GET_PENALTY(p_br_seq);

   END IF;--IF vv_bk_chrg_typ = 'FEE' THEN

   --Set the Service Status to PENDING
   PKG_CHRG_MANAGEMENT.p_create_svc_chrg_stat(p_sbi_seq,
                                              NULL,
                                              'PENDING');

   dbms_output.put_line('P_BR_BILL_APPR_SBI-vv_br_chrg_stat='||vv_br_chrg_stat);

  --For Approvals, the charge will be generated
   --For Re-Approvals the charge will be re-approved only if a charge exists on INTERFACED_CHRGS
   /*IF (vv_br_chrg_stat IS NOT NULL AND NVL(p_sbi_appr,' ') = 'Y') OR
      (vv_br_chrg_stat IS NULL     AND NVL(p_sbi_appr,'N') = 'N')
   THEN

      --Approve OMS Charge
      PKG_BOOKING.p_br_appr_oms_chrg(p_sbi_seq,
                                     p_br_seq,
                                     p_bs_seq,
                                     p_chrg_typ,
                                     charge_tab);

   END IF;--IF (vv_br_chrg_stat IS NOT NULL AND NVL(p_sbi_appr,' ') = 'Y') OR
   */
   --**********************************************
   -- NUEVO MANEJO DE LA APROVACION DE LOS CARGOS.
   -- CUANDO SE TRATA DE UN PENALTY 8LATE ARRIVAL)
   --**********************************************

 --S117771-los Penalty quedaran como PENDING para poder procesarolos en el SWL
 -- IF P_BK_CHRG_TYP = 'PEN' AND NVL(P_SBI_APPR, ' ') = 'Y'

    IF P_BK_CHRG_TYP <> 'PEN'      THEN
       PKG_BOOKING.p_br_appr_oms_chrg(p_sbi_seq,
                                     p_br_seq,
                                     p_bs_seq,
                                     p_chrg_typ,
                                     charge_tab);
     END IF;
   --**********************************************

   --Clean variables
   charge_tab.delete;

END IF;--IF p_br_seq IS NOT NULL AND p_sbi_seq IS NOT NULL THEN

EXCEPTION
WHEN OTHERS THEN

   -- Recording event in log table
   Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BR_BILL_APPR_SBI',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'P_BR_SEQ= '||TO_CHAR(P_BR_SEQ)||' '||
                               'P_CHRG_TYP= '||P_CHRG_TYP||' '||
                               'P_BK_CHRG_TYP= '||P_BK_CHRG_TYP||' '||
                                SUBSTR(SQLERRM,1,1400),
                               'ERROR');

END;
/* Approval of OMS Service according to booking information */
PROCEDURE P_BDR_BILL_APPR_SBI
 (P_SBI_SEQ IN SVC_BILL_INSTRS.SEQ%TYPE
 ,P_BR_SEQ IN BK_RQSTS.SEQ%TYPE
 ,P_BS_SEQ IN BILL_SETS.SEQ%TYPE
 ,P_CHRG_TYP IN SVC_BILL_INSTRS.TYP%TYPE
 ,P_BK_CHRG_TYP IN SVC_BILL_INSTRS.BK_CHRG_TYP%TYPE
 ,P_EFF_DATE IN DATE
 ,P_BR_RQST_BK_DATE IN BK_RQSTS.RQST_BK_DATE%TYPE
 ,P_BDR_RQST_DATE IN BK_DLT_RQSTS.RQST_DATE%TYPE
 ,P_BDR_RQST_CANCL_DATE IN BK_DLT_RQSTS.RQST_CANCL_DATE%TYPE
 ,P_BDR_STAT IN BK_DLT_RQSTS.STAT%TYPE
 ,P_BDR_PREV_STAT IN BK_DLT_RQSTS.STAT%TYPE
 ,P_SBI_APPR IN VARCHAR2 := NULL
 )
 IS

VV_OMS_CHRG CONSTANT CHRG_GRP_NOS.TYP%TYPE := PKG_CHRG_MANAGEMENT.F_OMS_CHRG;

CHARGE_BDR_REC PKG_MB621000PR.RATE_RECORD_TYPE;
VV_BDR_CHRG_STAT INTERFACED_CHRGS.STATUS%TYPE;

--============================================================================
-- DESCRIPCION: Procedimiento para manejar la aprobación de registros de OMS de acuerdo
--                             a la información de booking charge type.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha         Cambios realizados
-- -----------  ---------------    -----------------   -----------------------------------------
-- S88293  GVillarreal    31-Dec-2008   Version Inicial.
--
--=============================================================================
BEGIN

charge_bdr_rec := NULL;

IF p_br_seq IS NOT NULL AND p_sbi_seq IS NOT NULL THEN

   vv_bdr_chrg_stat := PKG_CHRG_MANAGEMENT.F_GET_SVC_REF_STATUS(p_sbi_seq,vv_oms_chrg,p_chrg_typ,2);

   --Get Booking Daylight Granted or Forfeited Charge
   IF p_bdr_stat IN ('DTBGRA','DTFOR') THEN

      charge_bdr_rec := PKG_MB621000PR.F_GET_BDT_CHARGE(p_eff_date, p_br_rqst_bk_date, p_bdr_rqst_date);

   --Get Booking Daylight Cancellation charge
   ELSIF p_bdr_stat IN ('DTCAN') THEN

      charge_bdr_rec := PKG_MB621000PR.F_GET_CANCEL_BDT(p_eff_date, p_br_rqst_bk_date, NVL(p_bdr_rqst_cancl_date,p_eff_date), p_bdr_rqst_date, p_bdr_prev_stat, p_br_seq, 'S');

   END IF;-- IF p_bdr_stat IN ('DTBGRA','DTFOR') THEN

   --Set the OMS Service Status to PENDING (before the approval)
   PKG_CHRG_MANAGEMENT.p_create_svc_chrg_stat(p_sbi_seq,
                                              NULL,
                                             'PENDING');

   dbms_output.put_line('P_BDR_BILL_APPR-charge_bdr_rec.eff_date'||to_char(charge_bdr_rec.EFF_DATE,'DD-MON-YYYY'));
   dbms_output.put_line('P_BDR_BILL_APPR-charge_bdr_rec.QTY'||to_char(charge_bdr_rec.QTY));
   dbms_output.put_line('P_BDR_BILL_APPR-charge_bdr_rec.unit'||charge_bdr_rec.UNIT);
   dbms_output.put_line('P_BDR_BILL_APPR-charge_bdr_rec.description'||charge_bdr_rec.dESCRIPTION);
   dbms_output.put_line('P_BDR_BILL_APPR-charge_bdr_rec.rate'||to_char(charge_bdr_rec.RATE));
   dbms_output.put_line('P_BDR_BILL_APPR-charge_bdr_rec.amount'||to_char(charge_bdr_rec.amount));
   dbms_output.put_line('P_BDR_BILL_APPR-charge_bdr_rec.item_no'||charge_bdr_rec.ITEM_NO);


   dbms_output.put_line('P_BDR_BILL_APPR_SBI-vv_br_chrg_stat='||vv_bdr_chrg_stat);

   --For Approvals, the charge will be generated
   --For Re-Approvals the charge will be re-approved only if a charge exists on INTERFACED_CHRGS
   IF (vv_bdr_chrg_stat IS NOT NULL AND NVL(p_sbi_appr,' ') = 'Y') OR
      (vv_bdr_chrg_stat IS NULL     AND NVL(p_sbi_appr,'N') = 'N')
   THEN

      dbms_output.put_line('P_BDR_BILL_APPR_SBI-IF de VV_BR_CHRG_STAT');

      --Approve OMS Charge for Booking Daylight Charge
      PKG_BOOKING.p_bdr_appr_oms_chrg(p_sbi_seq,
                                      p_bs_seq,
                                      p_chrg_typ,
                                      charge_bdr_rec);

   END IF;--IF (vv_bdr_chrg_stat IS NOT NULL AND NVL(p_sbi_appr,' ') = 'Y') OR


END IF;--IF p_br_seq IS NOT NULL AND p_sbi_seq IS NOT NULL THEN

EXCEPTION
WHEN OTHERS THEN

   -- Recording event in log table
   Pkg_Plog.SEC_IMPLEMENTATION('PKG_BOOKING.P_BR_BILL_APPR_SBI',
                               '*DATE='||TO_CHAR(SYSDATE,'DD-MON-YYYY HH24MI')||' '||
                               'P_BR_SEQ= '||TO_CHAR(P_BR_SEQ)||' '||
                               'P_CHRG_TYP= '||P_CHRG_TYP||' '||
                               'P_BK_CHRG_TYP= '||P_BK_CHRG_TYP||' '||
                                SUBSTR(SQLERRM,1,1400),
                               'ERROR');

END;

END PKG_BOOKING;