-- M:\APPS\Solicitudes\S195850_T1_T4\BD\PKG_MB620000PR.pks
--
-- Generated for Oracle 9i on Mon Jan 20  16:20:42 2014 by Server Generator 6.5.93.2.10
 

PROMPT Creating Package 'PKG_MB620000PR'
CREATE OR REPLACE PACKAGE PKG_MB620000PR IS
--============================================================================
-- DESCRIPCION: Este pauete se encarga de generar los cargos de Booking.
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
-- R######  ??????        ??-???-???? Versiones previas al 10-Ene-2008.
 -- S61519   VJaen         10-Ene-2008 Adicion de campo "descripcion" a tipo de
 --                                    registro "CANCEL_RECORD_TYPE".
 -- S65727  VJaen         18-Feb-2008 Adición de la función F_GET_LATE_ARR_CHRGS.
--=============================================================================
 --
   TYPE charge_record_type IS RECORD (
      eff_date      charges.svc_date%TYPE,
      qty           charges.qty%TYPE,
      unit          charges.uom%TYPE,
      description   charges.descr%TYPE,
      rate          charges.rate%TYPE,
      amount        charges.chrg_amt%TYPE,
      item_no       charges.tar_tar_no%TYPE,
      acg_cd        charges.acg_cd%TYPE
   );

   TYPE cancel_record_type IS RECORD (
      date_from         NUMBER (10, 5),
      date_to           NUMBER (10, 5),
      tar_no            charges.tar_tar_no%TYPE,
      PERCENT           NUMBER (3),
      amount            charges.chrg_amt%TYPE,
      tar_min           charges.tar_tar_no%TYPE,
      min_amount        charges.chrg_amt%TYPE,
      description       charges.descr%TYPE,
      min_description   charges.descr%TYPE,
      acg_cd            charges.acg_cd%TYPE
   );

   TYPE charge_table_type IS TABLE OF charge_record_type
      INDEX BY BINARY_INTEGER;

   TYPE cancel_table_type IS TABLE OF cancel_record_type
      INDEX BY BINARY_INTEGER;

-- Sub-Program Unit Declarations
   FUNCTION f_get_book_charges (
      p_br                IN   NUMBER,
      p_stat              IN   VARCHAR2,
      p_ind               IN   VARCHAR2,
      p_mode              IN   CHAR,
      p_date              IN   DATE,
      p_pcums             IN   NUMBER,
      p_pcgross           IN   NUMBER,
      p_ondeck            IN   NUMBER,
      p_gf_ind            IN   VARCHAR2,
      p_hs_ind            IN   VARCHAR2,
      p_br_stat           IN   VARCHAR2,
      p_br_bk_date        IN   DATE,
      p_br_cancl_date     IN   DATE,
      p_br_arr_time_par   IN   VARCHAR2,
      p_req_date          IN   DATE DEFAULT NULL,
      p_bs_seq_swap       IN   NUMBER DEFAULT NULL,
      -- Billing Set Sequence of Swapped Vessel (For correction)
      p_br_swap_pc_ums    IN   NUMBER DEFAULT NULL,
      -- PC_UMS of Swapped Vessel (For correction)
      p_qty_teu           IN   NUMBER DEFAULT NULL,
-- TEU Total allow del certificado para los 07 o NTT para los Hibridos  (For correction)
      p_vty_cd            IN   VARCHAR2 DEFAULT NULL,
-- Vessel Type(For correction) -- PC_UMS of Swapped Vessel (For correction)  -- TEU Total allow del certificado para los 07 o NTT para los Hibridos -- Jdam 25-nov-2005
      p_sin_seq           IN   NUMBER DEFAULT NULL,
      -- S1519 Jdam 23-nov-2005 ( for correction )
      p_cgn_seq           IN   NUMBER DEFAULT NULL,
      -- S4332 Jdam 10-mar-2006  ( for correction )
      p_bid_amt           IN   NUMBER DEFAULT NULL,
      --S4332  Jdam 10-mar-2006 ( for correction )
      p_extm_beam         IN   cust_vsl_chars.extm_beam%TYPE DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_loa               IN   cust_vsl_chars.len_overall%TYPE DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_ic_seq            IN   NUMBER DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_bofr_amt          IN   NUMBER DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_swap_amt          IN   NUMBER DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_subs_amt          IN   NUMBER DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_chgd_amt          IN   NUMBER DEFAULT NULL,
   -- S61519  JaDiaz 12-dec-2007    
      p_action            IN   VARCHAR2  DEFAULT NULL
     --S195850_T1_T4 
   )
      RETURN charge_table_type;

   FUNCTION f_auction                                -- S4332 Jdam 10-mar-2006
                     (
      p_mode      IN   CHAR,
      p_stat      IN   VARCHAR2,
      p_br_seq    IN   NUMBER,                           --( for operational )
      p_cgn_seq   IN   NUMBER,                            --( for correction )
      p_bid_amt   IN   NUMBER                            -- ( for correction )
   )
      RETURN charge_table_type;

   FUNCTION f_gen_bp_id (p_rqst_bk_date IN DATE, p_rqst_date IN DATE)
      RETURN bk_pers.ID%TYPE;

   FUNCTION f_get_br_bp_id (p_br IN NUMBER)
      RETURN NUMBER;

   FUNCTION f_get_cancel_charges (
      p_br                IN   NUMBER,
      p_mode              IN   CHAR,
      p_date              IN   DATE,
      p_pcums             IN   NUMBER,
      p_pcgross           IN   NUMBER,
      p_ondeck            IN   NUMBER,
      p_gf_ind            IN   VARCHAR2,
      p_hs_ind            IN   VARCHAR2,
      p_br_stat           IN   VARCHAR2,
      p_br_bk_date        IN   DATE,
      p_br_cancl_date     IN   DATE,
      p_br_arr_time_par   IN   VARCHAR2,
      p_req_date          IN   DATE DEFAULT NULL,
      p_bs_seq_swap       IN   NUMBER DEFAULT NULL,
      -- Billing Set Sequence of Swapped Vessel (For correction)
      p_br_swap_pc_ums    IN   NUMBER DEFAULT NULL,
      -- PC_UMS of Swapped Vessel (For correction)
      p_qty_teu           IN   NUMBER DEFAULT NULL,
-- TEU Total allow del certificado para los 07 o NTT para los Hibridos  (For correction)
      p_vty_cd            IN   VARCHAR2 DEFAULT NULL,
-- Vessel Type(For correction) -- PC_UMS of Swapped Vessel (For correction) -- S1519 Jdam 25-nov-2005
      p_extm_beam         IN   cust_vsl_chars.extm_beam%TYPE DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_loa               IN   cust_vsl_chars.len_overall%TYPE DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_bofr_amt          IN   NUMBER DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_swap_amt          IN   NUMBER DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_subs_amt          IN   NUMBER DEFAULT NULL,
      -- S61519  JaDiaz 12-dec-2007
      p_chgd_amt          IN   NUMBER DEFAULT NULL
   -- S61519  JaDiaz 12-dec-2007
   )
      RETURN cancel_table_type;

   FUNCTION f_get_booking (
      p_hs         IN   VARCHAR2,
      p_gr         IN   VARCHAR2,
      p_pcums      IN   NUMBER,
      p_pcgross    IN   NUMBER,
      p_ondeck     IN   NUMBER,
      p_date       IN   DATE,
      p_len        IN   NUMBER,
      p_eff_date   IN   DATE,
      p_qty_teu    IN   NUMBER,
      p_vty_cd     IN   VARCHAR2,
      p_teu_date   IN   DATE,
      p_bk_stat    IN   VARCHAR2 DEFAULT NULL,
      p_req_date   IN   DATE DEFAULT NULL,
      p_beam       IN   NUMBER DEFAULT NULL,
      p_bp_id      IN   NUMBER DEFAULT NULL,
      p_tbi_ind    IN   VARCHAR2 DEFAULT 'N',
      --S94501 Fase 2 JDArosemena
      p_bk_date    IN   DATE DEFAULT NULL
      --
   )
      RETURN charge_table_type;

   FUNCTION f_get_for_eff_date
      RETURN DATE;

   FUNCTION f_fill_cancel_table (
      p_summ            IN   NUMBER,
      p_date            IN   DATE,
      p_arr_time_par    IN   VARCHAR2,
      p_eff_date        IN   DATE,
      p_canc_eff_date   IN   DATE,
      p_stat_date       IN   DATE,
      p_br_stat         IN   bk_rqsts.stat%TYPE DEFAULT NULL,
      p_rqst_bk_date    IN   bk_rqsts.rqst_bk_date%TYPE DEFAULT NULL,
      p_rqst_date       IN   bk_rqsts.rqst_date%TYPE DEFAULT NULL,
      p_return_all      IN   VARCHAR2 := 'Y'
   )
      RETURN cancel_table_type;

   FUNCTION f_chk_if_gf (p_sin NUMBER)
      RETURN VARCHAR2;

   FUNCTION f_chk_if_hs (p_seq IN NUMBER)
      RETURN VARCHAR2;

   FUNCTION f_get_sin (p_br IN NUMBER)
      RETURN NUMBER;

   FUNCTION f_get_br_stat (p_br IN NUMBER)
      RETURN VARCHAR2;

   FUNCTION f_get_br_stat_date (p_br IN NUMBER)
      RETURN DATE;

   FUNCTION f_get_bk_date (p_br IN NUMBER)
      RETURN DATE;

   FUNCTION f_get_bs_seq (p_br IN NUMBER)
      RETURN NUMBER;

   FUNCTION f_get_penalty (
      p_br         IN   NUMBER,
      p_req_date   IN   DATE DEFAULT NULL,
      p_trn_date   IN   DATE DEFAULT NULL
   )
      RETURN charge_table_type;

   FUNCTION f_get_bk_chg_date (p_msg1 OUT VARCHAR, p_msg2 OUT VARCHAR2)
      RETURN DATE;

   FUNCTION f_get_br_req_date (p_br IN NUMBER)
      RETURN DATE;

   FUNCTION f_get_swap_bs (p_br IN NUMBER)
      RETURN NUMBER;

   FUNCTION f_get_prev_tons (p_sin IN NUMBER, p_date IN DATE)
      RETURN NUMBER;

   FUNCTION f_get_bk_canc_chg (p_msg3 OUT VARCHAR, p_msg4 OUT VARCHAR2)
      RETURN DATE;

   FUNCTION f_get_late_arr_chrgs (
      p_eff_date       DATE DEFAULT SYSDATE,
      p_req_arr_time   VARCHAR,
      p_bk_date        DATE,
      p_bk_fee         NUMBER
   )
      RETURN cancel_table_type;

   TYPE BOOK_CHARGE_PARAM_RT IS RECORD
  (CHRG_MODE VARCHAR2(3)
  ,EFF_DATE DATE
  ,RQST_DATE DATE
  ,RQST_BK_DATE DATE
  ,BEAM CUST_VSL_CHARS.EXTM_BEAM%TYPE
  ,LOA CUST_VSL_CHARS.LEN_OVERALL%TYPE
  ,BP_ID BK_RQSTS.BP_ID%TYPE
  ,BR_CHARGE_TRN VARCHAR2(10)
  ,BR_STAT BK_RQSTS.STAT%TYPE
  ,FIX_AMT BK_RQSTS.FIX_AMT%TYPE
  ,BO_AMT BK_RQST_DETS.AMOUNT%TYPE
  ,TBI_IND VARCHAR2(1));

   FUNCTION F_GET_BOOK_CHARGES(
   BOOK_PARAM_REC IN OUT PKG_MB620000PR.BOOK_CHARGE_PARAM_RT
   )
   RETURN PKG_MB620000PR.CHARGE_TABLE_TYPE;

   FUNCTION F_GET_BOOKING(
   BOOKING_PARAM_REC PKG_MB620000PR.BOOK_CHARGE_PARAM_RT
   )
   RETURN PKG_MB620000PR.CHARGE_TABLE_TYPE;
END PKG_MB620000PR;

/
SHOW ERROR
