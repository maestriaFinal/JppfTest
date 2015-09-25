-- M:\APPS\Solicitudes\S195850_T1_T4\BD\PKG_MB620000PR.pkb
--
-- Generated for Oracle 9i on Mon Jan 20  16:20:42 2014 by Server Generator 6.5.93.2.10
 

PROMPT Creating Package Body 'PKG_MB620000PR'
CREATE OR REPLACE PACKAGE BODY PKG_MB620000PR IS
   --
   -- Package Local Declarations
   vt_tab   pkg_book_tcm_util.tcm_tab_typ;
   vt_rec   pkg_book_tcm_util.tcm_rec_typ;

   --
   FUNCTION f_chk_bk_chrg_by_dim (p_req_date IN DATE)
      RETURN BOOLEAN
   IS
-- ======================================================================================================
-- DESCRIPCION:
--   Esta funcion se encarga de determinar basado en la fecha del request, si el cargo de
--   booking debera ser generado en base a las dimensiones del buque.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
-- ======================================================================================================
-- HISTORIA DE MODIFICACIONES
-- SOS       Desarrollador   Fecha         Cambios realizados
-- --------- --------------  -----------   -------------------------------------------------------
-- S61519    JADIAZ          18-DEC-2007   Version Inicial
-- ======================================================================================================
--
-- Function Local Declarations
--
      vd_dim_chrg_eff_date   DATE   := pkg_book_datadict.f_bill_eff_date_v1;
      vd_dim_chrg_grc_prd    NUMBER
                                  := pkg_book_datadict.f_bill_grace_period_v1;
   --
   BEGIN
      --
      IF p_req_date >= vd_dim_chrg_eff_date + NVL (vd_dim_chrg_grc_prd, 0)
      THEN
         RETURN TRUE;
      ELSE
         RETURN FALSE;
      END IF;
   END f_chk_bk_chrg_by_dim;

   FUNCTION F_GET_TAR_NO_BY_AMOUNT(P_RQST_DATE     BK_RQSTS.RQST_DATE%TYPE
                                  ,P_TAR_NO        TARIFF_RATES.TAR_TAR_NO%type
                                  ,P_BK_FEE_AMOUNT TARIFF_RATES.TARIFF_RATE%TYPE
                                  ,P_BR_STAT       BK_RQSTS.STAT%TYPE
                                  ,P_BP_ID         BK_RQSTS.BP_ID%TYPE) RETURN TARIFF_RATES.TAR_TAR_NO%TYPE IS
-- ======================================================================================================
-- DESCRIPCION:
--   Esta funcion se encarga de determinar si existe un periodo de gracia para la reservacion.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
-- ======================================================================================================
-- HISTORIA DE MODIFICACIONES
-- SOS       Desarrollador   Fecha         Cambios realizados
-- --------- --------------  -----------   -------------------------------------------------------
-- S100672   JADIAZ          04-JUN-2009   Version Inicial
--                                         Proposito de obtener el TAR_NO basado en el monto del cargo
--                                         de manera general, pero por el implementado solo en aquellas
--                                         transacciones de SUBS, CHGD y SWAP
-- ======================================================================================================
--
   -- Local Declarations
   cv_bk_dim_fix_cd    CONSTANT tariff_category_mappings.typ%TYPE         := pkg_book_datadict.f_typ_bkdimfix;
   cv_unit_beam_cd     CONSTANT tariff_category_mappings.range1_unit%TYPE := pkg_book_datadict.f_runit_beam;
   cv_unit_loa_cd      CONSTANT tariff_category_mappings.range2_unit%TYPE := pkg_book_datadict.f_runit_loa;
   cv_std_tar_grp_cd   CONSTANT tariff_acc_grp_assgs.tar_grp%TYPE         := pkg_book_datadict.f_tar_grp_std;
   cv_for_tar_grp_cd   CONSTANT tariff_acc_grp_assgs.tar_grp%TYPE         := pkg_book_datadict.f_tar_grp_forfeit;
   vv_tar_grp                   tariff_acc_grp_assgs.tar_grp%TYPE         := cv_std_tar_grp_cd;
   vv_tar_no                    tariff_rates.tar_tar_no%type;

   BEGIN

     --
     IF P_BR_STAT = 'FOR'
     THEN
         vv_tar_grp := cv_for_tar_grp_cd;
     END IF;

     --
     -- Registro en el PL/SQL Log
     pkg_plog.sec_implementation ('PKG_MB620000PR.F_GET_TAR_NO_BY_AMOUNT', '<vv_tar_grp> '      ||vv_tar_grp      ||
                                                                  CHR(10)||'<cv_bk_dim_fix_cd> '||cv_bk_dim_fix_cd||
                                                                  CHR(10)||'<cv_unit_beam_cd> ' ||cv_unit_beam_cd ||
                                                                  CHR(10)||'<cv_unit_loa_cd> '  ||cv_unit_loa_cd  ||
                                                                  CHR(10)||'<p_bp_id> '         ||p_bp_id         ||
                                                                  CHR(10)||'<p_bk_fee_amount> ' ||p_bk_fee_amount ||
                                                                  CHR(10)||'<p_rqst_date> '     ||p_rqst_date, 'DEBUG');

     --
     -- Se obtiene el numero de tarifa basado en el monto mayor obtenido entre el calculado por
     -- dimensiones y el transado mediante una operacion de SUBS, SWAP o CHGD.
     BEGIN
       --
       SELECT tr.tar_tar_no
         INTO vv_tar_no
         FROM tariff_category_mappings tcm
             ,tariff_acc_grp_assgs taga
             ,tariff_rates tr
        WHERE tcm.seq         = taga.tcm_seq
          AND taga.tar_no     = tr.tar_tar_no
          AND taga.tar_grp    = vv_tar_grp
          AND tcm.typ         = cv_bk_dim_fix_cd
          AND tcm.range1_unit = cv_unit_beam_cd
          AND tcm.range2_unit = cv_unit_loa_cd
          AND tcm.bk_period   = p_bp_id
          AND tr.tariff_rate  = p_bk_fee_amount
          AND p_rqst_date between tcm.date_from  and tcm.date_to
          AND p_rqst_date between taga.date_from and taga.date_to
          AND p_rqst_date between tr.date_from   and tr.date_to;
       --
     EXCEPTION
       --
       -- En caso de presentarzs cualquier exception a la regla, digase que se encontro mas de una
       -- tarifa para el mismo monto, se blanquea el valor para posteriormente ser evaluado.
       WHEN OTHERS THEN
         vv_tar_no := Null;
     END;

     --
     -- Si la tarifa es NULA por cualquier causa, se devuelve la tarifa original obtenida
     -- mediante el calculo de la funcion F_GET_BOOKING.
     IF vv_tar_no IS NULL
     THEN vv_tar_no := P_tar_no;
     END IF;

     RETURN vv_tar_no;

   END F_GET_TAR_NO_BY_AMOUNT;

   FUNCTION f_chk_bk_grace_prd (p_req IN DATE, p_trn IN DATE)
      RETURN BOOLEAN
   IS
-- ======================================================================================================
-- DESCRIPCION:
--   Esta funcion se encarga de determinar si existe un periodo de gracia para la reservacion.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
-- ======================================================================================================
-- HISTORIA DE MODIFICACIONES
-- SOS       Desarrollador   Fecha         Cambios realizados
-- --------- --------------  -----------   -------------------------------------------------------
-- S61519    JADIAZ          18-DEC-2007   Se incluye validaciones sobre nuevo metodo basado en
--                                         dimensiones
-- ======================================================================================================
--
-- Function Local Declarations
      vv_grace      BOOLEAN;
      vd_eff_date   DATE;
   --
   BEGIN
      --
      vd_eff_date := pkg_toll_charges.f_get_bill_eff_date ('TEU_4856', p_trn);

      --
      IF p_trn < vd_eff_date
      THEN
         IF     TO_CHAR (p_req, 'yyyy') = '2003'
            AND TO_CHAR (p_trn, 'yyyy') = '2004'
         THEN
            vv_grace := TRUE;
         ELSE
            vv_grace := FALSE;
         END IF;
      ELSIF p_trn >= vd_eff_date
      THEN
         IF p_req < vd_eff_date
         THEN
            vv_grace := TRUE;
         ELSIF p_req >= vd_eff_date
         THEN
            vv_grace := FALSE;
         END IF;
      END IF;

      --
      RETURN vv_grace;
   --
   EXCEPTION
      WHEN OTHERS
      THEN
         pkg_plog.sec_implementation ('PKG_MB620000PR.F_CHK_BK_GRACE_PRD',
                                      'EXCEPTION WHEN OTHERS=' || SQLERRM,
                                      'INFO'
                                     );
         vv_grace := FALSE;
         RETURN vv_grace;
   END f_chk_bk_grace_prd;

   FUNCTION f_get_dtu_seq (
      p_mode      IN   VARCHAR2,
      p_ind       IN   VARCHAR2 DEFAULT NULL,
      p_seq_ref   IN   NUMBER
   )
      RETURN NUMBER
   IS
-- ======================================================================================================
-- DESCRIPCION:
--
-- NOTAS: Esta funcion obtiene el DTU_SEQ basado en la secuencia de la reservacion.
--
-- REQUERIMIENTOS:
--
-- ======================================================================================================
-- HISTORIA DE MODIFICACIONES
-- SOS       Desarrollador   Fecha         Cambios realizados
-- --------- --------------  -----------   -------------------------------------------------------
-- S61519    JADIAZ          03-JAN-2008   Initial Version
-- ======================================================================================================
--
-- Function Local Declarations
      vn_dtu_seq   bk_rqsts.dtu_seq%TYPE;
   --
   BEGIN
       --
      -- Modo standard se obtiene el DTU_SEQ de la tabla BK_RQSTS
      IF p_mode = 'S'
      THEN
         --
         BEGIN
            SELECT dtu_seq
              INTO vn_dtu_seq
              FROM bk_rqsts bk
             WHERE bk.seq = p_seq_ref;
         EXCEPTION
            WHEN OTHERS
            THEN
               vn_dtu_seq := NULL;
         END;
        --
       --
      -- Modo correccion
      ELSIF p_mode = 'C'
      THEN
         --
         -- Se obtiene el DTU_SEQ de la tabla BK_RQSTS basado en la secuencia de la reservacion
         -- registrada en el campo TOLL_REQ_SEQ_REF de la tabla CHRG_GRP_NOS
         IF p_ind = 'T'
         THEN
            --
            BEGIN
               SELECT br.dtu_seq
                 INTO vn_dtu_seq
                 FROM bk_rqsts br, chrg_grp_nos cgn
                WHERE br.seq = cgn.toll_req_seq_ref
                  AND cgn.seq = p_seq_ref
                  AND cgn.typ = 'TOLL';
            EXCEPTION
               WHEN OTHERS
               THEN
                  vn_dtu_seq := NULL;
            END;
            --
         --
         -- Se obtiene el DTU_SEQ de la tabla BK_RQSTS basado en la secuencia de la reservacion
         -- registrada en el campo BR_SEQ de la tabla SVC_BILL_INSTRS
         ELSIF p_ind = 'O'
         THEN
            --
            BEGIN
               SELECT br.dtu_seq
                 INTO vn_dtu_seq
                 FROM bk_rqsts br, svc_bill_instrs sbi, interfaced_chrgs ic
                WHERE br.seq = sbi.br_seq
                  AND sbi.seq = ic.oms_sbi_seq_ref
                  AND ic.seq = p_seq_ref;
            EXCEPTION
               WHEN OTHERS
               THEN
                  vn_dtu_seq := NULL;
            END;
         --
         END IF;
      --
      END IF;

      --
      RETURN vn_dtu_seq;
   --
   END f_get_dtu_seq;

   FUNCTION f_get_tbi_ind (
      p_dtu_seq   IN   NUMBER,
      p_vty_cd    IN   ship_id_no.vty_cd%TYPE
   )
      RETURN VARCHAR2
   IS
-- ======================================================================================================
-- DESCRIPCION:
--
-- NOTAS: Esta funcion obtiene el GRP_SEQ de la tabla FAMILY_GRPS basado en
--        la secuencia del DTU_SEQ.
--
--
-- REQUERIMIENTOS:
--
--
-- ======================================================================================================
-- HISTORIA DE MODIFICACIONES
-- SOS       Desarrollador   Fecha         Cambios realizados
-- --------- --------------  -----------   -------------------------------------------------------
-- S61519    JADIAZ          03-JAN-2008   Initial Version
-- ======================================================================================================
--
-- Function Local Declarations
      cv_tug_cd   CONSTANT vsl_typs.cd%TYPE     := pkg_book_datadict.f_tug_cd;
      vn_barge_grp_seq     family_grps.grp_seq%TYPE;
      vn_tug_grp_seq       family_grps.grp_seq%TYPE;
      vv_tbi_ind           VARCHAR2 (1)               := 'N';

      --
      CURSOR c_fgp_barge (pn_dtu_seq NUMBER)
      IS
         SELECT fgp.grp_seq barge_grp_seq
           FROM itin_items iti,
                cust_sched_needs csn,
                family_grps fgp,
                ship_id_no SIN
          WHERE iti.dtu_group_by_seq = pn_dtu_seq
            AND iti.stat_cd != 'CAN'
            AND iti.typ = 'TII'                      -- Itinerario de transito
            AND fgp.typ = 'TBI'                        -- Tug Barge Integrated
            AND iti.csn_seq = csn.seq
            AND csn.sin_seq = fgp.sin_seq
            AND csn.sin_seq = SIN.seq
            AND fgp.sin_seq = SIN.seq
            AND SIN.vty_cd != cv_tug_cd;

      --
      CURSOR c_fgp_tug (pn_dtu_seq NUMBER)
      IS
         SELECT fgp.grp_seq tug_grp_seq
           FROM itin_items iti,
                cust_sched_needs csn,
                family_grps fgp,
                ship_id_no SIN
          WHERE iti.dtu_group_by_seq = pn_dtu_seq
            AND iti.stat_cd != 'CAN'
            AND iti.typ = 'TII'                      -- Itinerario de transito
            AND fgp.typ = 'TBI'                        -- Tug Barge Integrated
            AND iti.csn_seq = csn.seq
            AND csn.sin_seq = fgp.sin_seq
            AND csn.sin_seq = SIN.seq
            AND fgp.sin_seq = SIN.seq
            AND SIN.vty_cd = cv_tug_cd;
   --
   BEGIN
      --
      IF p_vty_cd = cv_tug_cd
      THEN
          --
          -- Se abre el cursor para obtener los diferentes family group sequences asignados a la
         -- barcasa para la secuencia del DTU
         OPEN c_fgp_barge (p_dtu_seq);

         LOOP
            --
            -- Se define la secuencia del fgp en la barcaza.
            FETCH c_fgp_barge
             INTO vn_barge_grp_seq;

            --
            -- Si la sequencia de la barcaza no es nula.
            IF vn_barge_grp_seq IS NOT NULL
            THEN
                --
                -- Se abre el cursor para obtener los diferentes family group sequences asignados al
               -- remolcador para la secuencia del DTU
               OPEN c_fgp_tug (p_dtu_seq);

               LOOP
                  --
                  -- Se define la secuencia del fgp en la remolcador.
                  FETCH c_fgp_tug
                   INTO vn_tug_grp_seq;

                    --
                    -- Se compara la secuencia del family group de la barcaza con la del remolcador,
                  -- si estas son iguales entonces estan efectivamente definidos como TBI.
                  IF vn_barge_grp_seq = vn_tug_grp_seq
                  THEN
                     vv_tbi_ind := 'Y';
                  END IF;

                  --
                  EXIT WHEN c_fgp_tug%NOTFOUND OR vv_tbi_ind = 'Y';
               --
               END LOOP;

               CLOSE c_fgp_tug;
            --
            END IF;

            --
            EXIT WHEN c_fgp_barge%NOTFOUND OR vv_tbi_ind = 'Y';
         --
         END LOOP;

         CLOSE c_fgp_barge;
      END IF;

      --
      RETURN vv_tbi_ind;
   --
   EXCEPTION
      WHEN OTHERS
      THEN
         RETURN 'N';
   END f_get_tbi_ind;

   FUNCTION f_display_chrg_desc (
      p_descr   VARCHAR2 DEFAULT '$$NO-VALUE$$',
      p_par_1   VARCHAR2 DEFAULT '$$NO-VALUE$$',
      p_par_2   VARCHAR2 DEFAULT '$$NO-VALUE$$',
      p_par_3   VARCHAR2 DEFAULT '$$NO-VALUE$$',
      p_par_4   VARCHAR2 DEFAULT '$$NO-VALUE$$'
   )
      RETURN VARCHAR2
   IS
      --
      -- Local Declarations
      vv_message_text   VARCHAR2 (2000);
                                   -- Guarda el texto del mensaje a desplegar
   --
   BEGIN
      --
      vv_message_text := p_descr;

      --
      IF NVL (p_par_1, 'X') <> '$$NO-VALUE$$'
      THEN
         --
         -- Aquí se reemplaza el valor o valores que queremos incluir en el mensaje, en la posición donde se encuentra el parámetro
         vv_message_text := REPLACE (vv_message_text, ':1', p_par_1);
      END IF;

      IF NVL (p_par_2, 'X') <> '$$NO-VALUE$$'
      THEN
         --
         -- Aquí se reemplaza el valor o valores que queremos incluir en el mensaje, en la posición donde se encuentra el parámetro
         vv_message_text := REPLACE (vv_message_text, ':2', p_par_2);
      END IF;

      IF NVL (p_par_3, 'X') <> '$$NO-VALUE$$'
      THEN
         --
         -- Aquí se reemplaza el valor o valores que queremos incluir en el mensaje, en la posición donde se encuentra el parámetro
         vv_message_text := REPLACE (vv_message_text, ':3', p_par_3);
      END IF;

      IF NVL (p_par_4, 'X') <> '$$NO-VALUE$$'
      THEN
         --
         -- Aquí se reemplaza el valor o valores que queremos incluir en el mensaje, en la posición donde se encuentra el parámetro
         vv_message_text := REPLACE (vv_message_text, ':4', p_par_4);
      END IF;

      RETURN vv_message_text;
   EXCEPTION
      WHEN OTHERS
      THEN
         RETURN p_descr;
   END f_display_chrg_desc;

   FUNCTION f_best_offer (
      pv_mode             IN   VARCHAR2,
      pn_br_seq           IN   bk_rqsts.seq%TYPE,
      pn_best_offer_amt   IN   NUMBER
   )
      RETURN charge_table_type
   IS
      --
      charge_tab             charge_table_type;
      cn_chrg_qty   CONSTANT INTEGER                      := 1;
      vv_typ                 bk_rqsts.typ%TYPE;
      vn_bsps_seq            bk_rqsts.bsps_seq%TYPE;
      vd_rqst_date           bk_rqsts.rqst_date%TYPE;
      vd_rqst_bk_date        bk_rqsts.rqst_bk_date%TYPE;
      vd_stat_date           bk_rqsts.stat_date%TYPE;
      vn_best_offer_amt      bk_rqst_dets.amount%TYPE     := 0;

      --
      CURSOR c_bk_rqsts
      IS
         SELECT br.typ, br.bsps_seq, br.rqst_date, br.stat_date,
                br.rqst_bk_date
           FROM bk_rqsts br
          WHERE br.seq = pn_br_seq;
   --
   BEGIN
      --
      OPEN c_bk_rqsts;

      FETCH c_bk_rqsts
       INTO vv_typ, vn_bsps_seq, vd_rqst_date, vd_stat_date, vd_rqst_bk_date;

      CLOSE c_bk_rqsts;

      --
      IF pkg_book_reftab_util.f_is_bestoffer (vn_bsps_seq) = 'Y'
      THEN
         --
         IF pv_mode = 'S'
         THEN
            vn_best_offer_amt :=
               NVL
                  (pkg_booking.f_get_bk_rqst_dets
                                                (pn_br_seq,
                                                 pkg_book_datadict.f_bkp_bofr,
                                                 1
                                                ),
                   0
                  );
         ELSE
            vn_best_offer_amt := NVL (pn_best_offer_amt, 0);
         END IF;

         pkg_plog.sec_implementation ('PKG_MB620000PR.F_BEST_OFFER',
                                         'vn_best_offer_amt='
                                      || vn_best_offer_amt
                                      || CHR (10)
                                      || 'vd_rqst_date='
                                      || vd_rqst_date,
                                      'INFO'
                                     );

         IF vn_best_offer_amt > 0
         THEN
            --
            charge_tab.DELETE;
            --
            vt_tab := pkg_book_tcm_util.f_get_bk_best_off (vd_rqst_date);
            --
            pkg_plog.sec_implementation ('PKG_MB620000PR.F_BEST_OFFER',
                                         'vt_tab.COUNT=' || vt_tab.COUNT,
                                         'INFO'
                                        );

            --
            IF vt_tab IS NOT NULL
            THEN
               --
               FOR i IN 1 .. vt_tab.COUNT
               LOOP
                  --
                  vt_rec := vt_tab (i);

                  --
                  -- Si la tarifa no es nula, se definen los campos de la tabla pl/sql
                  IF vt_rec.tar_no IS NOT NULL
                  THEN
                     --
                     charge_tab (i).unit := 'UNI';
                     charge_tab (i).qty := cn_chrg_qty;
                     charge_tab (i).acg_cd := vt_rec.acg_cd;
                     charge_tab (i).item_no := vt_rec.tar_no;
                     charge_tab (i).rate := vn_best_offer_amt;
                     charge_tab (i).amount := vn_best_offer_amt;
                     charge_tab (i).eff_date := vd_stat_date;
                     charge_tab (i).description :=
                        f_display_chrg_desc (vt_rec.tar_descr,
                                             TO_CHAR (vd_rqst_bk_date,
                                                      'DD-MON-RRRR'
                                                     )
                                            );
                  --
                  END IF;
               --
               END LOOP;
            --
            END IF;                                       -- vt_tab evaluation
         --
         END IF;                                      -- vn_best_offer_amt > 0
      --
      END IF;                                            -- Class = Best offer

      --
      RETURN charge_tab;
   --
   EXCEPTION
      WHEN OTHERS
      THEN
         --
         RETURN charge_tab;
   --
   END f_best_offer;

   --
   FUNCTION f_get_book_charges (
      p_br                IN   NUMBER,      -- Booking Request Sequence Number
      p_stat              IN   VARCHAR2,
      -- Indicator of whether the charge is fee or cancellation
      p_ind               IN   VARCHAR2,
      -- Indicator of whether the fee goes in a toll invoice or oms invoice
      p_mode              IN   CHAR,
      -- Indicator of whether the fee a regular fee or a correction fee
      p_date              IN   DATE,
      -- billing date parameter (for correction)
      p_pcums             IN   NUMBER,      -- PCUMS net tons (for correction)
      p_pcgross           IN   NUMBER,        -- PCGROSS tons (for correction)
      p_ondeck            IN   NUMBER,        -- ON DECK tons (for correction)
      p_gf_ind            IN   VARCHAR2,
      -- Grand Father Indicator (for correction)
      p_hs_ind            IN   VARCHAR2,
      -- High Season Indicator (for correction)
      p_br_stat           IN   VARCHAR2,
      -- Status of Booking Request (for correction)
      p_br_bk_date        IN   DATE,   -- Date for which the vessel was booked
      p_br_cancl_date     IN   DATE,
      -- Date the Cancellation (status) was received
      p_br_arr_time_par   IN   VARCHAR2,
      -- Arrival time parameter for the vessel
      p_req_date          IN   DATE DEFAULT NULL,
      -- Request Date of Booking (for correction)
      p_bs_seq_swap       IN   NUMBER DEFAULT NULL,
      -- Billing Set Sequence of Swapped Vessel (For correction)
      p_br_swap_pc_ums    IN   NUMBER DEFAULT NULL,
      -- PC_UMS of Swapped Vessel (For correction)
      p_qty_teu           IN   NUMBER DEFAULT NULL,
-- TEU Total allow del certificado para los 07 o NTT para los Hibridos  (For correction)
      p_vty_cd            IN   VARCHAR2 DEFAULT NULL,
      -- Vessel Type(For correction) -- S1519 Jdam 25-Nov-2005
      p_sin_seq           IN   NUMBER DEFAULT NULL,
      -- S1519 Jdam 23-nov-2005 ( for correction )
      p_cgn_seq           IN   NUMBER DEFAULT NULL,
      -- S4332 Jdam 10-mar-2006 ( for correction )
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
      p_action            IN   VARCHAR2 DEFAULT NULL
   )
      RETURN charge_table_type
   IS
      -- PL/SQL Specification
      -- Program Data
      vn_days               NUMBER (8, 2);
      vn_hours              NUMBER (8, 2);
      x                     NUMBER (1);
      y                     NUMBER (1);
      vn_summ               NUMBER (8, 2);
      vv_stat               bk_rqsts.stat%TYPE; --booking status de un vessel
      vv_desc               charges.descr%TYPE;
      vd_stat_date          bk_rqsts.stat_date%TYPE;--fecha del booking status de un vessel
      vd_date               DATE;
      charge_tab            charge_table_type;
      cancel_tab            cancel_table_type;
      vn_br                 bk_rqsts.seq%TYPE;
      vn_sin                ship_id_no.seq%TYPE;
      vn_bs                 bill_sets.seq%TYPE;
      vv_hs_bp              VARCHAR2 (1);
      vv_grand              VARCHAR2 (1);
      vn_csn                cust_sched_needs.seq%TYPE;
      cvc_table             pkg_csn_vsl_chars.csn_cvc_table;
      vn_pcums              NUMBER (5);
      vn_ondeck             NUMBER (5);
      vn_gross              NUMBER (5);
      vd_bk_date            bk_rqsts.rqst_bk_date%TYPE;
      vv_arr_time_par       VARCHAR2 (10);
      vv_tar_no             tariffs.tar_no%TYPE;
      vn_length             cust_vsl_chars.len_overall%TYPE;
      vd_eff_date           DATE;
      vv_msg_1              VARCHAR2 (2000);
      vv_msg_2              VARCHAR2 (2000);
      vd_req_date           DATE;
      vn_swap_bs            bk_rqsts.bs_seq_swap%TYPE;
      vn_swap_pc_ums        pc_ums_certs.net_tons%TYPE;
      vv_other_pc_ums       VARCHAR2 (1);
      vn_qty_teu            pc_ums_certs.total_teu_allow%TYPE;
      vv_vty_cd             vsl_typs.cd%TYPE;
      vd_teu_date           DATE;
      vv_vsl_descr          vsl_typs.descr%TYPE;
      vv_teu_srce           VARCHAR2 (3);
      --tRACKER 5124
      vd_canc_eff_date      DATE;
      vv_msg_3              VARCHAR2 (2000);
      vv_msg_4              VARCHAR2 (2000);
      --Tracker ####
      vd_pcums_cert_date    DATE;
      vd_trn_date           DATE;
      vd_rqst_bk_date       bk_rqsts.rqst_bk_date%TYPE;
      vd_bk_stat_date       bk_rqsts.stat_date%TYPE;
      vv_bk_stat            bk_rqsts.stat%TYPE;
      -- S18227 Jdam 2-mar-2007
      vd_pcums_act_date     DATE;
      vn_net_tons           pc_ums_certs.vsl_net_tons%TYPE;
      vn_abov_dek           vsl_cgo_caps.abov_dek_cont_cap%TYPE;
      vn_blow_dek           vsl_cgo_caps.blow_dek_cont_cap%TYPE;
      --
      -- Added by JaDiaz on June 8, 2007 S28465
      vn_full_ld_disp       pc_ums_certs.lvc_metric_full_ld_disp%TYPE := NULL;
      vn_disp_ums_factor    NUMBER                                    := NULL;
      vd_chrg_date          DATE                                      := NULL;
      --
      -- S61519 Added by JaDiaz on December 20, 2007
      cn_rec_idx   CONSTANT NUMBER                                      := 1;
      vn_beam               cust_vsl_chars.extm_beam%TYPE;
      vn_bp_id              bk_rqsts.bp_id%TYPE;
      vn_tug_grp_seq        family_grps.grp_seq%TYPE;
      vn_barge_grp_seq      family_grps.grp_seq%TYPE;
      vv_tbi_ind            VARCHAR2 (1)                               := 'N';
      vn_dtu_seq            bk_rqsts.dtu_seq%TYPE;
      vv_bestoffer_ind      VARCHAR2 (1)                               := 'N';
      vn_bofr_amt           bk_rqst_dets.amount%TYPE;
      vn_swap_amt           bk_rqst_dets.amount%TYPE;
      vn_subs_amt           bk_rqst_dets.amount%TYPE;
      vn_chgd_amt           bk_rqst_dets.amount%TYPE; 
       --S195850_T1_T4
      vd_rqst_bk_date_ini   bk_rqsts.rqst_bk_date%type;  
   --
   -- PL/SQL Block
   BEGIN
     --
     -- 1. Evaluacion para metodo de subasta
     ------------------------------------------------------------------------------------------------
   -- SOS           Comentarios
   -- --------      --------------------------------------------------------------------------------
   -- S61519        Se evalua mediante la funcion F_CHK_BOOK_CHRG_BY_DIM si el cargo de booking
   -- S4332         Jdam 13-mar-2006
   --               Si la tabla pl*sql charge_tab retorna con registros significa que el booking
   --               es producto de una subasta, de lo contrario retorna la tabla vacia.
   -- S94501 Fase 2 Se añade el parámetro VD_BK_DATE en el llamado a F_GET_BOOKING
------------------------------------------------------------------------------------------------
      --S195850_T1_T4
      BEGIN
         SELECT  rqst_bk_date
           INTO vd_rqst_bk_date_ini
           FROM bk_rqsts
          WHERE seq = p_br;
      EXCEPTION
         WHEN OTHERS
         THEN
            vd_rqst_bk_date_ini := NULL;
      END;
        
      IF (vd_rqst_bk_date_ini <= TO_DATE (sf_get_param (920, 'BOOKING'), 'DD-MON-YYYY')) OR
         ((vd_rqst_bk_date_ini > TO_DATE (sf_get_param (920, 'BOOKING'), 'DD-MON-YYYY')) AND p_action IS NULL)  THEN
      
        BEGIN
         --
         charge_tab := f_auction (p_mode, p_stat, p_br, p_cgn_seq, p_bid_amt);

         --
         IF charge_tab.COUNT > 0
         THEN
            RETURN (charge_tab);
         END IF;
         --
        END;
      END IF;
--
-- 2. Definicion de variables segun el metodo de ejecucion
------------------------------------------------------------------------------------------------
--    Comentarios : Se definen las variables segun el metodo de ejecución.
--                  C - Correction / S - Standard
------------------------------------------------------------------------------------------------
      BEGIN
         IF p_mode = 'C'
         THEN
            --
            vn_sin := p_sin_seq;
            vd_date := p_date;
            vv_hs_bp := p_hs_ind;
            vv_grand := p_gf_ind;
            vn_pcums := p_pcums;
            vn_gross := p_pcgross;
            vn_ondeck := p_ondeck;
            vv_stat := p_br_stat;
            vd_bk_date := p_br_bk_date;
            vd_stat_date := p_br_cancl_date;
            vv_arr_time_par := p_br_arr_time_par;
            vd_req_date := p_req_date;
            vn_swap_bs := p_bs_seq_swap;
            vn_swap_pc_ums := p_br_swap_pc_ums;
            vn_qty_teu := p_qty_teu;
            vv_vty_cd := p_vty_cd;
            --
            -- Added by JaDiaz on December 17, 2007 S61519
            vn_length := p_loa;
            vn_beam := p_extm_beam;
            vn_bp_id := f_gen_bp_id (vd_bk_date, vd_req_date);

            IF p_ind = 'O'
            THEN
               vn_dtu_seq := f_get_dtu_seq (p_mode, p_ind, p_ic_seq);
            ELSIF p_ind = 'T'
            THEN
               vn_dtu_seq := f_get_dtu_seq (p_mode, p_ind, p_cgn_seq);
            END IF;

            --
            vv_tbi_ind := f_get_tbi_ind (vn_dtu_seq, vv_vty_cd);
            vn_bofr_amt := p_bofr_amt;
            vn_swap_amt := p_swap_amt;
            vn_subs_amt := p_subs_amt;
            vn_chgd_amt := p_chgd_amt;
         --
         -- Modo Standard
         ELSIF p_mode = 'S'
         THEN
            --
            vn_br := p_br;
            vn_sin := f_get_sin (vn_br);
            vn_bs := f_get_bs_seq (vn_br);
            vd_date := sf_get_bill_date_par (vn_bs);
            vv_hs_bp := f_chk_if_hs (vn_br);
            vv_grand := f_chk_if_gf (vn_sin);
            vn_csn := sf_get_visit_no (vn_bs);
            cvc_table :=
               pkg_csn_vsl_chars.f_get_csn_chars ('V', vn_csn, vd_date, NULL);
            vn_pcums := NVL (p_pcums, cvc_table (cn_rec_idx).pcums_net_ton);
            vn_ondeck := cvc_table (cn_rec_idx).on_dek_tons;
            vn_gross := cvc_table (cn_rec_idx).pc_gross_tons;
            vn_length := cvc_table (cn_rec_idx).len_overall;
            vv_stat := f_get_br_stat (vn_br);      
            vd_bk_date := f_get_bk_date (vn_br);
            vd_stat_date := f_get_br_stat_date (vn_br);
            vv_arr_time_par := sf_get_arr_time_par (vn_bs, vv_stat);
            vd_req_date := f_get_br_req_date (vn_br);
            vn_swap_bs := f_get_swap_bs (vn_br);
            vv_vty_cd := sf_get_vsl_typ (vn_csn);
            vn_qty_teu :=
                  pkg_toll_charges.f_get_bill_teu (vn_bs, vv_vty_cd, vd_date);
            --
            -- Added by JaDiaz on December 17, 2007 S61519
            vn_beam := cvc_table (cn_rec_idx).extm_beam;
            vn_bp_id := f_get_br_bp_id (vn_br);
            vn_dtu_seq := f_get_dtu_seq (p_mode, p_ind, vn_br);
            vv_tbi_ind := f_get_tbi_ind (vn_dtu_seq, vv_vty_cd);
            vn_swap_amt :=
               NVL
                  (pkg_booking.f_get_bk_rqst_dets
                                                (vn_br,
                                                 pkg_book_datadict.f_bkp_swap,
                                                 1
                                                ),
                   0
                  );
            vn_subs_amt :=
               NVL
                  (pkg_booking.f_get_bk_rqst_dets
                                                (vn_br,
                                                 pkg_book_datadict.f_bkp_subs,
                                                 1
                                                ),
                   0
                  );
            vn_chgd_amt :=
               NVL
                  (pkg_booking.f_get_bk_rqst_dets
                                                (vn_br,
                                                 pkg_book_datadict.f_bkp_chgd,
                                                 1
                                                ),
                   0
                  );
         --
         END IF;
      END;

--
-- 3. Evaluación del metodo de calculo de cargo de booking
------------------------------------------------------------------------------------------------
   -- SOS       Comentarios
   -- --------  --------------------------------------------------------------------------------
   -- S61519    Se evalua mediante la funcion F_CHK_BOOK_CHRG_BY_DIM si el cargo de booking
   --           debera ser generado en base a las dimensiones del buque (Length y Beam). De
   --           lo contrario el cargo sera generado de manera regular evaluando criterios
   --           como Pcums y Teu.
   --
   --           La función utiliza como referencia la fecha del request contra la fecha de
   --           efectividad definida para la implementacion del cobro basado en dimensiones.
------------------------------------------------------------------------------------------------
      IF NOT f_chk_bk_chrg_by_dim (vd_req_date)
      THEN
--
-- Regular booking charge evaluation.
-------------------------------------
         BEGIN
            --
            IF f_chk_bk_grace_prd (vd_req_date, vd_date)
            THEN
               vd_date := vd_req_date;

               --
               -- Tracker 4223, logica para obtener la  fecha requerida para buscar el certificado de pcums
               IF p_mode = 'S'
               THEN                          -- Cargo de casos operacionales.
                  --
                  BEGIN
                     --
                     BEGIN
                        SELECT sf_get_transit_date (bs_seq), rqst_bk_date,
                               stat_date, stat
                          INTO vd_trn_date, vd_rqst_bk_date,
                               vd_bk_stat_date, vv_bk_stat
                          FROM bk_rqsts
                         WHERE seq = p_br;
                     EXCEPTION
                        WHEN OTHERS
                        THEN
                           NULL;
                     END;

                     --
                     IF vv_bk_stat = 'CAN'
                     THEN
                        vd_pcums_cert_date := vd_bk_stat_date;
                     ELSIF     vv_bk_stat IN ('FOR', 'GRA', 'BKD')
                           AND vd_trn_date IS NOT NULL
                     THEN
                        vd_pcums_cert_date := vd_trn_date;
                     ELSIF     vv_bk_stat IN ('FOR', 'GRA', 'BKD')
                           AND vd_trn_date IS NULL
                     THEN
                        vd_pcums_cert_date := vd_rqst_bk_date;
                     END IF;
                  --
                  END;
               --
               ELSIF p_mode = 'C'
               THEN
                  --
                  vd_pcums_cert_date := p_date;            -- Modo correccion
               --
               END IF;

               --
               vn_pcums := f_get_prev_tons (vn_sin, vd_pcums_cert_date);
-- -- S1519  el primer parametro se esta inicializando arriba, S4223 logica de segundo parametro
            --
            END IF;

            --
            vd_eff_date := f_get_bk_chg_date (vv_msg_1, vv_msg_2);

            --
            IF vd_bk_date < vd_eff_date
            THEN
               --
               vd_date := vd_bk_date;
            --
            END IF;

            --
            vd_teu_date :=
               pkg_toll_charges.f_get_bill_eff_date ('TEU_4856',
                                                     '01-MAY-2005');

            --
            IF vd_bk_date < vd_teu_date
            THEN
               --
               vd_date := vd_bk_date;
            --
            END IF;

            -- TRACKER 5124
            vd_canc_eff_date := f_get_bk_canc_chg (vv_msg_3, vv_msg_4);

            --
            IF vn_swap_bs IS NOT NULL
            THEN
               IF vd_date >= vd_eff_date
               THEN
                  IF p_mode = 'S'
                  THEN
                     vn_swap_pc_ums := sf_get_bk_swap_net (vn_bs, vn_pcums);

                     IF vn_pcums >= vn_swap_pc_ums
                     THEN
                        vv_other_pc_ums := 'N';
                     ELSE
                        vn_pcums := vn_swap_pc_ums;
                        vv_other_pc_ums := 'Y';
                     END IF;
                  ELSIF p_mode = 'C'
                  THEN
                     IF vn_pcums >= vn_swap_pc_ums
                     THEN
                        vn_pcums := vn_swap_pc_ums;
                        vv_other_pc_ums := 'N';
                     ELSE
                        vv_other_pc_ums := 'Y';
                     END IF;
                  END IF;
               END IF;
            END IF;

            --
            IF NVL (vn_qty_teu, 0) = 0 AND p_mode = 'S' AND vv_vty_cd <> '07'
            THEN
               --
               BEGIN
                  SELECT 'NTT'
                    INTO vv_teu_srce
                    FROM toll_chrg_dets
                   WHERE bs_seq = vn_bs;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     vn_qty_teu :=
                        pkg_toll_charges.f_get_tot_teu_allow (vn_sin,
                                                              vd_date,
                                                              'ABOV'
                                                             );
                     vv_teu_srce := 'TAD';
               END;
            --
            END IF;

            --
            -- Jdam 2-mar-2007 buques grandfathers
            -- Este codigo debe permancer hasta tanto existan buques tipo grandfathers
            BEGIN
               --
               IF vv_grand = 'Y'
               THEN
                  --
                  BEGIN
                     --
                     vd_pcums_act_date := NULL;
                     vn_net_tons := 0;
                     vn_abov_dek := 0;
                     vn_blow_dek := 0;

                     --
                     SELECT NVL (cert_date, apprv_date),
                            NVL (vsl_net_tons, 0)
                       INTO vd_pcums_act_date,
                            vn_net_tons
                       FROM pc_ums_certs
                      WHERE sin_seq = vn_sin
                        AND stat_cd != 'D'
                        AND date_to >= TO_DATE ('31DEC4712', 'DDMONYYYY');
                  --
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        NULL;
                  END;

                  --
                  IF vd_pcums_act_date < vd_teu_date
                  THEN                            -- vd_teu_date = 01-may-2005
                     --
                     BEGIN
                        SELECT NVL (abov_dek_cont_cap, 0),
                               NVL (blow_dek_cont_cap, 0)
                          INTO vn_abov_dek,
                               vn_blow_dek
                          FROM vsl_cgo_caps
                         WHERE sin_seq = vn_sin AND active = 'Y';
                     EXCEPTION
                        WHEN OTHERS
                        THEN
                           NULL;
                     END;

                     --
                     IF vv_vty_cd = '07'
                     THEN
                        --
                        IF vn_abov_dek + vn_blow_dek > 0
                        THEN
                           vn_qty_teu := vn_abov_dek + vn_blow_dek;
                        END IF;
                     --
                     ELSIF vv_vty_cd <> '07'
                     THEN
                        --
                        IF vn_abov_dek > 0
                        THEN
                           vn_qty_teu := vn_abov_dek;
                        END IF;

                        --
                        IF vn_net_tons > 0
                        THEN
                           vn_pcums := vn_net_tons;
                        END IF;
                     --
                     END IF;
                  --
                  END IF;
               -- IF vd_pcums_act_date < vd_teu_date THEN -- vd_teu_date = 01-may-2005
                       --
               END IF;                                 -- IF vv_grand='Y' THEN
            --
            END;

            --
            -- Added by JaDiaz on June 8, 2007 S28465
            BEGIN
               --
               -- Se evalua si se trata de un buque de desplazamiento
               IF pkg_acp_cert_util.f_is_displacement (vv_vty_cd)
               THEN
                  --
                  -- Si la fecha de reservacion no es nula
                  IF vd_bk_date IS NOT NULL
                  THEN
                     --
                     -- Se define la fecha de en que sera basado el cargo
                     vd_chrg_date :=
                        NVL (sf_get_transit_date (vn_bs),
                             sf_get_bill_date_par (vn_bs)
                            );

                     --
                     -- Si la fecha sobre la cual el cargo es basado es mayor a la fecha parametro y el certificado esta
                     -- aprobado y vigente para la fecha
                     IF     vd_chrg_date >=
                                        TO_DATE (sf_get_param (20, 'BILLING'))
                        AND pkg_acp_cert_util.f_is_cert_approve (vn_sin,
                                                                 vd_chrg_date
                                                                )
                     THEN
                        --
                        -- Se obtiene el valor tonelaje de desplazamiento maximo del certificado
                        vn_full_ld_disp :=
                           pkg_acp_cert_util.f_get_full_ld_disp (vn_sin,
                                                                 vd_chrg_date
                                                                );
                        --
                        -- Se obtiene el factor de conversion del DISPLACEMENT a PCUMS
                        vn_disp_ums_factor :=
                                   sf_get_tons_rigth_fee ('DST', vd_chrg_date);

                        --
                        -- Se evalua si el valor de tonelaje para desplazamiento maximo no es nulo y si
                        -- existe un factor de conversion a PCUMS
                        IF vn_full_ld_disp > 0 AND vn_disp_ums_factor > 0
                        THEN
                            --
                           -- Se realiza el calculo de conversion de DISPLACEMENT a PCUMS
                           vn_pcums :=
                                 ROUND (vn_full_ld_disp * vn_disp_ums_factor);
                        --
                        END IF;
                     --
                     END IF;                   -- Vd_bk_date mayor a parametro
                  --
                  END IF;                            -- vd_bk_date is not null
               --
               END IF;                                      -- Is Displacement
            --
            END;
         --
         END;
-----------------------------------------
-- End Regular booking charge evaluation.
--
      ELSE
         vd_eff_date := f_get_bk_chg_date (vv_msg_1, vv_msg_2);
         vd_canc_eff_date := f_get_bk_canc_chg (vv_msg_3, vv_msg_4);
         --
         -- NUEVO METODO: Se evalua si el booking es por un BEST OFFER
         charge_tab := f_best_offer (p_mode, p_br, vn_bofr_amt);

         IF charge_tab.COUNT > 0
         THEN
            vv_bestoffer_ind := 'Y';
         END IF;
      --
      END IF;

--
-- 4. Generación del cargo de booking
------------------------------------------------------------------------------------------------
   -- SOS       Comentarios
   -- --------  --------------------------------------------------------------------------------
   -- S18530    Se adiciono al llamado de la funcion F_GET_BOOKING el status del booking para no
   --           generar cargo a buques hibridos cuando es un FORFEITURE
   -- S61519    Se adicionaron los parametro de Length y Beam para la generación del cargo
   --           basado en las dimensiones del buque.
------------------------------------------------------------------------------------------------
      BEGIN
           --
           -- Se valida que la tabla de cargas este vacia, de lo contrario es porque la misma fue
         -- definida por una reservacion tipo BEST OFFER.
         pkg_plog.sec_implementation ('PKG_MB620000PR.F_GET_BEST_OFFER',
                                      'charge_tab.COUNT=' || charge_tab.COUNT,
                                      'INFO'
                                     );

         IF charge_tab.COUNT = 0
         THEN
            --
            charge_tab :=
               f_get_booking (vv_hs_bp,
                              vv_grand,
                              vn_pcums,
                              vn_gross,
                              vn_ondeck,
                              vd_date,
                              vn_length,
                              vd_eff_date,
                              vn_qty_teu,
                              vv_vty_cd,
                              vd_teu_date,
                              vv_stat,
                              vd_req_date,
                              vn_beam,
                              vn_bp_id,
                              vv_tbi_ind,
                              --S94501 Fase 2 JDArosemena
                              vd_bk_date
                              --
                             );
         --
         END IF;
      --
      END;

--
-- 5. Evaluacion post-Generacion
------------------------------------------------------------------------------------------------
      BEGIN
-------------------------------
-- Cancellation Fee evaluation.
-------------------------------
         IF p_stat = 'CF'
         THEN
 --
 -- Regular booking charge evalautions.
 ------------------------------------------------------------------------------------
 -- Como el cargo de booking basado en dimensiones es generado en su totalidad dentro
-- de la funcion F_GET_BOOKING, esta sección solo aplicaria para cuando el cargo es
-- generado bajo el metodo anterior.
            IF vd_stat_date < pkg_book_datadict.f_bill_eff_date_v1
            THEN
               --
               -- Tracker xxxx
               vn_days :=
                    TO_DATE (   TO_CHAR (vd_bk_date, 'DD-MON-YYYY')
                             || ' '
                             || NVL (vv_arr_time_par, '0200'),
                             'DD-MON-YYYY HH24MI'
                            )
                  - vd_stat_date;
               vn_hours :=
                    (  TO_DATE (   TO_CHAR (vd_bk_date, 'DD-MON-YYYY')
                                || ' '
                                || NVL (vv_arr_time_par, '0200'),
                                'DD-MON-YYYY HH24MI'
                               )
                     - TO_DATE (vd_stat_date, 'DD-MON-YYYY HH24MI')
                    )
                  * 24;
               vn_summ := 0;
               vv_desc := 'CANCELLATION BOOKING FEE ';

               --
               FOR x IN 1 .. charge_tab.COUNT
               LOOP
                  IF charge_tab (x).unit = 'UMS'
                  THEN
                     --
                     vv_desc :=
                           vv_desc
                        || charge_tab (x).qty
                        || ' PC/UMS NET TONS $'
                        || charge_tab (x).rate
                        || ' PER TON ';
                  --
                  ELSIF charge_tab (x).unit = 'PGR'
                  THEN
                     --
                     vv_desc :=
                           vv_desc
                        || charge_tab (x).qty
                        || ' PC/GROSS $'
                        || charge_tab (x).rate
                        || ' PER TON ';
                  --
                  ELSIF charge_tab (x).unit = 'MDC'
                  THEN
                     --
                     vv_desc :=
                           vv_desc
                        || ' + '
                        || charge_tab (x).qty
                        || ' ON-DECK TONS $'
                        || charge_tab (x).rate
                        || ' PER TON ';
                  --
                  ELSIF charge_tab (x).unit = 'TEU'
                  THEN
--
--------------------------------------------------------------------------
-- Modified by JaDiaz on April 23, 2007 S33318
-- vv_vsl_descr := UPPER(Sf_Get_Vsl_Typ_D(vn_csn));
                     vv_vsl_descr := UPPER (sf_get_vessel_descr (vv_vty_cd));

--------------------------------------------------------------------------
--
                     IF vv_vty_cd = '07'
                     THEN
                        --
                        vv_desc :=
                              vv_desc
                           || charge_tab (x).qty
                           || ' TEU X '
                           || charge_tab (x).rate
                           || ' PER TEU ';
                     --
                     ELSE
                        charge_tab (x).amount := 0;
                     --
                     END IF;
                  --
                  END IF;

                  --
                  vn_summ := vn_summ + NVL (charge_tab (x).amount, 0);
               --
               END LOOP;

               pkg_plog.sec_implementation
                                         ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                             'ANTES DE VN_SUMM = CERO'
                                          || CHR (10)
                                          || 'vn_summ='
                                          || vn_summ
                                          || CHR (10)
                                          || 'cn_rec_idx='
                                          || cn_rec_idx,
                                          'INFO'
                                         );

               --
               IF vn_summ = 0
               THEN
                  --
                  charge_tab (cn_rec_idx).qty := NULL;
                  charge_tab (cn_rec_idx).unit := NULL;
                  charge_tab (cn_rec_idx).description := vv_desc;
                  charge_tab (cn_rec_idx).amount := NVL (vn_summ, 0);
                  charge_tab (cn_rec_idx).rate := NULL;
                  charge_tab (cn_rec_idx).item_no := NULL;
                  --
                  RETURN charge_tab;
               --
               END IF;

               pkg_plog.sec_implementation
                                   ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                    'SI VN_SUMM = CERO NO LLEGA A ESTE PUNTO',
                                    'INFO'
                                   );
               --
               cancel_tab :=
                  f_fill_cancel_table (vn_summ,
                                       vd_date,
                                       vv_arr_time_par,
                                       vd_eff_date,
                                       vd_canc_eff_date,
                                       vd_stat_date,
                                       vv_stat,
                                       vd_bk_date,
                                       vd_req_date,
                                       'N'
                                      );
               --
               charge_tab.DELETE;
               x := 1;
               --
               -- fill in charge pl/sql table
               charge_tab (x).eff_date := vd_stat_date;
               --
               -- Change QTY and UNIT to NULL for final input to the table
               charge_tab (x).qty := NULL;
               charge_tab (x).unit := NULL;

               --
               IF vd_stat_date < vd_canc_eff_date
               THEN
                  --
                  IF vn_days BETWEEN 31.00000 AND 365.99999
                  THEN
                     --
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := cancel_tab (1).amount;
                     charge_tab (x).item_no := cancel_tab (1).tar_no;

                     --
                     IF charge_tab (x).amount < cancel_tab (1).min_amount
                     THEN
                        --
                        charge_tab (x).amount := cancel_tab (1).min_amount;
                        charge_tab (x).item_no := cancel_tab (1).tar_min;
                     --
                     END IF;

                     --
                     y := 1;
                  --
                  ELSIF vn_days BETWEEN 22.00000 AND 30.99999
                  THEN
                     --
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := cancel_tab (2).amount;
                     charge_tab (x).item_no := cancel_tab (2).tar_no;

                     --
                     IF charge_tab (x).amount < cancel_tab (2).min_amount
                     THEN
                        --
                        charge_tab (x).amount := cancel_tab (2).min_amount;
                        charge_tab (x).item_no := cancel_tab (2).tar_min;
                     --
                     END IF;

                     --
                     y := 2;
                  --
                  ELSIF vn_days BETWEEN 4.00000 AND 21.99999
                  THEN
                     --
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := cancel_tab (3).amount;
                     charge_tab (x).item_no := cancel_tab (3).tar_no;

                     --
                     IF charge_tab (x).amount < cancel_tab (3).min_amount
                     THEN
                        --
                        charge_tab (x).amount := cancel_tab (3).min_amount;
                        charge_tab (x).item_no := cancel_tab (3).tar_min;
                     --
                     END IF;

                     --
                     y := 3;
                  --
                  ELSIF vn_days <= 3.99999
                  THEN
                     --
                     IF vd_date < vd_eff_date
                     THEN
                        --
                        IF vn_hours > 8
                        THEN
                           --
                           charge_tab (x).rate := NULL;
                           charge_tab (x).amount := cancel_tab (4).amount;
                           charge_tab (x).item_no := cancel_tab (4).tar_no;

                           --
                           IF charge_tab (x).amount <
                                                    cancel_tab (4).min_amount
                           THEN
                              --
                              charge_tab (x).amount :=
                                                    cancel_tab (4).min_amount;
                              charge_tab (x).item_no :=
                                                       cancel_tab (4).tar_min;
                           --
                           END IF;

                           --
                           y := 4;
                        --
                        ELSIF vn_hours <= 8
                        THEN
                           --
                           charge_tab (x).rate := NULL;
                           charge_tab (x).amount := cancel_tab (5).amount;
                           charge_tab (x).item_no := cancel_tab (5).tar_no;

                           --
                           IF charge_tab (x).amount <
                                                    cancel_tab (5).min_amount
                           THEN
                              --
                              charge_tab (x).amount :=
                                                    cancel_tab (5).min_amount;
                              charge_tab (x).item_no :=
                                                       cancel_tab (5).tar_min;
                           --
                           END IF;

                           --
                           y := 5;
                        --
                        END IF;
                     --
                     ELSIF vd_date >= vd_eff_date
                     THEN
                        --
                        IF vn_hours > 36
                        THEN
                           --
                           charge_tab (x).rate := NULL;
                           charge_tab (x).amount := cancel_tab (4).amount;
                           charge_tab (x).item_no := cancel_tab (4).tar_no;

                           --
                           IF charge_tab (x).amount <
                                                    cancel_tab (4).min_amount
                           THEN
                              --
                              charge_tab (x).amount :=
                                                    cancel_tab (4).min_amount;
                              charge_tab (x).item_no :=
                                                       cancel_tab (4).tar_min;
                           --
                           END IF;

                           --
                           y := 4;
                        --
                        ELSIF vn_hours <= 36
                        THEN
                           --
                           charge_tab (x).rate := NULL;
                           charge_tab (x).amount := cancel_tab (5).amount;
                           charge_tab (x).item_no := cancel_tab (5).tar_no;

                           --
                           IF charge_tab (x).amount <
                                                    cancel_tab (5).min_amount
                           THEN
                              --
                              charge_tab (x).amount :=
                                                    cancel_tab (5).min_amount;
                              charge_tab (x).item_no :=
                                                       cancel_tab (5).tar_min;
                           --
                           END IF;

                           --
                           y := 5;
                        --
                        END IF;
                     --
                     END IF;
                  --
                  END IF;
               --
               ELSIF vd_stat_date >= vd_canc_eff_date
               THEN
                  --
                  IF vn_hours >= 720
                  THEN
                     --
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := cancel_tab (1).amount;
                     charge_tab (x).item_no := cancel_tab (1).tar_no;

                     --
                     IF charge_tab (x).amount < cancel_tab (1).min_amount
                     THEN
                        --
                        charge_tab (x).amount := cancel_tab (1).min_amount;
                        charge_tab (x).item_no := cancel_tab (1).tar_min;
                     --
                     END IF;

                     --
                     y := 1;
                  --
                  ELSIF vn_hours > 504 AND vn_hours <= 720
                  THEN
                     --
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := cancel_tab (2).amount;
                     charge_tab (x).item_no := cancel_tab (2).tar_no;

                     --
                     IF charge_tab (x).amount < cancel_tab (2).min_amount
                     THEN
                        --
                        charge_tab (x).amount := cancel_tab (2).min_amount;
                        charge_tab (x).item_no := cancel_tab (2).tar_min;
                     --
                     END IF;

                     --
                     y := 2;
                  --
                  ELSIF vn_hours > 72 AND vn_hours <= 504
                  THEN
                     --
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := cancel_tab (3).amount;
                     charge_tab (x).item_no := cancel_tab (3).tar_no;

                     --
                     IF charge_tab (x).amount < cancel_tab (3).min_amount
                     THEN
                        --
                        charge_tab (x).amount := cancel_tab (3).min_amount;
                        charge_tab (x).item_no := cancel_tab (3).tar_min;
                     --
                     END IF;

                     --
                     y := 3;
                  --
                  ELSIF vn_hours >= 36 AND vn_hours <= 72
                  THEN
                     --
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := cancel_tab (4).amount;
                     charge_tab (x).item_no := cancel_tab (4).tar_no;

                     --
                     IF charge_tab (x).amount < cancel_tab (4).min_amount
                     THEN
                        --
                        charge_tab (x).amount := cancel_tab (4).min_amount;
                        charge_tab (x).item_no := cancel_tab (4).tar_min;
                     --
                     END IF;

                     --
                     y := 4;
                  --
                  ELSIF vn_hours < 36
                  THEN
                     --
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := cancel_tab (5).amount;
                     charge_tab (x).item_no := cancel_tab (5).tar_no;

                     --
                     IF charge_tab (x).amount < cancel_tab (5).min_amount
                     THEN
                        --
                        charge_tab (x).amount := cancel_tab (5).min_amount;
                        charge_tab (x).item_no := cancel_tab (5).tar_min;
                     --
                     END IF;

                     --
                     y := 5;
                  --
                  END IF;
               --
               END IF;

               --
               vv_desc :=
                     vv_desc
                  || ' X '
                  || cancel_tab (y).PERCENT
                  || '% - TRANSIT '
                  || vd_bk_date;
               charge_tab (x).description := vv_desc;
            --

            --
 -- Booking charge by dimmension evaluations.
 ------------------------------------------------------------------------------------
 -- Nueva logica implementada para el proceso de cancelacion de reservaciones basadas
-- en el nuevo metodo generado en base a las dimensiones del buque.
            ELSE
               --
               --
               IF charge_tab IS NOT NULL AND charge_tab.COUNT > 0
               THEN
                  vv_desc := 'CANCELLATION BOOKING FEE ';

                  --
                  -- Totaliza el monto de los cargos de booking, en caso de que sean más de uno.
                  FOR x IN 1 .. charge_tab.COUNT
                  LOOP
                     --
                     IF charge_tab (x).unit = 'UMS'
                     THEN
                        --
                        vv_desc :=
                              vv_desc
                           || charge_tab (x).qty
                           || ' PC/UMS NET TONS $'
                           || charge_tab (x).rate
                           || ' PER TON ';
                     --
                     ELSIF charge_tab (x).unit = 'PGR'
                     THEN
                        --
                        vv_desc :=
                              vv_desc
                           || charge_tab (x).qty
                           || ' PC/GROSS $'
                           || charge_tab (x).rate
                           || ' PER TON ';
                     --
                     ELSIF charge_tab (x).unit = 'MDC'
                     THEN
                        --
                        vv_desc :=
                              vv_desc
                           || ' + '
                           || charge_tab (x).qty
                           || ' ON-DECK TONS $'
                           || charge_tab (x).rate
                           || ' PER TON ';
                     --
                     ELSIF charge_tab (x).unit = 'TEU'
                     THEN
                        vv_vsl_descr :=
                                      UPPER (sf_get_vessel_descr (vv_vty_cd));

                        IF vv_vty_cd = '07'
                        THEN
                           --
                           vv_desc :=
                                 vv_desc
                              || charge_tab (x).qty
                              || ' TEU X '
                              || charge_tab (x).rate
                              || ' PER TEU ';
                        --
                        ELSE
                           --
                           -- No se acumula el monto de un cargo basado en TEU para buques Hibridos cuando es cancelacion
                           charge_tab (x).amount := 0;
                        --
                        END IF;
                     --
                     END IF;

                     --
                     vn_summ :=
                              NVL (vn_summ, 0)
                              + NVL (charge_tab (x).amount, 0);
                  END LOOP;

                  --
                  IF NVL (vn_summ, 0) < NVL (vn_swap_amt, 0)
                  THEN
                     vn_summ := vn_swap_amt;
                  END IF;

                  --
                  IF NVL (vn_summ, 0) < NVL (vn_subs_amt, 0)
                  THEN
                     vn_summ := vn_subs_amt;
                  END IF;

                  --
                  IF NVL (vn_summ, 0) < NVL (vn_chgd_amt, 0)
                  THEN
                     vn_summ := vn_chgd_amt;
                  END IF;
               --
               ELSE
                  DBMS_OUTPUT.put_line
                     ('*** Tabla de cargos [charge_tab] es nula o está vacía.'
                     );
               END IF;

               pkg_plog.sec_implementation
                                         ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                             'cancel_tab.COUNT='
                                          || cancel_tab.COUNT
                                          || CHR (10)
                                          || 'vn_summ='
                                          || vn_summ,
                                          'INFO'
                                         );
               --
               -- Solicita la generación del cargo por cancelación específico.
               cancel_tab :=
                  f_fill_cancel_table (vn_summ,
                                       vd_date,
                                       vv_arr_time_par,
                                       vd_eff_date,
                                       vd_canc_eff_date,
                                       vd_stat_date,
                                       vv_stat,
                                       vd_bk_date,
                                       vd_req_date,
                                       'N'
                                      );
               --
               -- VJaen 23Ene2008
               -- Borra la tabla de cargos de booking, para llenarlas
               -- con los cargos por cancelación.  La tabla de cargos original
               -- purden contener 2 elementos.
               charge_tab.DELETE;
               pkg_plog.sec_implementation
                                         ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                             'cancel_tab.COUNT='
                                          || cancel_tab.COUNT,
                                          'INFO'
                                         );

               --
               --  Pasa los datos de la tabla de cancelación
               -- hacia la tabla de cargos, a ser retornada.
               IF cancel_tab IS NOT NULL AND cancel_tab.COUNT > 0
               THEN
                  --
                  -- En caso de que a futuro se generen más de un cargo por cancelación
                  -- se pasar registro por registro a la tabla de cargos.
                  FOR i IN 1 .. cancel_tab.COUNT
                  LOOP
                     charge_tab (i).rate := NULL;
                     charge_tab (i).amount := cancel_tab (i).amount;
                     charge_tab (i).item_no := cancel_tab (i).tar_no;

                     --
                     IF charge_tab (i).amount < cancel_tab (i).min_amount
                     THEN
                        --
                        charge_tab (i).amount := cancel_tab (i).min_amount;
                        charge_tab (i).item_no := cancel_tab (i).tar_min;
                     --
                     END IF;

                     --
                     charge_tab (i).acg_cd := cancel_tab (i).acg_cd;
                     --
                     -- fill in charge pl/sql table
                     charge_tab (i).eff_date := vd_stat_date;
                     --
                     -- Change QTY and UNIT to NULL for final input to the table
                     charge_tab (i).qty := NULL;
                     charge_tab (i).unit := NULL;

                     --
                     -- Booking fee basado en un best offer
                     IF vv_bestoffer_ind = 'Y'
                     THEN
                        charge_tab (i).description :=
                              'CANCELLATION BOOKING FEE - BEST OFFER - '
                           || TO_CHAR (cancel_tab (i).amount, '$999,999.00')
                           || ' X '
                           || cancel_tab (i).PERCENT
                           || '% - TRANSIT '
                           || TO_CHAR (vd_bk_date, 'DD-MON-RRRR');
                     ELSE
                              --
                        -- Booking fee basado en el metodo anterior 01-feb-2008
                        IF NOT f_chk_bk_chrg_by_dim (vd_req_date)
                        THEN
                           --
                           vv_desc :=
                                 vv_desc
                              || ' X '
                              || cancel_tab (i).PERCENT
                              || '% - TRANSIT '
                              || TO_CHAR (vd_bk_date, 'DD-MON-RRRR');
                           charge_tab (i).description := vv_desc;
                        --

                        --
                        -- Booking fee basado en el metodo nuevo posterior al 01-feb-2008
                        ELSE
                           --
                           IF cancel_tab (i).tar_min IS NOT NULL
                           THEN
                              charge_tab (i).description :=
                                 f_display_chrg_desc
                                         (cancel_tab (i).min_description,
                                          TO_CHAR (cancel_tab (i).min_amount,
                                                   '$999,999.00'
                                                  ),
                                          TO_CHAR (vd_bk_date, 'DD-MON-RRRR')
                                         );
                           ELSE
                              charge_tab (i).description :=
                                 f_display_chrg_desc
                                                  (cancel_tab (i).description,
                                                   TO_CHAR (vn_summ,
                                                            '$999,999.00'
                                                           ),
                                                   TO_CHAR (vd_bk_date,
                                                            'DD-MON-RRRR'
                                                           )
                                                  );
                           END IF;
                        --
                        END IF;
                     END IF;
                  --
                  END LOOP;
               END IF;
            END IF;
--------------------------
-- Booking Fee evaluation.
--------------------------
         ELSIF p_stat = 'BF'
         THEN
 --
 -- Regular booking charge evalautions.
 ------------------------------------------------------------------------------------
 -- Como el cargo de booking basado en dimensiones es generado en su totalidad dentro
-- de la funcion F_GET_BOOKING, esta sección solo aplicaria para cuando el cargo es
-- generado bajo el metodo anterior.
            IF NOT f_chk_bk_chrg_by_dim (vd_req_date)
            THEN
               --
               IF p_ind = 'O'
               THEN
                  --
                  vn_summ := 0;

                  --
                  IF vv_stat = 'FOR'
                  THEN
                     --
                     vv_desc := 'FORFEITURE OF BOOKING FEE ';
                  --
                  ELSIF vv_stat IN ('GRA', 'NGRA RQST')
                  THEN
                     --
                     vv_desc := 'TRANSIT BOOKING FEE ';
                  --
                  END IF;

                  --
                  FOR x IN 1 .. charge_tab.COUNT
                  LOOP
                     --
                     IF charge_tab (x).unit = 'UMS'
                     THEN
                        --
                        IF vd_date < vd_eff_date
                        THEN
                           vv_tar_no := '1050.0010';
                        ELSIF vd_date >= vd_eff_date
                        THEN
                           vv_tar_no := '1050.0210';
                        END IF;

                        --
                        vv_desc :=
                              vv_desc
                           || charge_tab (x).qty
                           || ' PC/UMS NET TONS X '
                           || charge_tab (x).rate
                           || ' PER TON ';
                     --
                     ELSIF charge_tab (x).unit = 'PGR'
                     THEN
                        --
                        vv_tar_no := '1050.0015';
                        vv_desc :=
                              vv_desc
                           || charge_tab (x).qty
                           || ' PC/GROSS X '
                           || charge_tab (x).rate
                           || ' PER TON ';
                     --
                     ELSIF charge_tab (x).unit = 'MDC'
                     THEN
                        --
                        vv_tar_no := '1050.0015';
                        vv_desc :=
                              vv_desc
                           || ' + '
                           || charge_tab (x).qty
                           || ' ON-DECK TONS X '
                           || charge_tab (x).rate
                           || ' PER TON ';
                     --
                     ELSIF charge_tab (x).unit = 'TEU'
                     THEN
                        --
                        vv_tar_no := '1050.0210';
--------------------------------------------------------------------------
-- Modified by JaDiaz on April 23, 2007 S33318
-- vv_vsl_descr := UPPER(Sf_Get_Vsl_Typ_D(vn_csn));
                        vv_vsl_descr :=
                                      UPPER (sf_get_vessel_descr (vv_vty_cd));

--------------------------------------------------------------------------
--
                        IF vv_vty_cd = '07'
                        THEN
                           --
                           vv_desc :=
                                 vv_desc
                              || charge_tab (x).qty
                              || ' TEU X '
                              || charge_tab (x).rate
                              || ' PER TEU ('
                              || vv_vsl_descr
                              || ')';
                        --
                        ELSE
                           --
                           IF vv_teu_srce = 'TAD'
                           THEN
                              --
                              vv_desc :=
                                    vv_desc
                                 || ' + '
                                 || charge_tab (x).qty
                                 || ' TEU ABOVE DECK X '
                                 || charge_tab (x).rate
                                 || ' PER TEU ('
                                 || vv_vsl_descr
                                 || ')';
                           --
                           ELSE
                              --
                              vv_desc :=
                                    vv_desc
                                 || ' + '
                                 || charge_tab (x).qty
                                 || ' TEU X '
                                 || charge_tab (x).rate
                                 || ' PER TEU ('
                                 || vv_vsl_descr
                                 || ')';
                           --
                           END IF;
                        --
                        END IF;
                     --
                     ELSE
                        --
                        vv_tar_no := charge_tab (x).item_no;
                     --
                     END IF;

                     --
                     vn_summ := vn_summ + NVL (charge_tab (x).amount, 0);
                  --
                  END LOOP;

                  --
                  IF vn_swap_bs IS NOT NULL
                  THEN
                     --
                     IF vd_date >= vd_eff_date
                     THEN
                        --
                        IF vv_other_pc_ums = 'Y'
                        THEN
                           vv_desc :=
                                 vv_desc
                              || ' '
                              || 'BASED ON TONNAGE OF SWAPPING SIN: '
                              || sf_get_ship_no (sf_get_visit_no (vn_swap_bs));
                        END IF;
                     --
                     END IF;
                  --
                  END IF;

                  --
                  charge_tab.DELETE;
                  charge_tab (cn_rec_idx).rate := NULL;
                  charge_tab (cn_rec_idx).amount := vn_summ;
                  charge_tab (cn_rec_idx).item_no := vv_tar_no;
                  charge_tab (cn_rec_idx).eff_date := vd_bk_date;
                  charge_tab (cn_rec_idx).description := vv_desc;
                  charge_tab (cn_rec_idx).unit := NULL;
                  charge_tab (cn_rec_idx).qty := NULL;
               --
               ELSIF p_ind = 'T'
               THEN
                  --
                  IF vv_stat = 'FOR'
                  THEN
                     --
                     FOR x IN 1 .. charge_tab.COUNT
                     LOOP
                        charge_tab (x).description :=
                                                 'FORFEITURE OF BOOKING FEE ';
                     END LOOP;
                  --
                  END IF;
               --
               END IF;
            ELSE
               --
               -- Booking fee basado en un best offer
               IF vv_bestoffer_ind = 'Y'
               THEN
                  charge_tab (cn_rec_idx).eff_date := vd_req_date;
               ELSE
                  charge_tab (cn_rec_idx).eff_date := vd_bk_date;
               END IF;

               --
               -- Totaliza el monto de los cargos de booking, en caso de que sean más de uno.
               FOR x IN 1 .. charge_tab.COUNT
               LOOP
                  --
                  vn_summ := NVL (vn_summ, 0)
                             + NVL (charge_tab (x).amount, 0);
               --
               END LOOP;

               --
               -- S15142.T20
               -- VJaen 02-Jul-2008
               -- Reemplazo del monto del cargo, en caso de que el monto de Swap/Subs/Chgd sea mayor.
               --
               IF NVL (vn_summ, 0) < NVL (vn_swap_amt, 0)
               THEN
                  vn_summ := vn_swap_amt;
                  charge_tab (cn_rec_idx).amount := vn_summ;
                  charge_tab (cn_rec_idx).rate   := vn_summ;
                  charge_tab (cn_rec_idx).item_no:= F_get_tar_no_by_amount(trunc(vd_req_date)
                                                                          ,charge_tab (cn_rec_idx).item_no
                                                                          ,charge_tab (cn_rec_idx).amount
                                                                          ,vv_stat
                                                                          ,vn_bp_id);
               END IF;

               --
               IF NVL (vn_summ, 0) < NVL (vn_subs_amt, 0)
               THEN
                  vn_summ := vn_subs_amt;
                  charge_tab (cn_rec_idx).amount := vn_summ;
                  charge_tab (cn_rec_idx).rate   := vn_summ;
                  charge_tab (cn_rec_idx).item_no:= F_get_tar_no_by_amount(trunc(vd_req_date)
                                                                          ,charge_tab (cn_rec_idx).item_no
                                                                          ,charge_tab (cn_rec_idx).amount
                                                                          ,vv_stat
                                                                          ,vn_bp_id);
               END IF;

               --
               IF NVL (vn_summ, 0) < NVL (vn_chgd_amt, 0)
               THEN
                  vn_summ := vn_chgd_amt;
                  charge_tab (cn_rec_idx).amount := vn_summ;
                  charge_tab (cn_rec_idx).rate   := vn_summ;
                  charge_tab (cn_rec_idx).item_no:= F_get_tar_no_by_amount(trunc(vd_req_date)
                                                                          ,charge_tab (cn_rec_idx).item_no
                                                                          ,charge_tab (cn_rec_idx).amount
                                                                          ,vv_stat
                                                                          ,vn_bp_id);
               END IF;

            --
            -- Pendiente evaluar manejo en caso tal que el buque tenga mas de un cargo (PCUMS / TEU)
            --
            END IF;
         --
         END IF;
-----------------------------------
-- Transaction mode evaluation end.
--
      END;

--
-- 6. Retorno del cargo de booking generado y definido en la tabla pl/sql
------------------------------------------------------------------------------------------------
--
      RETURN charge_tab;
   --
   END f_get_book_charges;

   FUNCTION f_auction                                -- S4332 Jdam 10-mar-2006
                     (
      p_mode      IN   CHAR,      -- 'C' modo correccion; 'S' modo operacional
      p_stat      IN   VARCHAR2,
      -- 'BF' cargo por booking fee; 'CF' cargo por cancellation
      p_br_seq    IN   NUMBER,
      -- solo cuando es modo operacional se envia el sequence del bk_rqsts
      p_cgn_seq   IN   NUMBER,
      -- solo cuando es modo correccion se envia el cgn_seq
      p_bid_amt   IN   NUMBER                -- solo cuando es modo correccion
   )
      RETURN charge_table_type
   IS
/*

    Funcion que se encarga de verificar si el booking fue subastado, en caso afirmativo se retornar la tabla pl*sql con la informacion requerida, tanto para booking o cancelacion,
    esto dependera del parametro p_stat;

    En caso de que no sea un booking subastado retornara la tabla de pl*sql vacia.

*/
      charge_tab         charge_table_type;
      x                  NUMBER (1);
      vv_desc            VARCHAR2 (250);
-- Modo operacional
      vn_fix_amt         bk_rqsts.fix_amt%TYPE;
      vv_tar_no_bkd      tariffs.tar_no%TYPE;
      vv_tar_no_can      tariffs.tar_no%TYPE;
      vn_can_bid_rate    tariff_rates.tariff_rate%TYPE;
      vv_stat            bk_rqsts.stat%TYPE;
      vd_eff_date        DATE;
-- Modo correccion
      vn_ic_chrg_amt     interfaced_chrgs.chrg_amt%TYPE;
      vv_ic_tar_tar_no   interfaced_chrgs.tar_tar_no%TYPE;
      vd_ic_svc_date     interfaced_chrgs.svc_date%TYPE;

-- Este cursor se utiliza para modo correccion
      CURSOR c_correction_amt (p_cgn_seq NUMBER)
      IS
         SELECT   chrg_amt, tar_tar_no, svc_date
             FROM (SELECT chrg_amt, cgn.seq cgn_seq, ic.tar_tar_no,
                          ic.svc_date, ic.seq ic_seq    -- Para cuando es TOLL
                     FROM chrg_grp_nos cgn, interfaced_chrgs ic
                    WHERE cgn.seq = ic.cgn_seq AND chrg_typ = 'BOOKING'
                   UNION ALL
                   SELECT chrg_amt, cgn.seq cgn_seq, ic.tar_tar_no,
                          ic.svc_date, ic.seq ic_seq     -- Para cuando es OMS
                     FROM chrg_grp_nos cgn,
                          chrg_grp_assgs cga,
                          interfaced_chrgs ic
                    WHERE cgn.seq = cga.cgn_seq
                      AND ic.seq = cga.ic_seq
                      AND chrg_typ = 'BR')
            WHERE cgn_seq = p_cgn_seq
         ORDER BY ic_seq ASC;
   BEGIN
      charge_tab.DELETE;

      IF p_mode = 'S' AND p_br_seq IS NOT NULL
      THEN
         BEGIN
            vd_eff_date := sf_get_bill_date_par (f_get_bs_seq (p_br_seq));

            SELECT fix_amt, stat
              INTO vn_fix_amt, vv_stat
              FROM bk_rqsts
             WHERE seq = p_br_seq;
         EXCEPTION
            WHEN OTHERS
            THEN
               pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);
         END;

         IF vv_stat = 'FOR'
         THEN
            vv_desc := 'FORFEITURE OF BOOKING FEE AUCTION ';
         ELSIF vv_stat IN ('CAN', 'VCAN RQST')
         THEN
            vv_desc := 'CANCELLATION BOOKING FEE AUCTION ';
         ELSE
            vv_desc := 'TRANSIT BOOKING FEE AUCTION ';
         END IF;

         IF NVL (vn_fix_amt, 0) > 0 AND p_stat = 'BF'
            AND vd_eff_date IS NOT NULL
         THEN                             -- modo operacional, Fee por subasta
            BEGIN
               SELECT tar_no
                 INTO vv_tar_no_bkd
                 FROM tariffs
                WHERE CLASS = 'BIDBKD';
            EXCEPTION
               WHEN NO_DATA_FOUND
               THEN
                  raise_application_error (-20000, 'USR-00660');
               WHEN TOO_MANY_ROWS
               THEN
                  raise_application_error (-20000, 'USR-00661');
               WHEN OTHERS
               THEN
                  pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);
            END;

            IF vv_tar_no_bkd IS NOT NULL
            THEN
               x := 1;
               charge_tab (x).rate := vn_fix_amt;
               charge_tab (x).amount := vn_fix_amt;
               charge_tab (x).item_no := vv_tar_no_bkd;
               charge_tab (x).eff_date := vd_eff_date;
               charge_tab (x).description := vv_desc;
               charge_tab (x).unit := 'UNI';
               charge_tab (x).qty := 1;

               --
               -- Added by JaDiaz on November 17, 2011 S158688_T33
               charge_tab (x).acg_cd := Sf_get_acc_grp(vv_tar_no_bkd,'STD',NULL,NULL,NULL,NULL,NULL,NULL,vd_eff_date);

            END IF;
         ELSIF     NVL (vn_fix_amt, 0) > 0
               AND p_stat = 'CF'
               AND vv_stat = 'CAN'
               AND vd_eff_date IS NOT NULL
         THEN                      -- modo operacional, Cancelacion de subasta
            BEGIN
               -- Buscar el rate de cancelacion
               SELECT tariff_rate, tar_no
                 INTO vn_can_bid_rate, vv_tar_no_can
                 FROM tariffs tar, tariff_rates tr
                WHERE tar.tar_no = tr.tar_tar_no
                  AND tar.CLASS = 'BIDCAN'
                  AND vd_eff_date BETWEEN tr.date_from AND tr.date_to;
            EXCEPTION
               WHEN NO_DATA_FOUND
               THEN
                  raise_application_error (-20000, 'USR-00662');
               WHEN TOO_MANY_ROWS
               THEN
                  raise_application_error (-20000, 'USR-00663');
               WHEN OTHERS
               THEN
                  pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);
            END;

            IF vv_tar_no_can IS NOT NULL AND vn_can_bid_rate IS NOT NULL
            THEN
               x := 1;
               charge_tab (x).rate := vn_fix_amt * (vn_can_bid_rate / 100);
               charge_tab (x).amount := vn_fix_amt * (vn_can_bid_rate / 100);
               charge_tab (x).item_no := vv_tar_no_can;
               charge_tab (x).eff_date := vd_eff_date;
               charge_tab (x).description := vv_desc;
               charge_tab (x).unit := 'UNI';
               charge_tab (x).qty := 1;

               --
               -- Added by JaDiaz on November 17, 2011 S158688_T33
               charge_tab (x).acg_cd := Sf_get_acc_grp(vv_tar_no_can,'STD',NULL,NULL,NULL,NULL,NULL,NULL,vd_eff_date);

            END IF;
         END IF;                               -- FIN, si del modo operacional
      ELSIF p_mode = 'C' AND p_cgn_seq IS NOT NULL
      THEN
         BEGIN
            SELECT tar_no
              INTO vv_tar_no_bkd
              FROM tariffs
             WHERE CLASS = 'BIDBKD';
         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
               raise_application_error (-20000, 'USR-00660');
            WHEN TOO_MANY_ROWS
            THEN
               raise_application_error (-20000, 'USR-00661');
            WHEN OTHERS
            THEN
               pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);
         END;

         FOR r_correction_amt IN c_correction_amt (p_cgn_seq)
         LOOP
            vn_ic_chrg_amt := r_correction_amt.chrg_amt;
            vv_ic_tar_tar_no := r_correction_amt.tar_tar_no;
            vd_ic_svc_date := r_correction_amt.svc_date;
         END LOOP;

         IF vn_ic_chrg_amt <> p_bid_amt AND p_bid_amt IS NOT NULL
         THEN
            vn_ic_chrg_amt := p_bid_amt;
         END IF;      -- Cuando cambian el monto del bid a nivel de correccion

         IF     vn_ic_chrg_amt IS NOT NULL
            AND vv_ic_tar_tar_no = vv_tar_no_bkd
            AND vv_tar_no_bkd IS NOT NULL
         THEN
            x := 1;
            charge_tab (x).rate := vn_ic_chrg_amt;
            charge_tab (x).amount := vn_ic_chrg_amt;
            charge_tab (x).item_no := vv_tar_no_bkd;
            charge_tab (x).eff_date := vd_ic_svc_date;
            charge_tab (x).description := 'TRANSIT BOOKING FEE AUCTION ';
            charge_tab (x).unit := 'UNI';
            charge_tab (x).qty := 1;

            --
            -- Added by JaDiaz on November 17, 2011 S158688_T33
            charge_tab (x).acg_cd := Sf_get_acc_grp(vv_tar_no_bkd,'STD',NULL,NULL,NULL,NULL,NULL,NULL,vd_ic_svc_date);

         END IF;
      END IF;

      RETURN charge_tab;
   EXCEPTION
      WHEN OTHERS
      THEN
         pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);
   END f_auction;

   FUNCTION f_gen_bp_id (p_rqst_bk_date IN DATE, p_rqst_date IN DATE)
      RETURN bk_pers.ID%TYPE
   IS
--
-- Local Declarations
      vn_bp_id   bk_pers.ID%TYPE;
   BEGIN
      BEGIN
         SELECT ID
           INTO vn_bp_id
           FROM bk_pers
          WHERE (p_rqst_bk_date - TRUNC (p_rqst_date)) BETWEEN date_from
                                                           AND date_to;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            --
            -- There is not an open period for this booking date.
            raise_application_error (-20000, 'USR-30062');
         --
         WHEN OTHERS
         THEN
            --
            pkg_evtms_db_util.p_generic_exception (SQLCODE, SQLERRM);
      --
      END;

      RETURN vn_bp_id;
   END f_gen_bp_id;

   FUNCTION f_get_br_bp_id (p_br IN NUMBER)
      RETURN NUMBER
   IS
-- ======================================================================================================
-- DESCRIPCION:
--   Esta funcion se encarga de obtener el booking period de la reservacion original.
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
-- ======================================================================================================
-- HISTORIA DE MODIFICACIONES
-- SOS       Desarrollador   Fecha         Cambios realizados
-- --------- --------------  -----------   -------------------------------------------------------
-- S61519    JADIAZ          18-DEC-2007   Version Inicial
-- ======================================================================================================
--
-- Local Declarations
      vn_bp_id   NUMBER;
--PL/SQL Block
   BEGIN
      SELECT br.bp_id
        INTO vn_bp_id
        FROM bk_rqsts br
       WHERE br.seq = p_br;

      RETURN vn_bp_id;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN NULL;
   END f_get_br_bp_id;

   FUNCTION f_get_cancel_charges (
      p_br                IN   NUMBER,      -- Booking Request Sequence Number
      p_mode              IN   CHAR,
      -- Indicator of whether the fee a regular fee or a correction fee
      p_date              IN   DATE,
      -- billing date parameter (for correction)
      p_pcums             IN   NUMBER,      -- PCUMS net tons (for correction)
      p_pcgross           IN   NUMBER,        -- PCGROSS tons (for correction)
      p_ondeck            IN   NUMBER,        -- ON DECK tons (for correction)
      p_gf_ind            IN   VARCHAR2,
      -- Grand Father Indicator (for correction)
      p_hs_ind            IN   VARCHAR2,
      -- High Season Indicator (for correction)
      p_br_stat           IN   VARCHAR2,
      -- Status of Booking Request (for correction)
      p_br_bk_date        IN   DATE,   -- Date for which the vessel was booked
      p_br_cancl_date     IN   DATE,
      -- Date the Cancellation (status) was received
      p_br_arr_time_par   IN   VARCHAR2,
      -- Arrival time parameter for the vessel
      p_req_date          IN   DATE DEFAULT NULL,
      -- Request Date of Booking (for correction)
      p_bs_seq_swap       IN   NUMBER DEFAULT NULL,
      -- Billing Set Sequence of Swapped Vessel (For correction)
      p_br_swap_pc_ums    IN   NUMBER DEFAULT NULL,
      -- PC_UMS of Swapped Vessel (For correction)
      p_qty_teu           IN   NUMBER DEFAULT NULL,
-- TEU Total allow del certificado para los 07 o NTT para los Hibridos  (For correction)
      p_vty_cd            IN   VARCHAR2 DEFAULT NULL,
      -- Vessel Type(For correction) -- S1519 Jdam 25-nov-2005
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
      RETURN cancel_table_type
   IS
      charge_tab            charge_table_type;
      cancel_tab            cancel_table_type;
      vn_hours              NUMBER (8, 2);
      x                     NUMBER;
      vn_summ               NUMBER (8, 2);
      vv_stat               bk_rqsts.stat%TYPE;
      vv_desc               charges.descr%TYPE;
      vd_date               DATE;
      vn_br                 bk_rqsts.seq%TYPE;
      vn_sin                ship_id_no.seq%TYPE;
      vn_bs                 bill_sets.seq%TYPE;
      vv_hs_bp              VARCHAR2 (1);
      vv_grand              VARCHAR2 (1);
      vn_csn                cust_sched_needs.seq%TYPE;
      cvc_table             pkg_csn_vsl_chars.csn_cvc_table;
      vn_pcums              NUMBER (5);
      vn_ondeck             NUMBER (5);
      vn_gross              NUMBER (5);
      vd_stat_date          bk_rqsts.stat_date%TYPE;
      vd_bk_date            bk_rqsts.rqst_bk_date%TYPE;
      vv_arr_time_par       VARCHAR2 (10);
      vn_length             cust_vsl_chars.len_overall%TYPE;
      vd_eff_date           DATE;
      vv_msg_1              VARCHAR2 (2000);
      vv_msg_2              VARCHAR2 (2000);
      vd_req_date           DATE;
      vn_swap_bs            bk_rqsts.bs_seq_swap%TYPE;
      vn_swap_pc_ums        pc_ums_certs.net_tons%TYPE;
      vv_other_pc_ums       VARCHAR2 (1);
-- Tracker 4856
      vn_qty_teu            pc_ums_certs.total_teu_allow%TYPE;
      vv_vty_cd             vsl_typs.cd%TYPE;
      vd_teu_date           DATE;
      n                     NUMBER (1);
--tRACKER 5124
      vv_msg_3              VARCHAR2 (2000);
      vv_msg_4              VARCHAR2 (2000);
-- S18227 Jdam 2-mar-2007
      vd_pcums_act_date     DATE;
      vn_net_tons           pc_ums_certs.vsl_net_tons%TYPE;
      vn_abov_dek           vsl_cgo_caps.abov_dek_cont_cap%TYPE;
      vn_blow_dek           vsl_cgo_caps.blow_dek_cont_cap%TYPE;
--
-- S61519 Added by JaDiaz on December 20, 2007
      vd_canc_eff_date      DATE    := f_get_bk_canc_chg (vv_msg_3, vv_msg_4);
      cn_rec_idx   CONSTANT NUMBER                                := 1;
      vn_beam               cust_vsl_chars.extm_beam%TYPE;
      vn_bp_id              bk_rqsts.bp_id%TYPE;
      vn_tug_grp_seq        family_grps.grp_seq%TYPE;
      vn_barge_grp_seq      family_grps.grp_seq%TYPE;
      vv_tbi_ind            VARCHAR2 (1)                          := 'N';
      vn_dtu_seq            bk_rqsts.dtu_seq%TYPE;
      vn_bofr_amt           bk_rqst_dets.amount%TYPE;
      vn_swap_amt           bk_rqst_dets.amount%TYPE;
      vn_subs_amt           bk_rqst_dets.amount%TYPE;
      vn_chgd_amt           bk_rqst_dets.amount%TYPE;
--
   BEGIN
--
-- 1. Definicion de variables segun el metodo de ejecucion
------------------------------------------------------------------------------------------------
--    Comentarios : Se definen las variables segun el metodo de ejecución.
--                  C - Correction / S - Standard
------------------------------------------------------------------------------------------------
      BEGIN
         IF p_mode = 'C'
         THEN
            vd_date := p_date;
            vv_hs_bp := p_hs_ind;
            vv_grand := p_gf_ind;
            vn_pcums := p_pcums;
            vn_gross := p_pcgross;
            vn_ondeck := p_ondeck;
            vv_stat := p_br_stat;
            vd_bk_date := p_br_bk_date;
            vd_stat_date := p_br_cancl_date;
            vv_arr_time_par := p_br_arr_time_par;
            vd_req_date := p_req_date;
            vn_swap_bs := p_bs_seq_swap;
            vn_swap_pc_ums := p_br_swap_pc_ums;
            vn_qty_teu := p_qty_teu;
            vv_vty_cd := p_vty_cd;
            --
            -- Added by JaDiaz on December 17, 2007 S61519
            vn_length := p_loa;
            vn_beam := p_extm_beam;
            vn_bp_id := f_gen_bp_id (vd_bk_date, vd_req_date);
            vn_bofr_amt := p_bofr_amt;
            vn_swap_amt := p_swap_amt;
            vn_subs_amt := p_subs_amt;
            vn_chgd_amt := p_chgd_amt;
         --
         ELSIF p_mode = 'S'
         THEN
            vn_br := p_br;
            vn_sin := f_get_sin (vn_br);
            vn_bs := f_get_bs_seq (vn_br);
            vd_date := sf_get_bill_date_par (vn_bs);
            vv_hs_bp := f_chk_if_hs (vn_br);
            vv_grand := f_chk_if_gf (vn_sin);
            vn_csn := sf_get_visit_no (vn_bs);
            cvc_table :=
               pkg_csn_vsl_chars.f_get_csn_chars ('V', vn_csn, vd_date, NULL);
            vn_pcums := cvc_table (cn_rec_idx).pcums_net_ton;
            vn_ondeck := cvc_table (cn_rec_idx).on_dek_tons;
            vn_length := cvc_table (cn_rec_idx).len_overall;
            vn_gross := cvc_table (cn_rec_idx).pc_gross_tons;
            vv_stat := f_get_br_stat (vn_br);
            vd_bk_date := f_get_bk_date (vn_br);
            vd_stat_date := f_get_br_stat_date (vn_br); 
            --S195850_T1_T4: se adiciona el parametro vv_stat para equiparar esta version con la de produccion
            vv_arr_time_par := sf_get_arr_time_par (vn_bs,vv_stat);
            vd_req_date := f_get_br_req_date (vn_br);
            vn_swap_bs := f_get_swap_bs (vn_br);
            vv_vty_cd := sf_get_vsl_typ (vn_csn);
            vn_qty_teu :=
                  pkg_toll_charges.f_get_bill_teu (vn_bs, vv_vty_cd, vd_date);
            --
            -- Added by JaDiaz on December 17, 2007 S61519
            vn_beam := cvc_table (cn_rec_idx).extm_beam;
            vn_bp_id := f_get_br_bp_id (vn_br);
            vn_dtu_seq := f_get_dtu_seq (p_mode, NULL, p_br);
            vv_tbi_ind := f_get_tbi_ind (vn_dtu_seq, vv_vty_cd);
            vn_swap_amt :=
               NVL
                  (pkg_booking.f_get_bk_rqst_dets
                                                (vn_br,
                                                 pkg_book_datadict.f_bkp_swap,
                                                 1
                                                ),
                   0
                  );
            vn_subs_amt :=
               NVL
                  (pkg_booking.f_get_bk_rqst_dets
                                                (vn_br,
                                                 pkg_book_datadict.f_bkp_subs,
                                                 1
                                                ),
                   0
                  );
            vn_chgd_amt :=
               NVL
                  (pkg_booking.f_get_bk_rqst_dets
                                                (vn_br,
                                                 pkg_book_datadict.f_bkp_chgd,
                                                 1
                                                ),
                   0
                  );
         --
         END IF;
      END;

--
-- 2. Evaluación del metodo de calculo de cargo de booking
------------------------------------------------------------------------------------------------
   -- SOS       Comentarios
   -- --------  --------------------------------------------------------------------------------
   -- S61519    Se evalua mediante la funcion F_CHK_BOOK_CHRG_BY_DIM si el cargo de booking
   --           debera ser generado en base a las dimensiones del buque (Length y Beam). De
   --           lo contrario el cargo sera generado de manera regular evaluando criterios
   --           como Pcums y Teu.
   --
   --           La función utiliza como referencia la fecha del request contra la fecha de
   --           efectividad definida para la implementacion del cobro basado en dimensiones.
------------------------------------------------------------------------------------------------
      DBMS_OUTPUT.put_line ('Vd_req_date ' || vd_req_date);

      IF NOT f_chk_bk_chrg_by_dim (vd_req_date)
      THEN
--
-- Regular booking charge evaluation.
-------------------------------------
         IF f_chk_bk_grace_prd (vd_req_date, vd_date)
         THEN
            vd_date := vd_req_date;
            vn_pcums :=
                       f_get_prev_tons (vn_sin, sf_get_bill_date_par (vn_bs));
         END IF;

         vd_eff_date := f_get_bk_chg_date (vv_msg_1, vv_msg_2);

         IF vd_bk_date < vd_eff_date
         THEN
            vd_date := vd_bk_date;
         END IF;

         vd_teu_date :=
              pkg_toll_charges.f_get_bill_eff_date ('TEU_4856', '01-MAY-2005');

         IF vd_bk_date < vd_teu_date
         THEN
            vd_date := vd_bk_date;
         END IF;

         IF vn_swap_bs IS NOT NULL
         THEN
            IF vd_date >= vd_eff_date
            THEN
               IF p_mode = 'S'
               THEN
                  vn_swap_pc_ums := sf_get_bk_swap_net (vn_bs, vn_pcums);

                  IF vn_pcums >= vn_swap_pc_ums
                  THEN
                     vv_other_pc_ums := 'N';
                  ELSE
                     vn_pcums := vn_swap_pc_ums;
                     vv_other_pc_ums := 'Y';
                  END IF;
               ELSIF p_mode = 'C'
               THEN
                  IF vn_pcums >= vn_swap_pc_ums
                  THEN
                     vn_pcums := vn_swap_pc_ums;
                     vv_other_pc_ums := 'N';
                  ELSE
                     vv_other_pc_ums := 'Y';
                  END IF;
               END IF;
            END IF;
         END IF;

         IF NVL (vn_qty_teu, 0) = 0 AND p_mode = 'S' AND vv_vty_cd <> '07'
         THEN
            BEGIN
               SELECT 1
                 INTO n
                 FROM toll_chrg_dets
                WHERE bs_seq = vn_bs;
            EXCEPTION
               WHEN NO_DATA_FOUND
               THEN
                  vn_qty_teu :=
                     pkg_toll_charges.f_get_tot_teu_allow (vn_sin,
                                                           vd_date,
                                                           'ABOV'
                                                          );
            END;
         END IF;

         -- Jdam 2-mar-2007 buques grandfathers
         -- Este codigo debe permancer hasta tanto existan buques tipo grandfathers
         BEGIN
            IF vv_grand = 'Y'
            THEN
               BEGIN
                  vd_pcums_act_date := NULL;
                  vn_net_tons := 0;
                  vn_abov_dek := 0;
                  vn_blow_dek := 0;

                  SELECT NVL (cert_date, apprv_date), NVL (vsl_net_tons, 0)
                    INTO vd_pcums_act_date, vn_net_tons
                    FROM pc_ums_certs
                   WHERE sin_seq = vn_sin
                     AND stat_cd != 'D'
                     AND date_to >= TO_DATE ('31DEC4712', 'DDMONYYYY');
               EXCEPTION
                  WHEN OTHERS
                  THEN
                     NULL;
               END;

               IF vd_pcums_act_date < vd_teu_date
               THEN                               -- vd_teu_date = 01-may-2005
                  BEGIN
                     SELECT NVL (abov_dek_cont_cap, 0),
                            NVL (blow_dek_cont_cap, 0)
                       INTO vn_abov_dek,
                            vn_blow_dek
                       FROM vsl_cgo_caps
                      WHERE sin_seq = vn_sin AND active = 'Y';
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        NULL;
                  END;

                  IF vv_vty_cd = '07'
                  THEN
                     IF vn_abov_dek + vn_blow_dek > 0
                     THEN
                        vn_qty_teu := vn_abov_dek + vn_blow_dek;
                     END IF;
                  ELSIF vv_vty_cd <> '07'
                  THEN
                     IF vn_abov_dek > 0
                     THEN
                        vn_qty_teu := vn_abov_dek;
                     END IF;

                     IF vn_net_tons > 0
                     THEN
                        vn_pcums := vn_net_tons;
                     END IF;
                  END IF;
               END IF;
            -- IF vd_pcums_act_date < vd_teu_date THEN -- vd_teu_date = 01-may-2005
            END IF;                                    -- IF vv_grand='Y' THEN
         END;
-----------------------------------------
-- End Regular booking charge evaluation.
--
      ELSE
         vd_eff_date := f_get_bk_chg_date (vv_msg_1, vv_msg_2);
         --
         -- NUEVO METODO: Se evalua si el booking es por un BEST OFFER
         charge_tab := f_best_offer (p_mode, p_br, 0);
      --
      END IF;

--
-- 3. Generación del cargo de booking
------------------------------------------------------------------------------------------------
   -- SOS       Comentarios
   -- --------  --------------------------------------------------------------------------------
   -- S18530    Se adiciono al llamado de la funcion F_GET_BOOKING el status del booking para no
   --           generar cargo a buques hibridos cuando es un FORFEITURE
   -- S61519    Se adicionaron los parametro de Length y Beam para la generación del cargo
   --           basado en las dimensiones del buque.
------------------------------------------------------------------------------------------------

      -- S18530 Se adiciono al llamado de la funcion F_GET_BOOKING el status del booking para no
      --        generar cargo a buques hibridos cuando es un FORFEITURE
      BEGIN
           --
           -- Se valida que la tabla de cargas este vacia, de lo contrario es porque la misma fue
         -- definida por una reservacion tipo BEST OFFER.
         IF charge_tab.COUNT = 0
         THEN
            --
            charge_tab :=
               f_get_booking (vv_hs_bp,
                              vv_grand,
                              vn_pcums,
                              vn_gross,
                              vn_ondeck,
                              vd_date,
                              vn_length,
                              vd_eff_date,
                              vn_qty_teu,
                              vv_vty_cd,
                              vd_teu_date,
                              vv_stat,
                              vd_req_date,
                              vn_beam,
                              vn_bp_id,
                              vv_tbi_ind
                             );
         --
         END IF;
      --
      END;

--
-- 4. Evaluacion post-Generacion
------------------------------------------------------------------------------------------------
      vn_summ := 0;

      FOR x IN 1 .. charge_tab.COUNT
      LOOP
         -- Tracker 5407
         -- JDArosemena 08-25-2005
         IF (    vv_stat = 'CAN'
             AND charge_tab (x).unit = 'TEU'
             AND vv_vty_cd <> '07'
            )
         THEN
            vn_summ := vn_summ;
         ELSE
            vn_summ := vn_summ + NVL (charge_tab (x).amount, 0);
         END IF;

         --

         --
         IF vn_swap_bs IS NOT NULL
         THEN
            IF vd_date >= vd_eff_date
            THEN
               IF vv_other_pc_ums = 'Y'
               THEN
                  charge_tab (x).description :=
                        charge_tab (x).description
                     || ' '
                     || 'BASED ON TONNAGE OF SWAPPING SIN: '
                     || sf_get_ship_no (sf_get_visit_no (vn_swap_bs));
               END IF;
            END IF;
         END IF;
      --
      END LOOP;

      --
      IF NVL (vn_summ, 0) < NVL (vn_swap_amt, 0)
      THEN
         vn_summ := vn_swap_amt;
      END IF;

      --
      IF NVL (vn_summ, 0) < NVL (vn_subs_amt, 0)
      THEN
         vn_summ := vn_subs_amt;
      END IF;

      --
      IF NVL (vn_summ, 0) < NVL (vn_chgd_amt, 0)
      THEN
         vn_summ := vn_chgd_amt;
      END IF;

      --

      --
      -- cancel_tab := F_FILL_CANCEL_TABLE(vn_summ,vd_Date,vv_arr_time_par);
      -- Tracker 3706
      cancel_tab :=
         f_fill_cancel_table (vn_summ,
                              vd_date,
                              vv_arr_time_par,
                              vd_eff_date,
                              vd_canc_eff_date,
                              vd_stat_date,
                              vv_stat,
                              vd_bk_date,
                              vd_req_date,
                              'Y'
                             );

      IF vn_summ = 0
      THEN
         FOR x IN 1 .. cancel_tab.COUNT
         LOOP
            cancel_tab (x).amount := 0;
            cancel_tab (x).min_amount := 0;
         END LOOP;
      END IF;

      --
      RETURN cancel_tab;
   --
   END f_get_cancel_charges;

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
      RETURN charge_table_type
   IS
-- PL/SQL Specification
-- Program Data
      vn_hours                     NUMBER (8, 2);
      x                            NUMBER (1);
      vv_tar_no                    VARCHAR2 (12);
      vn_rate                      NUMBER (8, 2);
      vn_summ                      NUMBER (8, 2);
      vd_stat_date                 DATE;
      charge_tab                   charge_table_type;
      cancel_tab                   cancel_table_type;
      vv_hs_bp                     VARCHAR2 (1);
      vv_grand                     VARCHAR2 (1);
      vn_pcums                     NUMBER (5);
      vn_ondeck                    NUMBER (5);
      vn_gross                     NUMBER (5);
      vn_length                    cust_vsl_chars.len_overall%TYPE;
      vd_date                      DATE;
      vd_eff_date                  DATE;
      vd_teu_date                  DATE;
      vd_for_eff_date              DATE;
      --S94501 Fase 2 JDArosemena
      vd_bk_date                   DATE;
      --
      --S18530 Fecha de efectividad para forfeiture
--
-- Local Declarations
      cn_chrg_qty         CONSTANT NUMBER                                := 1;
      cv_unit_beam_cd     CONSTANT tariff_category_mappings.range1_unit%TYPE
                                            := pkg_book_datadict.f_runit_beam;
      cv_unit_loa_cd      CONSTANT tariff_category_mappings.range2_unit%TYPE
                                             := pkg_book_datadict.f_runit_loa;
      cv_std_tar_grp_cd   CONSTANT tariff_acc_grp_assgs.tar_grp%TYPE
                                           := pkg_book_datadict.f_tar_grp_std;
      cv_for_tar_grp_cd   CONSTANT tariff_acc_grp_assgs.tar_grp%TYPE
                                       := pkg_book_datadict.f_tar_grp_forfeit;
      vv_tar_grp                   tariff_acc_grp_assgs.tar_grp%TYPE
                                                         := cv_std_tar_grp_cd;
      vd_rqst_date                 bk_rqsts.rqst_date%TYPE;
--
-- PL/SQL Block
   BEGIN
      --
      x := 1;
      vv_hs_bp := p_hs;
      vv_grand := p_gr;
      vn_pcums := p_pcums;
      vn_gross := p_pcgross;
      vn_ondeck := p_ondeck;
      vd_date := p_date;
      vn_length := p_len;
      vd_eff_date := p_eff_date;
      vd_teu_date := p_teu_date;
      vd_rqst_date := p_req_date;
      --S94501 Fase 2 JDArosemena
      vd_bk_date  := p_bk_date;
      --

      --
-- Regular booking charge evalautions.
------------------------------------------------------------------------------------
-- Esta sección solo aplicaria para cuando el cargo es generado bajo el metodo
-- anterior y no en base a las dimensiones del buque.
      IF NOT f_chk_bk_chrg_by_dim (vd_rqst_date)
      THEN
         pkg_plog.sec_implementation ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                      'METODO ANTERIOR',
                                      'INFO'
                                     );

         --
         IF vv_hs_bp = 'Y'
         THEN     -- IF HIGH SEASON WHETHER IT IS A GRAND FATHER VESSEL OR NOT
            --
            IF vd_date < p_eff_date
            THEN
               --
               vv_tar_no := '1050.0100';
            --
            ELSIF vd_date >= p_eff_date
            THEN
               --
               vv_tar_no := '1050.0300';
            --
            END IF;

            --
            IF    vd_date < vd_teu_date
               OR (vd_date >= vd_teu_date AND p_vty_cd <> '07')
            THEN
               --
               vn_rate := sf_get_eff_rate (vv_tar_no, vd_date);
               --
               charge_tab (x).eff_date := vd_date;
               charge_tab (x).qty := NVL (vn_pcums, 0);
               charge_tab (x).unit := 'UMS';
               charge_tab (x).description := 'TRANSIT BOOKING FEE (PREMIUM)';
               charge_tab (x).rate := vn_rate;
               charge_tab (x).amount := NVL (vn_rate * vn_pcums, 0);
               charge_tab (x).item_no := vv_tar_no;
            --
            END IF;

            --
            -- TRACKER 4856 TEU
            IF (NVL (p_qty_teu, 0) > 0) AND (vd_date >= vd_teu_date)
            THEN
               --
               IF charge_tab.COUNT > 0
               THEN
                  --
                  x := x + 1;
               --
               ELSE
                  --
                  charge_tab.DELETE;
               --
               END IF;

               --
               vv_tar_no := '1050.0301';
               vn_rate := sf_get_eff_rate (vv_tar_no, vd_date);

               --
               IF p_vty_cd = '07'
               THEN
                  charge_tab (x).description :=
                         'FULL CONTAINER-TEU - TRANSIT BOOKING FEE - PREMIUM';
               ELSE
                  charge_tab (x).description :=
                                  'OTHER-TEU - TRANSIT BOOKING FEE - PREMIUM';
               END IF;

               --
               charge_tab (x).eff_date := vd_date;
               charge_tab (x).qty := NVL (p_qty_teu, 0);
               charge_tab (x).unit := 'TEU';
               charge_tab (x).rate := vn_rate;
               charge_tab (x).amount := NVL (vn_rate * p_qty_teu, 0);
               charge_tab (x).item_no := vv_tar_no;
            --
            END IF;

              --
            -- Check if the resulting fee is less than minimum
            IF vd_date < p_eff_date
            THEN
               --
               vv_tar_no := '1050.0110';
            --
            ELSIF vd_date >= p_eff_date
            THEN
               --
               vv_tar_no := '1050.0310';
            --
            END IF;

            --
            vn_rate := sf_get_eff_rate (vv_tar_no, vd_date);
            vn_summ := 0;

            --
            FOR x IN 1 .. charge_tab.COUNT
            LOOP
               --
               vn_summ := vn_summ + NVL (charge_tab (x).amount, 0);
            --
            END LOOP;

            --
            IF vn_rate > vn_summ
            THEN                       -- if fee IS less than MINIMUM FEE THEN
               --
               IF    vd_date < vd_teu_date
                  OR (vd_date >= vd_teu_date AND p_vty_cd <> '07')
               THEN
                  --
                  IF (   (NVL (vn_pcums, 0) = 0 AND NVL (vn_length, 0) <= 300)
                      OR NVL (vn_pcums, 0) > 0
                     )
                  THEN
                     --
                     charge_tab.DELETE;
                     x := 1;
                     charge_tab (x).eff_date := vd_date;
                     charge_tab (x).qty := NULL;
                     charge_tab (x).unit := NULL;
                     charge_tab (x).description :=
                                      'MINIMUM TRANSIT BOOKING FEE (PREMIUM)';
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := vn_rate;
                     charge_tab (x).item_no := vv_tar_no;
                  --
                  END IF;
               --
               ELSIF vd_date >= vd_teu_date AND p_vty_cd = '07'
               THEN
                  --
                  IF    (NVL (p_qty_teu, 0) = 0 AND NVL (vn_length, 0) <= 300
                        )
                     OR NVL (p_qty_teu, 0) > 0
                  THEN
                     charge_tab.DELETE;
                     x := 1;
                     charge_tab (x).eff_date := vd_date;
                     charge_tab (x).qty := NULL;
                     charge_tab (x).unit := NULL;
                     charge_tab (x).description :=
                                      'MINIMUM TRANSIT BOOKING FEE (PREMIUM)';
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := vn_rate;
                     charge_tab (x).item_no := vv_tar_no;
                  END IF;
               --
               END IF;
            --
            END IF;
         --
         ELSE
            --
            IF (vd_date < vd_eff_date)
            THEN                               -- HASTA ANTES DEL 01-jan-2004
               --
               vv_tar_no := '1050.0010';
               vn_rate := sf_get_eff_rate (vv_tar_no, vd_date);
            --
            ELSIF    (vd_date >= vd_eff_date AND vd_date < vd_teu_date)
                  OR              -- 01-jan-2004   HASTA ANTES DEL 01-may-2005
                     (vd_date >= vd_teu_date AND p_vty_cd <> '07'
                     )
            THEN                                      -- 01-may-2005 O DESPUES
               --
               vv_tar_no := '1050.0210';
               vn_rate := sf_get_eff_rate (vv_tar_no, vd_date);
            --
            END IF;

            -- Tracker 4223
            IF NVL (vv_tar_no, '-1') <> '-1' AND NVL (vn_rate, -1) <> -1
            THEN
               --
               charge_tab.DELETE;
               x := 1;
               charge_tab (x).eff_date := vd_date;
               charge_tab (x).qty := NVL (vn_pcums, 0);
               charge_tab (x).unit := 'UMS';
               charge_tab (x).description := 'TRANSIT BOOKING FEE';
               charge_tab (x).rate := vn_rate;
               charge_tab (x).amount := NVL (vn_rate * vn_pcums, 0);
               charge_tab (x).item_no := vv_tar_no;
            END IF;

            IF charge_tab IS NOT NULL AND charge_tab.COUNT >= 1
            THEN
               pkg_plog.sec_implementation
                                        ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                            'METODO ANTERIOR '
                                         || 'charge_tab (x).amount='
                                         || charge_tab (x).amount
                                         || CHR (10)
                                         || 'NVL (vn_pcums, 0)='
                                         || NVL (vn_pcums, 0)
                                         || CHR (10)
                                         || 'vn_rate='
                                         || vn_rate,
                                         'INFO'
                                        );
            ELSE
               pkg_plog.sec_implementation
                                        ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                            'METODO ANTERIOR '
                                         || 'No se genero tabla pl/sql A',
                                         'INFO'
                                        );
            END IF;

            --
            -- Fin de asignacion de cargo para booking basado en UMS

            -- TRACKER 4856 TEU
            BEGIN
               SELECT f_get_for_eff_date
                 --S18530 Fecha de efectividad para forfeiture
               INTO   vd_for_eff_date
                 FROM DUAL;
            END;

            IF charge_tab IS NOT NULL AND charge_tab.COUNT >= 1
            THEN
               pkg_plog.sec_implementation
                                        ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                            'METODO ANTERIOR '
                                         || 'charge_tab (x).amount='
                                         || charge_tab (x).amount
                                         || CHR (10)
                                         || 'NVL (p_qty_teu, 0)='
                                         || NVL (p_qty_teu, 0)
                                         || CHR (10)
                                         || 'vn_rate='
                                         || vn_rate,
                                         'INFO'
                                        );
            ELSE
               pkg_plog.sec_implementation
                                        ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                            'METODO ANTERIOR '
                                         || 'No se genero tabla pl/sql B',
                                         'INFO'
                                        );
            END IF;

            --
            IF     (NVL (p_qty_teu, 0) > 0)
               AND (vd_date >= vd_teu_date)
               AND (NOT (    p_bk_stat IN ('FOR', 'FOR RQST')
                         AND p_vty_cd <> '07'
                         AND vd_date > vd_for_eff_date
                        )
                   )
-- S18530 Se adiciono el status del booking para no generar cargo a buques hibridos cuando es un FORFEITURE
            THEN
               --
               IF charge_tab.COUNT > 0
               THEN
                  --
                  x := x + 1;
               --
               END IF;

               --
               vv_tar_no := '1050.0211';
               vn_rate := sf_get_eff_rate (vv_tar_no, vd_date);

               --
               IF p_vty_cd = '07'
               THEN
                  charge_tab (x).description :=
                                   'FULL CONTAINER-TEU - TRANSIT BOOKING FEE';
               ELSE
                  charge_tab (x).description :=
                                            'OTHER-TEU - TRANSIT BOOKING FEE';
               END IF;

               --
               charge_tab (x).eff_date := vd_date;
               charge_tab (x).qty := NVL (p_qty_teu, 0);
               charge_tab (x).unit := 'TEU';
               charge_tab (x).rate := vn_rate;
               charge_tab (x).amount := NVL (vn_rate * p_qty_teu, 0);
               charge_tab (x).item_no := vv_tar_no;
            --
            END IF;

            --
            -- Check if the resulting fee is less than minimum
            IF vd_date >= vd_eff_date
            THEN
               --
               vv_tar_no := '1050.0230';
            --
            ELSIF vd_date < vd_eff_date
            THEN
               --
               vv_tar_no := '1050.0030';
            --
            END IF;

            --
            vn_rate := sf_get_eff_rate (vv_tar_no, vd_date);
            vn_summ := 0;

            --
            FOR x IN 1 .. charge_tab.COUNT
            LOOP
               --
               vn_summ := vn_summ + NVL (charge_tab (x).amount, 0);
            --
            END LOOP;

            IF charge_tab IS NOT NULL AND charge_tab.COUNT >= 1
            THEN
               pkg_plog.sec_implementation
                                ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                    'METODO ANTERIOR EVALUACION DEL MINIMUM'
                                 || 'charge_tab (x).amount='
                                 || charge_tab (x).amount
                                 || CHR (10)
                                 || 'NVL (vn_summ, 0)='
                                 || NVL (vn_summ, 0)
                                 || CHR (10)
                                 || 'vn_rate='
                                 || vn_rate,
                                 'INFO'
                                );
            ELSE
               pkg_plog.sec_implementation
                                        ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                            'METODO ANTERIOR '
                                         || 'No se genero tabla pl/sql C',
                                         'INFO'
                                        );
            END IF;

            --
            IF vn_rate > vn_summ
            THEN                       -- if fee is less than MINIMUM FEE THEN
               --
               pkg_plog.sec_implementation
                                ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                    'METODO ANTERIOR EVALUACION DEL MINIMUM'
                                 || CHR (10)
                                 || 'vd_date='
                                 || vd_date
                                 || CHR (10)
                                 || 'vd_teu_date='
                                 || vd_teu_date
                                 || CHR (10)
                                 || 'NVL (vn_pcums, 0)='
                                 || NVL (vn_pcums, 0)
                                 || CHR (10)
                                 || 'NVL(vn_length, 0)='
                                 || NVL (vn_length, 0)
                                 || CHR (10)
                                 || 'p_vty_cd ='
                                 || p_vty_cd,
                                 'INFO'
                                );

               IF    vd_date < vd_teu_date
                  OR (vd_date >= vd_teu_date AND p_vty_cd <> '07')
               THEN
                  --
                  IF (   (NVL (vn_pcums, 0) = 0 AND NVL (vn_length, 0) <= 300)
                      OR NVL (vn_pcums, 0) > 0
                     )
                  THEN
                     --
                     charge_tab.DELETE;
                     x := 1;
                     charge_tab (x).eff_date := vd_date;
                     charge_tab (x).qty := NULL;
                     charge_tab (x).unit := NULL;
                     charge_tab (x).description :=
                                                'MINIMUM TRANSIT BOOKING FEE';
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := vn_rate;
                     charge_tab (x).item_no := vv_tar_no;
                  --
                  END IF;
               --
               ELSIF vd_date >= vd_teu_date AND p_vty_cd = '07'
               THEN
                  --
                  IF    (NVL (p_qty_teu, 0) = 0 AND NVL (vn_length, 0) <= 300
                        )
                     OR NVL (p_qty_teu, 0) > 0
                  THEN
                     --
                     charge_tab.DELETE;
                     x := 1;
                     charge_tab (x).eff_date := vd_date;
                     charge_tab (x).qty := NULL;
                     charge_tab (x).unit := NULL;
                     charge_tab (x).description :=
                                                'MINIMUM TRANSIT BOOKING FEE';
                     charge_tab (x).rate := NULL;
                     charge_tab (x).amount := vn_rate;
                     charge_tab (x).item_no := vv_tar_no;
                  END IF;
               --
               END IF;

               IF charge_tab IS NOT NULL AND charge_tab.COUNT >= 1
               THEN
                  pkg_plog.sec_implementation
                                ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                    'METODO ANTERIOR EVALUACION DEL MINIMUM'
                                 || 'charge_tab (x).amount='
                                 || charge_tab (x).amount
                                 || CHR (10)
                                 || 'charge_tab (x).description='
                                 || charge_tab (x).description,
                                 'INFO'
                                );
               ELSE
                  pkg_plog.sec_implementation
                                        ('PKG_MB620000PR.F_GET_BOOK_CHARGES',
                                            'METODO ANTERIOR '
                                         || 'No se genero tabla pl/sql D',
                                         'INFO'
                                        );
               END IF;
            --
            END IF;
         --
         END IF;
--
-- Booking by dimension charge evaluations.
------------------------------------------------------------------------------------
-- Esta sección solo aplicaria para cuando el cargo es generado en base a las
-- dimensiones del buque.
      ELSE
         --
         -- Inicializacion de variables y tablas de pl/sql
         charge_tab.DELETE;
         vt_tab.DELETE;

         --
         IF p_bk_stat = 'FOR'
         THEN
            vv_tar_grp := cv_for_tar_grp_cd;
         END IF;

         --
         -- Se valida el indicador de TUG BARGE INTEGRATED, ya que si este no es el caso se
         -- generara el cargo de forma regular
         IF p_tbi_ind = 'N'
         THEN
            --
            --SOS 94501 Fase II JDArosemena
                /*vt_tab :=
                    pkg_book_tcm_util.f_get_bkfix_by_range
                                               (TRUNC (vd_rqst_date),
                                                -- Request Date
                                                p_beam,        -- Extreme Beam
                                                cv_unit_beam_cd,
                                                -- Range1 Unit
                                                p_len,          -- Len Overall
                                                cv_unit_loa_cd, -- Range2 Unit
                                                p_bp_id,     -- Booking Period
                                                vv_tar_grp
                                               );*/
            --
            IF (pkg_cont_eval_util.f_eval_qry_ttt_impl('07',vd_bk_date) = 1 and p_bk_stat <> 'GRA')
                OR (pkg_cont_eval_util.f_eval_qry_ttt_impl('07',vd_bk_date) = 0)
                OR (pkg_cont_eval_util.f_eval_qry_ttt_impl('07',vd_bk_date) = 1 and p_beam < 91)    THEN
                vt_tab :=
                    pkg_book_tcm_util.f_get_bkfix_by_range
                                               (TRUNC (vd_rqst_date),
                                                -- Request Date
                                                p_beam,        -- Extreme Beam
                                                cv_unit_beam_cd,
                                                -- Range1 Unit
                                                p_len,          -- Len Overall
                                                cv_unit_loa_cd, -- Range2 Unit
                                                p_bp_id,     -- Booking Period
                                                vv_tar_grp,  -- Tar group
                                                --SOS 94501 Fase II JDArosemena
                                                'OTHER'--p_bk_stat     --Booking Status
                                                --
                                               );
            ELSIF (pkg_cont_eval_util.f_eval_qry_ttt_impl('07',vd_bk_date) = 1) and p_bk_stat = 'GRA' THEN
                vt_tab :=
                    pkg_book_tcm_util.f_get_bkfix_by_range
                                               (TRUNC (vd_bk_date),
                                                -- Transit_date
                                                p_beam,        -- Extreme Beam
                                                cv_unit_beam_cd,
                                                -- Range1 Unit
                                                p_len,          -- Len Overall
                                                cv_unit_loa_cd, -- Range2 Unit
                                                p_bp_id,     -- Booking Period
                                                vv_tar_grp,  -- Tar group
                                                --SOS 94501 Fase II JDArosemena
                                                p_bk_stat     --Booking Status
                                                --
                                               );
            END IF;
         --
         -- Si el indicador TUG BARGE INTEGRATED esta definido como cierto, entonces el cargo
         -- sera generado como minimo.
         ELSIF p_tbi_ind = 'Y'
         THEN
            --
            vt_tab :=
               pkg_book_tcm_util.f_get_bktbi_min (TRUNC (vd_rqst_date),
                                                  vv_tar_grp
                                                 );
         --
         END IF;

         --

         --
         -- Para el caso especifico de reservaciones basadas en las dimensiones del buque,
         -- la tabla de pl/sql solo deberia retornar 1 registro que contenga todos los elementos
         -- necesarios para la generacion del cargo.
         IF vt_tab IS NOT NULL
         THEN
            --
            FOR i IN 1 .. vt_tab.COUNT
            LOOP
               --
               vt_rec := vt_tab (i);

               --
               -- Si la tarifa no es nula, se definen los campos de la tabla pl/sql
               IF vt_rec.tar_no IS NOT NULL
               THEN
                  --
                  charge_tab (i).unit := 'UNI';
                  charge_tab (i).qty := cn_chrg_qty;
                  charge_tab (i).acg_cd := vt_rec.acg_cd;
                  charge_tab (i).item_no := vt_rec.tar_no;
                  charge_tab (i).rate := vt_rec.tar_rate;
                  charge_tab (i).amount :=
                                     charge_tab (i).rate * charge_tab (i).qty;
                  charge_tab (i).eff_date := vd_date;
                  charge_tab (i).description := vt_rec.tar_descr;
               --
               END IF;
            --
            END LOOP;
         --
         END IF;                                          -- vt_tab evaluation
      --
      END IF;

      --
      RETURN charge_tab;
   --
   END;

   FUNCTION f_get_for_eff_date
      RETURN DATE
   IS
      vd_for_eff_date   DATE;
--S18530 Fecha de efectividad para forfeiture
   BEGIN
      BEGIN
         SELECT TO_DATE (val, 'DD-MON-YYYY')
           INTO vd_for_eff_date
           FROM PARAMETERS
          WHERE typ_cd = 'FOR_EFF_DATE';
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      RETURN vd_for_eff_date;
   END f_get_for_eff_date;

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
      RETURN cancel_table_type
   IS
--============================================================================
-- DESCRIPCION:  Esta función se encarga de llenar la tabla de cargo por cancelación.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha       Cambios realizados
-- ------- -------------  ----------- -----------------------------------------
 -- S#####   ?????         ??-???-???? Version inicial.
--  S61519   VJaen         13-Dic-2007 Implementación de cambios por cancelación
--                                     de reservaciones, para ser cobrados por
--                                     medio de una tabla de rangos de porcentajes.
--           VJaen         09-Jan-2008 Se incluyen parámetros para fecha de booking,
--                                     fecha del request y el estado; para poder
--                                     evaluar contra la fecha de efectividad.
--  S65727   VJaen         20-Feb-2008 Se generalizó la lógica de asignación de valores
--                                     de tarifa mínima y se condicionó la limpieza
--                                     de la misma, cuando el amount es mayor el
--                                     min_amount y no se está pidiendo la lista
--                                     completa (p_return_all='N').
--                         21-Feb-2008 Se condicionó el reemplazo del amount por el
--                                     minimum amount, cuando éste último sea mayor;
--                                     para el metodo viejo, aun vigente.  Sólo lo
--                                     hace si se está solicitando la lista completa
--                                     de cargos (Evalua P_RETURN_ALL).
--=============================================================================
 --
      x                    NUMBER (1);
      vn_summ1             NUMBER;
      vd_date              DATE;
      vd_eff_date          DATE;
      cancel_tab           cancel_table_type;
      --
      -- S61519
      -- VJaen 05-Dec-2007
      vd_s61519_eff_date   DATE       := pkg_book_datadict.f_bill_eff_date_v1;
      vd_stat_date_fake    DATE;
   BEGIN
      DBMS_OUTPUT.put_line (   'f_fill_cancel_table - INICIO: '
                            || CHR (10)
                            || 'p_summ='
                            || p_summ
                            || CHR (10)
                            || 'p_date='
                            || p_date
                            || CHR (10)
                            || 'p_arr_time_par='
                            || p_arr_time_par
                            || CHR (10)
                            || 'p_eff_date='
                            || p_eff_date
                            || CHR (10)
                            || 'p_canc_eff_date='
                            || p_canc_eff_date
                            || CHR (10)
                            || 'p_stat_date='
                            || p_stat_date
                            || CHR (10)
                            || 'p_return_all='
                            || p_return_all
                           );
      pkg_plog.sec_implementation ('PKG_MB620000PR.F_FILL_CANCEL_TABLE',
                                      'p_summ='
                                   || p_summ
                                   || CHR (10)
                                   || 'p_date='
                                   || p_date
                                   || CHR (10)
                                   || 'p_arr_time_par='
                                   || p_arr_time_par
                                   || CHR (10)
                                   || 'p_eff_date='
                                   || p_eff_date
                                   || CHR (10)
                                   || 'p_canc_eff_date='
                                   || p_canc_eff_date
                                   || CHR (10)
                                   || 'p_stat_date='
                                   || p_stat_date
                                   || CHR (10)
                                   || 'p_br_stat='
                                   || NVL (p_br_stat, '**NULO**')
                                   || CHR (10)
                                   || 'p_rqst_bk_date='
                                   || NVL (TO_CHAR (p_rqst_bk_date,
                                                    'DD-MON-YYYY HH24MI'
                                                   ),
                                           '**NULO**'
                                          )
                                   || CHR (10)
                                   || 'p_rqst_date='
                                   || NVL (TO_CHAR (p_rqst_date,
                                                    'DD-MON-YYYY HH24MI'
                                                   ),
                                           '**NULO**'
                                          )
                                   || CHR (10)
                                   || 'p_return_all='
                                   || p_return_all
                                   || CHR (10)
                                   || 'vd_s61519_eff_date='
                                   || NVL (TO_CHAR (vd_s61519_eff_date,
                                                    'DD-MON-YYYY'
                                                   ),
                                           '**NULO**'
                                          ),
                                   'INFO'
                                  );

      --
      -- S61519
      -- VJaen 09-Jan-2008.
      -- Se incluyó esta evaluación, debido a que cuando el estado es Booked, la fecha de
      -- request a igual a la fecha del buque, por lo tanto siempre se va por la nueva tarifa,
      -- cuando en realidad se debe evaluar la fecha en que solicitó.
      --
      -- Evalua el estado de la reservación.
      IF NVL (p_br_stat, 'BKD') = 'BKD'
      THEN
         --
         -- Si aun está en estado Booked, se utiliza el sysdate para determinar la tarifa.
         vd_stat_date_fake := SYSDATE;
      ELSE
         --
         -- De no estar en estado Booked, se utiliza la fecha del estatus.
         vd_stat_date_fake := p_stat_date;
      END IF;

      --
      x := 1;
      vd_date := p_date;
      vd_eff_date := p_eff_date;
      vn_summ1 := p_summ;

      IF p_stat_date < p_canc_eff_date
      THEN
         IF vd_date < vd_eff_date
         THEN
            cancel_tab (x).date_from := 30.00001;
            cancel_tab (x).date_to := 365.00000;
            cancel_tab (x).tar_no := '1050.0050';
            cancel_tab (x).PERCENT :=
                             sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
            cancel_tab (x).amount :=
                                   (vn_summ1 * cancel_tab (x).PERCENT / 100
                                   );
            cancel_tab (x).tar_min := '1050.0055';
            cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

            IF cancel_tab (x).amount < cancel_tab (x).min_amount
            THEN
               cancel_tab (x).amount := cancel_tab (x).min_amount;
            END IF;

            x := x + 1;
            cancel_tab (x).date_from := 21.00001;
            cancel_tab (x).date_to := 30.00000;
            cancel_tab (x).tar_no := '1050.0060';
            cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
            cancel_tab (x).amount :=
                                    (vn_summ1 * cancel_tab (x).PERCENT / 100
                                    );
            cancel_tab (x).tar_min := '1050.0065';
            cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

            IF cancel_tab (x).amount < cancel_tab (x).min_amount
            THEN
               cancel_tab (x).amount := cancel_tab (x).min_amount;
            END IF;

            x := x + 1;
            cancel_tab (x).date_from := 3.00001;
            cancel_tab (x).date_to := 21.00000;
            cancel_tab (x).tar_no := '1050.0070';
            cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
            cancel_tab (x).amount :=
                                    (vn_summ1 * cancel_tab (x).PERCENT / 100
                                    );
            cancel_tab (x).tar_min := '1050.0075';
            cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

            IF cancel_tab (x).amount < cancel_tab (x).min_amount
            THEN
               cancel_tab (x).amount := cancel_tab (x).min_amount;
            END IF;

            x := x + 1;
            cancel_tab (x).date_from :=
                             0
                             - ((TO_NUMBER (p_arr_time_par) / 2400) - .33334);
            cancel_tab (x).date_to := 3.00000;
            cancel_tab (x).tar_no := '1050.0080';
            cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
            cancel_tab (x).amount :=
                                    (vn_summ1 * cancel_tab (x).PERCENT / 100
                                    );
            cancel_tab (x).tar_min := '1050.0085';
            cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

            IF cancel_tab (x).amount < cancel_tab (x).min_amount
            THEN
               cancel_tab (x).amount := cancel_tab (x).min_amount;
            END IF;

            x := x + 1;
            cancel_tab (x).date_from := 0
                                        - (TO_NUMBER (p_arr_time_par) / 2400);
            cancel_tab (x).date_to :=
                             0
                             - ((TO_NUMBER (p_arr_time_par) / 2400) - .33333);
            cancel_tab (x).tar_no := '1050.0090';
            cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
            cancel_tab (x).amount :=
                                    (vn_summ1 * cancel_tab (x).PERCENT / 100
                                    );
            cancel_tab (x).tar_min := NULL;
            cancel_tab (x).min_amount := NULL;
         ELSIF vd_date >= vd_eff_date
         THEN
            cancel_tab (x).date_from := 30.00001;
            cancel_tab (x).date_to := 365.00000;
            cancel_tab (x).tar_no := '1050.0250';
            cancel_tab (x).PERCENT :=
                             sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
            cancel_tab (x).amount :=
                                   (vn_summ1 * cancel_tab (x).PERCENT / 100
                                   );
            cancel_tab (x).tar_min := '1050.0255';
            cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

            IF cancel_tab (x).amount < cancel_tab (x).min_amount
            THEN
               cancel_tab (x).amount := cancel_tab (x).min_amount;
            END IF;

            x := x + 1;
            cancel_tab (x).date_from := 21.00001;
            cancel_tab (x).date_to := 30.00000;
            cancel_tab (x).tar_no := '1050.0260';
            cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
            cancel_tab (x).amount :=
                                    (vn_summ1 * cancel_tab (x).PERCENT / 100
                                    );
            cancel_tab (x).tar_min := '1050.0265';
            cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

            IF cancel_tab (x).amount < cancel_tab (x).min_amount
            THEN
               cancel_tab (x).amount := cancel_tab (x).min_amount;
            END IF;

            x := x + 1;
            cancel_tab (x).date_from := 3.00001;
            cancel_tab (x).date_to := 21.00000;
            cancel_tab (x).tar_no := '1050.0270';
            cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
            cancel_tab (x).amount :=
                                    (vn_summ1 * cancel_tab (x).PERCENT / 100
                                    );
            cancel_tab (x).tar_min := '1050.0275';
            cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

            IF cancel_tab (x).amount < cancel_tab (x).min_amount
            THEN
               cancel_tab (x).amount := cancel_tab (x).min_amount;
            END IF;

            x := x + 1;
            cancel_tab (x).date_from :=
                             0
                             - ((TO_NUMBER (p_arr_time_par) / 2400) - 1.5000);
            cancel_tab (x).date_to := 3.00000;
            cancel_tab (x).tar_no := '1050.0280';
            cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
            cancel_tab (x).amount :=
                                    (vn_summ1 * cancel_tab (x).PERCENT / 100
                                    );
            cancel_tab (x).tar_min := '1050.0285';
            cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

            IF cancel_tab (x).amount < cancel_tab (x).min_amount
            THEN
               cancel_tab (x).amount := cancel_tab (x).min_amount;
            END IF;

            x := x + 1;
            cancel_tab (x).date_from := 0
                                        - (TO_NUMBER (p_arr_time_par) / 2400);
            cancel_tab (x).date_to :=
                             0
                             - ((TO_NUMBER (p_arr_time_par) / 2400) - 1.4990);
            cancel_tab (x).tar_no := '1050.0290';
            cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
            cancel_tab (x).amount :=
                                    (vn_summ1 * cancel_tab (x).PERCENT / 100
                                    );
            cancel_tab (x).tar_min := NULL;
            cancel_tab (x).min_amount := NULL;
         END IF;
      ELSIF     p_stat_date >= p_canc_eff_date
            AND vd_stat_date_fake < vd_s61519_eff_date
      -- S61519 VJaen 05-Dec-2007.
      THEN
         DBMS_OUTPUT.put_line
            ('Entrando por condicion [p_stat_date >= p_canc_eff_date AND vd_rqst_date_fake < vd_s61519_eff_date]'
            );
         pkg_plog.sec_implementation
            ('PKG_MB620000PR.F_FILL_CANCEL_TABLE',
             'Entrando por condicion [p_stat_date >= p_canc_eff_date AND vd_rqst_date_fake < vd_s61519_eff_date]',
             'INFO'
            );
         cancel_tab (x).date_from := 720.0167;
         cancel_tab (x).date_to := 8760.0000;
         cancel_tab (x).tar_no := '1050.0250';
         cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
         cancel_tab (x).amount := (vn_summ1 * cancel_tab (x).PERCENT / 100);
         cancel_tab (x).tar_min := '1050.0255';
         cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

         IF cancel_tab (x).amount < cancel_tab (x).min_amount
         AND NVL(p_return_all, 'Y') <> 'Y' -- S65727.T10 / VJaen 21Feb2008
         THEN
            cancel_tab (x).amount := cancel_tab (x).min_amount;
         END IF;

         x := x + 1;
         cancel_tab (x).date_from := 504.0167;
         cancel_tab (x).date_to := 720.0000;
         cancel_tab (x).tar_no := '1050.0260';
         cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
         cancel_tab (x).amount := (vn_summ1 * cancel_tab (x).PERCENT / 100);
         cancel_tab (x).tar_min := '1050.0265';
         cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

         IF cancel_tab (x).amount < cancel_tab (x).min_amount
         AND NVL(p_return_all, 'Y') <> 'Y' -- S65727.T10 / VJaen 21Feb2008
         THEN
            cancel_tab (x).amount := cancel_tab (x).min_amount;
         END IF;

         x := x + 1;
         cancel_tab (x).date_from := 72.0167;
         cancel_tab (x).date_to := 504.0000;
         cancel_tab (x).tar_no := '1050.0270';
         cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
         cancel_tab (x).amount := (vn_summ1 * cancel_tab (x).PERCENT / 100);
         cancel_tab (x).tar_min := '1050.0275';
         cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

         IF cancel_tab (x).amount < cancel_tab (x).min_amount
         AND NVL(p_return_all, 'Y') <> 'Y' -- S65727.T10 / VJaen 21Feb2008
         THEN
            cancel_tab (x).amount := cancel_tab (x).min_amount;
         END IF;

         x := x + 1;
         cancel_tab (x).date_from := 36.0000;
         cancel_tab (x).date_to := 72.0000;
         cancel_tab (x).tar_no := '1050.0280';
         cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
         cancel_tab (x).amount := (vn_summ1 * cancel_tab (x).PERCENT / 100);
         cancel_tab (x).tar_min := '1050.0285';
         cancel_tab (x).min_amount :=
                             sf_get_eff_rate (cancel_tab (x).tar_min, vd_date);

         IF cancel_tab (x).amount < cancel_tab (x).min_amount
         AND NVL(p_return_all, 'Y') <> 'Y' -- S65727.T10 / VJaen 21Feb2008
         THEN
            cancel_tab (x).amount := cancel_tab (x).min_amount;
         END IF;

         x := x + 1;
         cancel_tab (x).date_from := 0;
         cancel_tab (x).date_to := 35.9833;
         cancel_tab (x).tar_no := '1050.0290';
         cancel_tab (x).PERCENT :=
                              sf_get_eff_rate (cancel_tab (x).tar_no, vd_date);
         cancel_tab (x).amount := (vn_summ1 * cancel_tab (x).PERCENT / 100);
         cancel_tab (x).tar_min := NULL;
         cancel_tab (x).min_amount := NULL;
      ELSIF vd_stat_date_fake >= vd_s61519_eff_date
      -- S61519 VJaen 05-Dec-2007.
      THEN
         DBMS_OUTPUT.put_line
            ('Entrando por NUEVA condicion [vd_rqst_date_fake >= vd_s61519_eff_date]'
            );
         pkg_plog.sec_implementation
            ('PKG_MB620000PR.F_FILL_CANCEL_TABLE',
             'Entrando por NUEVA condicion [vd_rqst_date_fake >= vd_s61519_eff_date]',
             'INFO'
            );

         --
            -- ** INCLUR EL LLAMADO A pkg_book_tcm_util.f_get_bkcan_by_date **
         DECLARE
            vt_tab            pkg_book_tcm_util.tcm_tab_typ;
            vt_rec            pkg_book_tcm_util.tcm_rec_typ;
            vn_idx            NUMBER;
            vd_rqst_bk_date   DATE;
         BEGIN
            IF NVL (p_return_all, 'Y') = 'Y'
            THEN
               /*
                * Busca todas las categorías / tarifas.
                */
               DBMS_OUTPUT.put_line ('Busca todas las categorías / tarifas.');
               --
               -- Obtiene la lista completa de las tarifas por rango para Cancelación de Reservaciones.
               vt_tab :=
                  pkg_book_tcm_util.f_get_list_by_typ
                                            (vd_stat_date_fake,
                                             pkg_book_datadict.f_tar_grp_std,
                                             pkg_book_datadict.f_typ_bkcancel
                                            );
            ELSE
               /*
                * Busca unicamente la categorías / tarifas que aplica.
                */
               DBMS_OUTPUT.put_line
                      ('Busca unicamente la categorías / tarifas que aplica.');
               --
               --  Adiciona la hora requerida de arribo a la fecha del booking.
               vd_rqst_bk_date :=
                  TO_DATE (   TO_CHAR (p_rqst_bk_date, 'DD-MON-YYYY')
                           || p_arr_time_par,
                           'DD-MON-YYYY HH24MI'
                          );
               --
               -- Obtiene la categorías / tarifas para la cancelación de la reservacion.
               vt_tab :=
                  pkg_book_tcm_util.f_get_bkcan_by_date (vd_stat_date_fake,
                                                         /* Se toma para evaluar la fecha de efectividad del mapping con la tarifa */
                                                         vd_rqst_bk_date,
                                                         vd_stat_date_fake
                                                        /* Fecha del estatus, es sysdate cuando el estatus es BKD */
                                                        );
            END IF;

            --
            IF vt_tab IS NOT NULL AND vt_tab.COUNT > 0
            THEN
               FOR i IN vt_tab.FIRST .. vt_tab.LAST
               LOOP
                  vt_rec := vt_tab (i);
                  DBMS_OUTPUT.put_line (   vt_rec.acg_cd
                                        || ' - '
                                        || vt_rec.tar_no
                                        || ' - ['
                                        || vt_rec.tar_rate
                                        || '] - '
                                        || vt_rec.tar_descr
                                        || ' - '
                                        || vt_rec.tcm_typ
                                        || ' - '
                                        || vt_rec.range1_from
                                        || ' - '
                                        || vt_rec.range1_to
                                        || ' - '
                                        || vt_rec.range1_unit
                                       );
                  pkg_plog.sec_implementation
                                        ('PKG_MB620000PR.F_FILL_CANCEL_TABLE',
                                            'DATOS:'
                                         || CHR (10)
                                         || vt_rec.acg_cd
                                         || ' - '
                                         || vt_rec.tar_no
                                         || ' - ['
                                         || vt_rec.tar_rate
                                         || '] - '
                                         || vt_rec.tar_descr
                                         || ' - '
                                         || vt_rec.tcm_typ
                                         || ' - '
                                         || vt_rec.range1_from
                                         || ' - '
                                         || vt_rec.range1_to
                                         || ' - '
                                         || vt_rec.range1_unit,
                                         'INFO'
                                        );
                  /*
                   *
                   */
                  vn_idx := cancel_tab.COUNT + 1;

                  IF vt_rec.range1_unit = pkg_book_datadict.f_runit_mins
                  THEN
                     IF vn_idx = vt_tab.COUNT
                     THEN
                        -- último registro.
                        cancel_tab (vn_idx).date_from :=
                                                      vt_rec.range1_from / 60;
                        cancel_tab (vn_idx).date_to := vt_rec.range1_to / 60;
                     ELSE
                        cancel_tab (vn_idx).date_from :=
                                                      vt_rec.range1_from / 60;
                        cancel_tab (vn_idx).date_to := vt_rec.range1_to / 60;
                     END IF;
                  END IF;

                  cancel_tab (vn_idx).tar_no := vt_rec.tar_no;
                  cancel_tab (vn_idx).description := vt_rec.tar_descr;
                  cancel_tab (vn_idx).acg_cd := vt_rec.acg_cd;
                  cancel_tab (vn_idx).PERCENT := vt_rec.tar_rate;
                  cancel_tab (vn_idx).amount :=
                               (vn_summ1 * cancel_tab (vn_idx).PERCENT / 100
                               );
                  DBMS_OUTPUT.put_line (   'Amount='
                                        || cancel_tab (vn_idx).amount
                                        || ' < min_amount='
                                        || cancel_tab (vn_idx).min_amount
                                       );
                  --
                  -- Se pasan los valores de tarifa mínima al record que se devuelve.
                  cancel_tab (vn_idx).tar_min := vt_rec.min_tar_no;
                  cancel_tab (vn_idx).min_amount := vt_rec.min_tar_rate;
                  cancel_tab (vn_idx).min_description := vt_rec.min_tar_descr;

                  --
                  -- Evalua si el cálculo del porcentaje es menor al mínimo.
                  IF     cancel_tab (vn_idx).amount <
                                                  NVL (vt_rec.min_tar_rate, 0)
                     AND NVL (p_return_all, 'Y') <> 'Y'
                  THEN
                     --
                     -- Etablece el mínimo como el monto del cargo.
                     cancel_tab (vn_idx).amount :=
                                               cancel_tab (vn_idx).min_amount;
                  ELSIF NVL (p_return_all, 'Y') <> 'Y'
                  THEN
                     --
                     -- Blanquea los campos relacionados a tarifa minima.
                     cancel_tab (vn_idx).tar_min := NULL;
                     cancel_tab (vn_idx).min_amount := NULL;
                     cancel_tab (vn_idx).min_description := NULL;
                  END IF;
               /*
                *
                */
               END LOOP;
            ELSE
               DBMS_OUTPUT.put_line ('La tabla retornada está vacía...');
            END IF;
         END;
      END IF;

      RETURN cancel_tab;
   END f_fill_cancel_table;

   FUNCTION f_chk_if_gf (p_sin IN NUMBER)
      RETURN VARCHAR2
   IS
-- PL/SQL Specification
-- Program Data
      vv_source   VARCHAR2 (1);
      vv_gf       VARCHAR2 (1);
-- PL/SQL Block
   BEGIN
      SELECT pc.srce_cd
        INTO vv_source
        FROM pc_ums_certs pc
       WHERE seq =
                (SELECT MAX (seq)
                   FROM pc_ums_certs pc1
                  WHERE pc1.sin_seq = p_sin
                    AND stat_cd <> 'D'                                -- R2889
                    AND NVL (pc1.cert_date, pc1.apprv_date) IS NOT NULL
                    AND TRUNC (pc1.date_to) = TRUNC (TO_DATE ('31-DEC-4712')))
         AND stat_cd <> 'D';                                          -- R2889

      IF vv_source = 'P'
      THEN
         RETURN ('Y');
      ELSE
         RETURN ('N');
      END IF;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN ('N');
   END f_chk_if_gf;

   FUNCTION f_chk_if_hs (p_seq IN NUMBER)
      RETURN VARCHAR2
   IS
-- PL/SQL Specification
-- Program Data
      vd_date_to     DATE;
      vv_lev         VARCHAR2 (1);
      vd_date_from   DATE;
      vd_req_date    DATE;
-- PL/SQL Block
   BEGIN
      BEGIN
         SELECT rqst_date
           INTO vd_req_date
           FROM bk_rqsts
          WHERE seq = p_seq;
      EXCEPTION
         WHEN OTHERS
         THEN
            RETURN ('N');
      END;

      BEGIN
         SELECT DISTINCT bdca.bco_lev, bdca.date_from, bdca.date_to
                    INTO vv_lev, vd_date_from, vd_date_to
                    FROM bk_day_cond_assgs bdca, bk_days bd, bk_rqsts br
                   WHERE br.seq = p_seq
                     AND br.bd_seq = bd.seq
                     AND bd.bdca_seq = bdca.seq;

         IF vv_lev = '3'
         THEN
            IF vd_req_date BETWEEN vd_date_from AND vd_date_to
            THEN
               RETURN ('Y');
            ELSE
               RETURN ('N');
            END IF;
         ELSE
            RETURN ('N');
         END IF;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            RETURN ('N');
         WHEN OTHERS
         THEN
            RETURN ('N');
      END;
   END;

   FUNCTION f_get_sin (p_br IN NUMBER)
      RETURN NUMBER
   IS
-- PL/SQL Specification
-- Program Data
      vn_sin   NUMBER (7);
-- PL/SQL Block
   BEGIN
      SELECT DISTINCT sin_seq
                 INTO vn_sin
                 FROM cust_sched_needs csn,
                      itin_items iti,
                      bill_sets bs,
                      bk_rqsts br
                WHERE br.seq = p_br
                  AND br.bs_seq = bs.seq
                  AND bs.seq = iti.bs_seq
                  AND iti.csn_seq = csn.seq;

      RETURN vn_sin;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN NULL;
   END f_get_sin;

   FUNCTION f_get_br_stat (p_br IN NUMBER)
      RETURN VARCHAR2
   IS
      vv_stat   bk_rqsts.stat%TYPE;
   BEGIN
      SELECT stat
        INTO vv_stat
        FROM bk_rqsts
       WHERE seq = p_br;

      RETURN vv_stat;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN NULL;
   END f_get_br_stat;

   FUNCTION f_get_br_stat_date (p_br IN NUMBER)
      RETURN DATE
   IS
-- PL/SQL Specification
-- Program Data
      vd_stat_date   DATE;
--PL/SQL Block
   BEGIN
      SELECT br.stat_date
        INTO vd_stat_date
        FROM bk_rqsts br
       WHERE br.seq = p_br;

      RETURN vd_stat_date;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN NULL;
   END f_get_br_stat_date;

   FUNCTION f_get_bk_date (p_br IN NUMBER)
      RETURN DATE
   IS
-- PL/SQL Specification
-- Program Data
      vd_date   DATE;
   BEGIN
      SELECT TRUNC (br.rqst_bk_date)
        INTO vd_date
        FROM bk_rqsts br
       WHERE br.seq = p_br;

      RETURN vd_date;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN NULL;
   END f_get_bk_date;

   FUNCTION f_get_bs_seq (p_br IN NUMBER)
      RETURN NUMBER
   IS
-- PL/SQL Specification
-- Program Data
      vn_bs   bk_rqsts.bs_seq%TYPE;
-- PL/SQL Block
   BEGIN
      SELECT bs_seq
        INTO vn_bs
        FROM bk_rqsts
       WHERE seq = p_br;

      RETURN vn_bs;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN NULL;
      WHEN OTHERS
      THEN
         RETURN NULL;
   END f_get_bs_seq;

   FUNCTION f_get_penalty (
      p_br         IN   NUMBER,
      p_req_date   IN   DATE DEFAULT NULL,
      p_trn_date   IN   DATE DEFAULT NULL
   )
      RETURN charge_table_type
   IS
--============================================================================
-- DESCRIPCION:  Esta función se encarga de generar el cargo de reservación
 -- por arribo tardío.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- Tracker Desarrollador  Fecha       Cambios realizados
-- ------- -------------  ----------- -----------------------------------------
-- S#####   ?????         ??-???-???? Version inicial.
-- S61519   VJaen         13-Dic-2007 Implementación de cambios por arribos tardíos,
--                                    para ser cobrados por medio de una tabla de
--                                    rangos de porcentajes.
--                        21-Ene-2007 Se solicitó la evaluación del sight time para
--                                    el late arrival.
--                        23-Ene-2008 Se limpia la tabla de cargos en caso de no
--                                    obtener un rango aplicable.
--                        24-Ene-2008 Evaluación del transit date para aplicar
--                                    tarifa vieja o rangos/tarifas nuevas.
-- S64337                 28-Ene-2008 Definición de nueva variable-tabla de cargos (pen_charge_tab),
--                                    para retornar el penalty, ya que al utilizar la tabla
--                                    original se pueden tener 2 registros generados para
--                                    el booking fee por PCUMS/TEU.
--                                    Inclusión de búsqueda de Account Code para el método viejo.
--=============================================================================
 --
      charge_tab           charge_table_type;
      pen_charge_tab       charge_table_type;
      vv_tar_no            VARCHAR2 (12);
      vn_resp              NUMBER (8, 2);
      vn_bs_seq            bk_rqsts.bs_seq%TYPE;
      x                    NUMBER (1);
      vn_summ              NUMBER (8, 2);
      vn_rate              NUMBER (8, 2);
      vn_br                bk_rqsts.seq%TYPE;
      vd_date              DATE;
      vd_eff_date          DATE;
      vv_msg_1             VARCHAR2 (2000);
      vv_msg_2             VARCHAR2 (2000);
-- S4332 Jdam 13-mar-2006
      vn_fix_amt           bk_rqsts.fix_amt%TYPE;
      vd_sdt_date          DATE;
      --
      -- S61519
      -- VJaen 05-Dec-2007
      vd_s61519_eff_date   DATE       := pkg_book_datadict.f_bill_eff_date_v1;
      vd_rqst_bk_date      bk_rqsts.rqst_bk_date%TYPE;
      vd_trn_date          vsl_act_events.act_time%TYPE;
      -- JDArosemena S94501_Fase2
      vv_bk_stat           BK_RQSTS.STAT%type;
      --                
      --S195850_T1_T4  
      vd_rqst_arr_time    bk_rqsts.rqst_arrival_time%TYPE; 
      vc_format_time       CONSTANT VARCHAR2 (20) := 'HH24MI';
      --
   BEGIN
      vn_br := p_br;
      x := 1;
      --
      --S195850_T1_T4: adicion de busqueda de fecha JIT - rqst_arrival_time
      BEGIN
         SELECT bs_seq, fix_amt, rqst_bk_date, stat, rqst_arrival_time
           INTO vn_bs_seq, vn_fix_amt, vd_rqst_bk_date, vv_bk_stat, vd_rqst_arr_time
           FROM bk_rqsts
          WHERE seq = vn_br;
      EXCEPTION
         WHEN OTHERS
         THEN
            vn_bs_seq := NULL;
      END;

      --
      vd_eff_date := f_get_bk_chg_date (vv_msg_1, vv_msg_2);

      IF p_trn_date IS NULL
      THEN
         vd_date := sf_get_bill_date_par (vn_bs_seq);
         --
         -- S61519
         -- VJaen 24-Ene-2008
         -- Busca el transit date para evaluar que tarifa debe aplicar.
         vd_trn_date := sf_get_transit_date (vn_bs_seq);
      ELSE
         vd_date := p_trn_date;
      END IF;
      --
      --S195850_T1_T4: adicion de la condicion del parametro en el IF 
      --               y el nuevo parametro PEN en el llamado f_get_book_charges
      IF NVL (vn_fix_amt, 0) = 0  OR
        ( NVL (vn_fix_amt, 0) > 0 and  vd_rqst_bk_date > TO_DATE (sf_get_param (920, 'BOOKING'), 'DD-MON-YYYY') )
      THEN                                            -- Booking NO subastados o subastados condicionados
         charge_tab :=
            f_get_book_charges (vn_br,
                                'BF',
                                'O',
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
                                NULL,
                                null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,'PEN'
                               );
         vn_summ := 0;

         FOR x IN 1 .. charge_tab.COUNT
         LOOP
            vn_summ := vn_summ + NVL (charge_tab (x).amount, 0);
         END LOOP;
      ELSIF NVL (vn_fix_amt, 0) > 0  AND
            vd_rqst_bk_date <= TO_DATE (sf_get_param (920, 'BOOKING'), 'DD-MON-YYYY')
      THEN                                     -- Booking producto de  subasta
         vn_summ := vn_fix_amt;
      END IF;                                            -- FIN, NO es subasta

      IF NVL (p_trn_date, vd_trn_date) < vd_s61519_eff_date
      THEN                /* S61519 -- Condicionar por fecha de efectividad */
         IF vd_date < vd_eff_date
         THEN
            vv_tar_no := '1050.0095';
         ELSIF vd_date >= vd_eff_date
         THEN
            vv_tar_no := '1050.0295';
         END IF;

         vn_rate := sf_get_eff_rate (vv_tar_no, vd_date);

         IF NVL (vn_fix_amt, 0) > 0
         THEN
            vd_sdt_date := vd_date;
         ELSIF NVL (vn_fix_amt, 0) = 0
         THEN
            vd_sdt_date := charge_tab (x).eff_date;
         END IF;

         vn_resp := vn_summ * vn_rate / 100;
         pen_charge_tab (x).rate := NULL;
         pen_charge_tab (x).amount := vn_resp;
         pen_charge_tab (x).item_no := vv_tar_no;
         pen_charge_tab (x).eff_date := vd_sdt_date;
         pen_charge_tab (x).description :=
                                         'EXTRAORDINARY BOOKING RETENTION FEE';
         pen_charge_tab (x).unit := NULL;
         pen_charge_tab (x).qty := NULL;
         pen_charge_tab (x).acg_cd :=
            sf_get_acc_grp (pen_charge_tab (x).item_no,
                            'STD',
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            pen_charge_tab (x).eff_date
                           );
      ELSE    /* S61519 -- Calculo de Late Arrival por rango de porcentajes */
         DECLARE
            vt_tab                   pkg_book_tcm_util.tcm_tab_typ;
            vt_rec                   pkg_book_tcm_util.tcm_rec_typ;
            vn_idx                   NUMBER;
            vn_csn_seq               cust_sched_needs.seq%TYPE;
            vn_iti_seq               itin_items.seq%TYPE;
            vd_csn_arr_date_time     cust_sched_needs.arr_date_time%TYPE;
            vd_bk_req_arr_datetime   DATE;
            vt_vsl_info              pkg_csn_vsl_chars.csn_cvc_table;
            vn_extm_beam             cust_vsl_chars.extm_beam%TYPE;
            vc_hml                   cust_vsl_chars.perm_hl_ind%TYPE;
            vc_rest_cd               rest_usr_cds.cd%TYPE;
            vv_req_arr_time          VARCHAR2 (10);
            vv_bk_latearr_typ        tariff_category_mappings.typ%TYPE;

            --
            -- Cursor para obtener secuencia de la visita.
            CURSOR vis_iti_cur
            IS
               SELECT csn.seq, iti.seq,
                      NVL (csn.sight_date_time, csn.arr_date_time)
                 FROM cust_sched_needs csn, itin_items iti
                WHERE csn.seq = iti.csn_seq
                  AND iti.stat_cd <> 'CAN'
                  AND iti.tii_first_in_bs = 'Y'
                  AND iti.bs_seq = vn_bs_seq;
         BEGIN
            /*
             * Esta sección busca el Required Arrival Time
             */
            OPEN vis_iti_cur;

            FETCH vis_iti_cur
             INTO vn_csn_seq, vn_iti_seq, vd_csn_arr_date_time;

            CLOSE vis_iti_cur;

            --
            IF vn_csn_seq IS NOT NULL
            THEN
               -- Busca informacion del buque, por medio del paquete de caracteristicas.
               vt_vsl_info :=
                  pkg_csn_vsl_chars.f_get_csn_chars ('V',
                                                     vn_csn_seq,
                                                     NULL,
                                                     'L'
                                                    );
               vn_extm_beam := vt_vsl_info (1).extm_beam;
               vc_hml := vt_vsl_info (1).hml;
               vc_rest_cd := sf_get_rest_user_bk (vn_iti_seq);
               --  
               --S195850_T1_T4: adicion del IF
               IF vd_rqst_arr_time IS NOT NULL THEN  
                    
                    --busca la hora del JIT
                    vv_req_arr_time := TO_CHAR( vd_rqst_arr_time, vc_format_time);
               
               ELSE 
               
                    --  Solicita el Required Arrival Time.
                    vv_req_arr_time :=
                          pkg_booking.f_get_arr_par (vn_bs_seq,
                                                     vn_extm_beam,
                                                     vc_hml,
                                                     vc_rest_cd
                                                     );
               END IF;
                --
                -- Concatena la fecha del booking con el tiempo requerido de arribo.
                vd_bk_req_arr_datetime :=
                  TO_DATE (   TO_CHAR (vd_rqst_bk_date, 'DD-MON-YYYY')
                           || ' '
                           || vv_req_arr_time,
                           'DD-MON-YYYY HH24MI'
                          );
            END IF;

            /*
             * FIN.  Required Arrival Time.
             */
            --
            -- Solicita la información de tarifa aplicable al rango correspondiente a la hora de arribo.
             --S94501 FASE 2 JDArosemena
            select DECODE(vv_bk_stat,'GRA',vd_rqst_bk_date,vd_date)  into vd_date from dual;
            /*vt_tab :=
               pkg_book_tcm_util.f_get_bklatearr_by_date
                                                       (vd_date,
                                                        vd_csn_arr_date_time,
                                                        vd_bk_req_arr_datetime
                                                       );*/
                vt_tab :=
               pkg_book_tcm_util.f_get_bklatearr_by_date
                                                       (vd_date,
                                                        vd_csn_arr_date_time,
                                                        vd_bk_req_arr_datetime
                                                       );
            --
            --

            --
            IF vt_tab IS NOT NULL AND vt_tab.COUNT > 0
            THEN
               vt_rec := vt_tab (1);
               DBMS_OUTPUT.put_line (   vt_rec.acg_cd
                                     || ' - '
                                     || vt_rec.tar_no
                                     || ' - ['
                                     || vt_rec.tar_rate
                                     || '] - '
                                     || vt_rec.tar_descr
                                     --    || ' - '
                                     --    || vt_rec.fee_min_value
                                     || ' - '
                                     || vt_rec.tcm_typ
                                     || ' - '
                                     || vt_rec.range1_from
                                     || ' - '
                                     || vt_rec.range1_to
                                     || ' - '
                                     || vt_rec.range1_unit
                                    );

               /*
                * Determina el service date (vd_sdt_date) dependiendo de si es un Auction o no.
                */    
               --
               --S195850_T1_T4: adicion de la condicion del parametro
               --      
               IF NVL (vn_fix_amt, 0) > 0  AND 
                  vd_rqst_bk_date <= TO_DATE (sf_get_param (920, 'BOOKING'), 'DD-MON-YYYY')
               THEN

                  vd_sdt_date := vd_date;

               ELSIF NVL (vn_fix_amt, 0) = 0   OR
                     ( NVL (vn_fix_amt, 0) > 0 and  vd_rqst_bk_date > TO_DATE (sf_get_param (920, 'BOOKING'), 'DD-MON-YYYY') )
               THEN

                  vd_sdt_date := charge_tab (x).eff_date;

               END IF;

                 /*
                  * Reemplaza los valores como se hacía antes,
               * sobre el mismo registro que se tenía del cálculo del fee.
               */
               pen_charge_tab (x).rate := NULL;
               pen_charge_tab (x).amount := (vn_summ * vt_rec.tar_rate / 100);
               pen_charge_tab (x).item_no := vt_rec.tar_no;
               pen_charge_tab (x).eff_date := vd_sdt_date;
               pen_charge_tab (x).description := vt_rec.tar_descr;
               pen_charge_tab (x).unit := NULL;
               pen_charge_tab (x).qty := NULL;
               pen_charge_tab (x).acg_cd := vt_rec.acg_cd;
            ELSE
               --
               --
               DBMS_OUTPUT.put_line
                  ('La tabla retornada está vacía...verifique si la diferencia de minutos en negativa.'
                  );
            END IF;
         END;
      END IF;

      RETURN pen_charge_tab;
   END f_get_penalty;

/* Para obtener la fecha de efectividad de la tabla de parametros. */
   FUNCTION f_get_bk_chg_date (p_msg1 OUT VARCHAR, p_msg2 OUT VARCHAR2)
      RETURN DATE
   IS
      vd_eff_date   DATE;
---------------------------------------------------------------------------
--R3706: Cambios en Tarifas de Booking
--JDArosemena 13-Dec-2003
--Esta funcion se encarga obtener la fecha de efectividad para la vigencia
--Dicha fecha de vigencia se encuentra definida en la tabla de parametros.
---------------------------------------------------------------------------
   BEGIN
      BEGIN
         SELECT TO_DATE (val, 'DD-MON-YYYY')
           INTO vd_eff_date
           FROM PARAMETERS
          WHERE typ_cd = 'BILLING' AND descr = 'BK_CHG_DATE';
      EXCEPTION
         WHEN OTHERS
         THEN
            vd_eff_date := TO_DATE ('01-JAN-2004', 'DD-MON-YYYY');
            p_msg1 :=
                  'Error reading parameter table - reading BK_CHG_DATE parameter - error:'
               || SQLERRM;
            p_msg2 :=
               'BK_CHG_DATE parameter was set to 01-JAN-2004 - If todays date is greater than JUN 01 2002 send an e-mail for assistace, if not please call support.';
      END;

      --
      --
      RETURN (vd_eff_date);
   END f_get_bk_chg_date;

/* Para obtener Fecha de Request de Booking para los cambios a partir del 1ro de enero de 2004*/
   FUNCTION f_get_br_req_date (p_br IN NUMBER)
      RETURN DATE
   IS
-- PL/SQL Specification
-- Program Data
      vd_req_date   DATE;
--PL/SQL Block
   BEGIN
      SELECT br.rqst_date
        INTO vd_req_date
        FROM bk_rqsts br
       WHERE br.seq = p_br;

      RETURN vd_req_date;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN NULL;
   END f_get_br_req_date;

/* Para obtener Fecha de Request de Booking para los cambios a partir del 1ro de enero de 2004*/
   FUNCTION f_get_swap_bs (p_br IN NUMBER)
      RETURN NUMBER
   IS
-- PL/SQL Specification
-- Program Data
      vn_bs_seq   bill_sets.seq%TYPE;
--PL/SQL Block
   BEGIN
      SELECT br.bs_seq_swap
        INTO vn_bs_seq
        FROM bk_rqsts br
       WHERE br.seq = p_br;

      RETURN vn_bs_seq;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN (NULL);
   END f_get_swap_bs;

   FUNCTION f_get_prev_tons (p_sin IN NUMBER, p_date IN DATE)
      RETURN NUMBER
   IS
      vn_pcums          pc_ums_certs.net_tons%TYPE;
      vn_ondeck         pc_ums_certs.on_dek_tons%TYPE;
      vn_vsl_net_tons   pc_ums_certs.vsl_net_tons%TYPE;
      vn_old_pcums      pc_ums_certs.net_tons%TYPE;
   BEGIN
      BEGIN
         SELECT net_tons, on_dek_tons, vsl_net_tons
           INTO vn_pcums, vn_ondeck, vn_vsl_net_tons
           FROM pc_ums_certs
          WHERE sin_seq = p_sin
            AND stat_cd <> 'D'                                        -- R2889
            AND p_date BETWEEN NVL (cert_date, apprv_date) AND date_to;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            NULL;
         WHEN TOO_MANY_ROWS
         THEN
            BEGIN
               SELECT net_tons, on_dek_tons, vsl_net_tons
                 INTO vn_pcums, vn_ondeck, vn_vsl_net_tons
                 FROM pc_ums_certs,
                      (SELECT seq
                         FROM (SELECT   seq
                                   FROM pc_ums_certs
                                  WHERE sin_seq = p_sin
                               ORDER BY seq DESC)
                        WHERE ROWNUM = 1) curr_pc_ums
                WHERE curr_pc_ums.seq = pc_ums_certs.seq
                  AND sin_seq = p_sin
                  AND stat_cd <> 'D'                                  -- R2889
                  AND p_date BETWEEN NVL (cert_date, apprv_date) AND date_to
                  AND apprv_date IS NOT NULL;
            EXCEPTION
               WHEN OTHERS
               THEN
                  NULL;
            END;
      END;

      IF NVL (vn_vsl_net_tons, 0) = NVL (vn_pcums, 0)
      THEN
         vn_old_pcums := NVL (vn_vsl_net_tons, 0) + NVL (vn_ondeck, 0);
      ELSE
         vn_old_pcums := NVL (vn_pcums, 0);
      END IF;

      RETURN vn_old_pcums;
   END f_get_prev_tons;

   FUNCTION f_get_bk_canc_chg (p_msg3 OUT VARCHAR, p_msg4 OUT VARCHAR2)
      RETURN DATE
   IS
      vd_eff_date   DATE;
---------------------------------------------------------------------------
--R3706: Cambios en Tarifas de Booking
--JDArosemena 13-Dec-2003
--Esta funcion se encarga obtener la fecha de efectividad para la vigencia
--Dicha fecha de vigencia se encuentra definida en la tabla de parametros.
---------------------------------------------------------------------------
   BEGIN
      BEGIN
         SELECT TO_DATE (val, 'DD-MON-YYYY')
           INTO vd_eff_date
           FROM PARAMETERS
          WHERE typ_cd = 'BILLING' AND descr = 'BK_CANCL';
      EXCEPTION
         WHEN OTHERS
         THEN
            vd_eff_date := TO_DATE ('01-JUN-2005', 'DD-MON-YYYY');
            p_msg3 :=
                  'Error reading parameter table - reading BK_CANCL parameter - error:'
               || SQLERRM;
            p_msg4 :=
               'BK_CANCL parameter was set to 01-JUN-2005 - If todays date is greater than JUL 01 2005 send an e-mail for assistace, if not please call support.';
      END;

      --
      --
      RETURN (vd_eff_date);
   END f_get_bk_canc_chg;

   /*
    *
    */
   FUNCTION f_get_late_arr_chrgs (
      p_eff_date       DATE DEFAULT SYSDATE,
      p_req_arr_time   VARCHAR,
      p_bk_date        DATE,
      p_bk_fee         NUMBER
   )
      RETURN cancel_table_type
   IS
--============================================================================
-- DESCRIPCION: Esta función se encarga de generar la lista de rangos para
 --  arribos tardíos.
--
-- NOTAS:
--
-- REQUERIMIENTOS:
--
--
--=============================================================================
-- HISTORIA DE MODIFICACIONES
-- SOS     Desarrollador  Fecha       Cambios realizados
-- ------- -------------  ----------  -----------------------------------------
-- S65727  VJaen         18-Feb-2008 Version inicial.
--=============================================================================
   --
      vt_tab                   pkg_book_tcm_util.tcm_tab_typ;
      vt_rec                   pkg_book_tcm_util.tcm_rec_typ;
      vn_idx                   NUMBER;
      vv_late_arr_type         tariff_category_mappings.typ%TYPE;
      vd_req_arr_date_time     DATE;
      vt_charge_tab            cancel_table_type;
      --
      --
      e_no_result              EXCEPTION;
      e_invalid_arrival_time   EXCEPTION;
   BEGIN
      pkg_plog.sec_implementation ('PKG_MB620000PR.F_GET_LATE_ARR_CHRGS',
                                      'PARAMETROS:'
                                   || CHR (10)
                                   || p_eff_date
                                   || ' - '
                                   || p_req_arr_time
                                   || ' - '
                                   || p_bk_date
                                   || ' - '
                                   || p_bk_fee,
                                   'INFO'
                                  );

      --
      -- Evalua la hora requerida de arribo, para determinar la categoría.
      IF p_req_arr_time = pkg_book_datadict.f_bk_arr_time_1
      THEN
         --
         -- El arribo requerido es a la 0200.
         vv_late_arr_type := pkg_book_datadict.f_typ_bklate02;
      ELSIF p_req_arr_time = pkg_book_datadict.f_bk_arr_time_2
      THEN
         --
         -- El arribo requerido es a la 1400.
         vv_late_arr_type := pkg_book_datadict.f_typ_bklate14;
      ELSIF p_req_arr_time = pkg_book_datadict.f_bk_arr_time_3
      THEN
         --
         -- Hora de arribo requerida 2359.
         DBMS_OUTPUT.put_line (   'La programación para el arrival time '
                               || p_req_arr_time
                               || ' no ha sido implementada.'
                              );
         RAISE e_no_result;
      ELSE
         --S195850_T1_T4: comentario del exception y adicion de la nueva constante
         --RAISE e_invalid_arrival_time;  
          
         -- El arribo requerido es el JIT
         vv_late_arr_type := pkg_book_datadict.f_typ_bklatejit;  
      END IF;

      --
      -- Obtiene la lista completa de las tarifas por rango para Late arrival.
      vt_tab :=
         pkg_book_tcm_util.f_get_list_by_typ (p_eff_date,
                                              pkg_book_datadict.f_tar_grp_std,
                                              vv_late_arr_type
                                             );

      --
      IF vt_tab IS NOT NULL AND vt_tab.COUNT > 0
      THEN
         --
         --
         vd_req_arr_date_time :=
            TO_DATE (TO_CHAR (p_bk_date, 'DD-MON-YYYY') || ' '
                     || p_req_arr_time,
                     'DD-MON-YYYY HH24MI'
                    );

         FOR i IN vt_tab.FIRST .. vt_tab.LAST
         LOOP
            vt_rec := vt_tab (i);
            DBMS_OUTPUT.put_line (   vt_rec.acg_cd
                                  || ' - '
                                  || vt_rec.tar_no
                                  || ' - ['
                                  || vt_rec.tar_rate
                                  || '] - '
                                  || vt_rec.tar_descr
                                  || ' - '
                                  || vt_rec.tcm_typ
                                  || ' - '
                                  || vt_rec.range1_from
                                  || ' - '
                                  || vt_rec.range1_to
                                  || ' - '
                                  || vt_rec.range1_unit
                                 );
            /*
             *
             */
            vn_idx := vt_charge_tab.COUNT + 1;

            IF vt_rec.range1_unit = pkg_book_datadict.f_runit_mins
            THEN
               IF vn_idx = vt_tab.COUNT
               THEN
                  -- Convierte los rangos de tiempo en fracciones de días.
                  vt_charge_tab (vn_idx).date_from :=
                                           (vt_rec.range1_from - 1
                                           ) / 60 / 24;
               --  Para el último registro solamente se genera el FROM.
               ELSE
                  -- Convierte los rangos de tiempo en fracciones de días.
                  vt_charge_tab (vn_idx).date_from :=
                                                 vt_rec.range1_from / 60 / 24;
                  vt_charge_tab (vn_idx).date_to :=
                                                   vt_rec.range1_to / 60 / 24;
               END IF;
            END IF;

            vt_charge_tab (vn_idx).tar_no := vt_rec.tar_no;
            vt_charge_tab (vn_idx).description := vt_rec.tar_descr;
            vt_charge_tab (vn_idx).acg_cd := vt_rec.acg_cd;
            vt_charge_tab (vn_idx).PERCENT := vt_rec.tar_rate;
            vt_charge_tab (vn_idx).amount :=
                            (p_bk_fee * vt_charge_tab (vn_idx).PERCENT / 100
                            );
            DBMS_OUTPUT.put_line (   'Amount='
                                  || vt_charge_tab (vn_idx).amount
                                  || ' < min_amount='
                                  || vt_charge_tab (vn_idx).min_amount
                                 );

            --
            -- Evalua si el cálculo del porcentaje es menor al mínimo.
            IF vt_charge_tab (vn_idx).amount < NVL (vt_rec.min_tar_rate, 0)
            THEN
               --
               -- Se pasan los valores de tarifa mínima al record que se devuelve.
               vt_charge_tab (vn_idx).tar_min := vt_rec.min_tar_no;
               vt_charge_tab (vn_idx).min_amount := vt_rec.min_tar_rate;
               vt_charge_tab (vn_idx).min_description := vt_rec.min_tar_descr;
               --
               -- Etablece el mínimo como el monto del cargo.
               vt_charge_tab (vn_idx).amount :=
                                            vt_charge_tab (vn_idx).min_amount;
            ELSE
               --
               -- Blanquea los campos relacionados a tarifa minima.
               vt_charge_tab (vn_idx).tar_min := NULL;
               vt_charge_tab (vn_idx).min_amount := NULL;
               vt_charge_tab (vn_idx).min_description := NULL;
            END IF;
         /*
          *
          */
         END LOOP;
      ELSE
         DBMS_OUTPUT.put_line ('La tabla retornada está vacía...');
      END IF;

      --
      --
      RETURN vt_charge_tab;
   EXCEPTION
      WHEN e_no_result
      THEN
         DBMS_OUTPUT.put_line (   'La programación para el arrival time '
                               || p_req_arr_time
                               || ' no ha sido implementada.  Check TLOG'
                              );
         pkg_plog.sec_implementation
                                  ('PKG_MB620000PR.F_GET_LATE_ARR_CHRGS',
                                      'La programación para el arrival time '
                                   || p_req_arr_time
                                   || ' no ha sido implementada.',
                                   'ERROR'
                                  );
         RETURN vt_charge_tab;
      WHEN e_invalid_arrival_time
      THEN
         DBMS_OUTPUT.put_line ('El arrival date/time es inválido.');
         pkg_plog.sec_implementation
                                    ('PKG_MB620000PR.F_GET_LATE_ARR_CHRGS',
                                        'El arrival date/time es inválido. '
                                     || p_req_arr_time
                                     || '.  Check TLOG',
                                     'ERROR'
                                    );
         RETURN vt_charge_tab;
      WHEN OTHERS
      THEN
         DBMS_OUTPUT.put_line
                             ('El arrival date/time es inválido.  Check TLOG');
         pkg_plog.sec_implementation ('PKG_MB620000PR.F_GET_LATE_ARR_CHRGS',
                                      SUBSTR (   'EXCEPTION WHEN OTHERS='
                                              || SQLERRM,
                                              1,
                                              2000
                                             ),
                                      'ERROR'
                                     );
         RETURN vt_charge_tab;
   END f_get_late_arr_chrgs;


   FUNCTION F_GET_BOOK_CHARGES
  (BOOK_PARAM_REC IN OUT PKG_MB620000PR.BOOK_CHARGE_PARAM_RT
  )
   RETURN PKG_MB620000PR.CHARGE_TABLE_TYPE IS
   --
   cn_rec_idx   CONSTANT NUMBER                                      := 1;
   vn_summ               NUMBER (8, 2);
   vn_swap_amt           bk_rqst_dets.amount%TYPE;
   vn_subs_amt           bk_rqst_dets.amount%TYPE;
   vn_chgd_amt           bk_rqst_dets.amount%TYPE;
   CHARGE_TAB        CHARGE_TABLE_TYPE;
   --
   BEGIN
      --
      BEGIN
         --
         -- Se valida que la tabla de cargas este vacia, de lo contrario es porque la misma fue
         -- definida por una reservacion tipo BEST OFFER.
         IF CHARGE_TAB.COUNT = 0 AND BOOK_PARAM_REC.EFF_DATE IS NOT NULL
         THEN

            --
            CHARGE_TAB := F_GET_BOOKING(BOOK_PARAM_REC);
            --

            IF BOOK_PARAM_REC.BR_CHARGE_TRN = 'BF'
            THEN

               --
               -- Booking fee basado en un best offer
               IF BOOK_PARAM_REC.BO_AMT > 0
               THEN
                  CHARGE_TAB(CN_REC_IDX).EFF_DATE := BOOK_PARAM_REC.RQST_DATE;
               ELSE
                  CHARGE_TAB(CN_REC_IDX).EFF_DATE := BOOK_PARAM_REC.RQST_BK_DATE;
               END IF;

               --
               -- Totaliza el monto de los cargos de booking, en caso de que sean más de uno.
               FOR X IN 1 .. CHARGE_TAB.COUNT
               LOOP
                  --
                  VN_SUMM := NVL (VN_SUMM,0) + NVL(CHARGE_TAB (X).AMOUNT,0);
                  --
               END LOOP;

               --
               -- S15142.T20
               -- VJaen 02-Jul-2008
               -- Reemplazo del monto del cargo, en caso de que el monto de Swap/Subs/Chgd sea mayor.
               --
               IF NVL (vn_summ, 0) < NVL (vn_swap_amt, 0)
               THEN
                  vn_summ := vn_swap_amt;
                  charge_tab (cn_rec_idx).amount := vn_summ;
                  charge_tab (cn_rec_idx).rate   := vn_summ;
                  charge_tab (cn_rec_idx).item_no:= F_get_tar_no_by_amount(trunc(book_param_rec.rqst_date)
                                                                          ,charge_tab(cn_rec_idx).item_no
                                                                          ,charge_tab(cn_rec_idx).amount
                                                                          ,book_param_rec.br_stat
                                                                          ,book_param_rec.bp_id);
               END IF;

               --
               IF NVL (vn_summ, 0) < NVL (vn_subs_amt, 0)
               THEN
                  vn_summ := vn_subs_amt;
                  charge_tab (cn_rec_idx).amount := vn_summ;
                  charge_tab (cn_rec_idx).rate   := vn_summ;
                  charge_tab (cn_rec_idx).item_no:= F_get_tar_no_by_amount(trunc(book_param_rec.rqst_date)
                                                                          ,charge_tab(cn_rec_idx).item_no
                                                                          ,charge_tab(cn_rec_idx).amount
                                                                          ,book_param_rec.br_stat
                                                                          ,book_param_rec.bp_id);
               END IF;

               --
               IF NVL (vn_summ, 0) < NVL (vn_chgd_amt, 0)
               THEN
                  vn_summ := vn_chgd_amt;
                  charge_tab (cn_rec_idx).amount := vn_summ;
                  charge_tab (cn_rec_idx).item_no:= F_get_tar_no_by_amount(trunc(book_param_rec.rqst_date)
                                                                          ,charge_tab(cn_rec_idx).item_no
                                                                          ,charge_tab(cn_rec_idx).amount
                                                                          ,book_param_rec.br_stat
                                                                          ,book_param_rec.bp_id);
               END IF;

            END IF;
            --
         END IF;
         --
      END;
      --
      RETURN CHARGE_TAB;
      --
   END F_GET_BOOK_CHARGES;

   FUNCTION F_GET_BOOKING
  (BOOKING_PARAM_REC PKG_MB620000PR.BOOK_CHARGE_PARAM_RT
  )
   RETURN PKG_MB620000PR.CHARGE_TABLE_TYPE IS
   --
   -- Local Declarations
   cn_chrg_qty         CONSTANT NUMBER                                    := 1;
   cv_unit_beam_cd     CONSTANT tariff_category_mappings.range1_unit%TYPE := pkg_book_datadict.f_runit_beam;
   cv_unit_loa_cd      CONSTANT tariff_category_mappings.range2_unit%TYPE := pkg_book_datadict.f_runit_loa;
   cv_std_tar_grp_cd   CONSTANT tariff_acc_grp_assgs.tar_grp%TYPE         := pkg_book_datadict.f_tar_grp_std;
   cv_for_tar_grp_cd   CONSTANT tariff_acc_grp_assgs.tar_grp%TYPE         := pkg_book_datadict.f_tar_grp_forfeit;
   vv_tar_grp                   tariff_acc_grp_assgs.tar_grp%TYPE         := cv_std_tar_grp_cd;
   charge_tab                   CHARGE_TABLE_TYPE;
   --
   BEGIN
     --
      IF F_CHK_BK_CHRG_BY_DIM(BOOKING_PARAM_REC.RQST_DATE)
      THEN
         --
         -- Inicializacion de variables y tablas de pl/sql
         CHARGE_TAB.DELETE;
         VT_TAB.DELETE;

         --
         IF BOOKING_PARAM_REC.BR_STAT = 'FOR'
         THEN
            VV_TAR_GRP := CV_FOR_TAR_GRP_CD;
         END IF;

         --
         -- Se valida el indicador de TUG BARGE INTEGRATED, ya que si este no es el caso se
         -- generara el cargo de forma regular
         IF NVL(BOOKING_PARAM_REC.TBI_IND,'N') = 'N'
         THEN
            --
            --SOS 94501 Fase II JDArosemena
            /*VT_TAB :=
                    PKG_BOOK_TCM_UTIL.F_GET_BKFIX_BY_RANGE
                                (TRUNC(BOOKING_PARAM_REC.RQST_DATE),  -- Request Date
                                 BOOKING_PARAM_REC.BEAM,              -- Extreme Beam
                                 CV_UNIT_BEAM_CD,                     -- Range1 Unit
                                 BOOKING_PARAM_REC.LOA,               -- Len Overall
                                 CV_UNIT_LOA_CD,                      -- Range2 Unit
                                 BOOKING_PARAM_REC.BP_ID,             -- Booking Period
                                 VV_TAR_GRP
                                );*/
            IF (pkg_cont_eval_util.f_eval_qry_ttt_impl('07',BOOKING_PARAM_REC.RQST_BK_DATE) = 1 and BOOKING_PARAM_REC.BR_STAT <>'GRA')
                OR (pkg_cont_eval_util.f_eval_qry_ttt_impl('07',BOOKING_PARAM_REC.RQST_BK_DATE) = 0)
                OR (pkg_cont_eval_util.f_eval_qry_ttt_impl('07',BOOKING_PARAM_REC.RQST_BK_DATE) = 1 and BOOKING_PARAM_REC.BEAM < 91) THEN
                VT_TAB :=
                    PKG_BOOK_TCM_UTIL.F_GET_BKFIX_BY_RANGE
                                (TRUNC(BOOKING_PARAM_REC.RQST_DATE),  -- Request Date
                                 BOOKING_PARAM_REC.BEAM,              -- Extreme Beam
                                 CV_UNIT_BEAM_CD,                     -- Range1 Unit
                                 BOOKING_PARAM_REC.LOA,               -- Len Overall
                                 CV_UNIT_LOA_CD,                      -- Range2 Unit
                                 BOOKING_PARAM_REC.BP_ID,             -- Booking Period
                                 VV_TAR_GRP,                          -- Tar group
                                 --SOS 94501 Fase II JDArosemena
                                 'OTHER'                              --Booking Status
                                 --
                                );
            ELSIF (pkg_cont_eval_util.f_eval_qry_ttt_impl('07',BOOKING_PARAM_REC.RQST_BK_DATE) = 1) and BOOKING_PARAM_REC.BR_STAT = 'GRA' THEN
                VT_TAB :=
                    PKG_BOOK_TCM_UTIL.F_GET_BKFIX_BY_RANGE
                                (TRUNC(BOOKING_PARAM_REC.RQST_BK_DATE), -- Request Date
                                 BOOKING_PARAM_REC.BEAM,                -- Extreme Beam
                                 CV_UNIT_BEAM_CD,                       -- Range1 Unit
                                 BOOKING_PARAM_REC.LOA,                 -- Len Overall
                                 CV_UNIT_LOA_CD,                        -- Range2 Unit
                                 BOOKING_PARAM_REC.BP_ID,               -- Booking Period
                                 VV_TAR_GRP,                            -- Tar group
                                 --SOS 94501 Fase II JDArosemena
                                 BOOKING_PARAM_REC.BR_STAT            --Booking Status
                                 --
                                );
            END IF;
         --
         -- Si el indicador TUG BARGE INTEGRATED esta definido como cierto, entonces el cargo
         -- sera generado como minimo.
         ELSIF BOOKING_PARAM_REC.TBI_IND = 'Y'
         THEN
            --
            VT_TAB :=
               PKG_BOOK_TCM_UTIL.F_GET_BKTBI_MIN
                                (TRUNC(BOOKING_PARAM_REC.RQST_DATE),
                                 VV_TAR_GRP
                                );
         --
         END IF;

         --
         -- Para el caso especifico de reservaciones basadas en las dimensiones del buque,
         -- la tabla de pl/sql solo deberia retornar 1 registro que contenga todos los elementos
         -- necesarios para la generacion del cargo.
         IF VT_TAB IS NOT NULL
         THEN
            --
            FOR i IN 1 .. VT_TAB.COUNT
            LOOP
               --
               VT_REC := VT_TAB (i);

               --
               -- Si la tarifa no es nula, se definen los campos de la tabla pl/sql
               IF VT_REC.TAR_NO IS NOT NULL
               THEN
                  --
                  CHARGE_TAB (i).UNIT        := 'UNI';
                  CHARGE_TAB (i).QTY         := CN_CHRG_QTY;
                  CHARGE_TAB (i).ACG_CD      := VT_REC.ACG_CD;
                  CHARGE_TAB (i).ITEM_NO     := VT_REC.TAR_NO;
                  CHARGE_TAB (i).RATE        := VT_REC.TAR_RATE;
                  CHARGE_TAB (i).AMOUNT      := CHARGE_TAB(i).RATE * CHARGE_TAB(i).QTY;
                  CHARGE_TAB (i).EFF_DATE    := BOOKING_PARAM_REC.EFF_DATE;
                  CHARGE_TAB (i).DESCRIPTION := VT_REC.TAR_DESCR;
                  --
                  -- Se evalua si el mode es de proforma se redefine la descripcion del cargo.
                  IF BOOKING_PARAM_REC.CHRG_MODE = 'PRF'
                  THEN CHARGE_TAB(i).DESCRIPTION := 'Booking Reservation';
                  END IF;
                  --
               END IF;
               --
            END LOOP;
            --
         END IF; -- vt_tab evaluation
         --
      END IF;
     --
     RETURN CHARGE_TAB;
     --
   END;
   --
END pkg_mb620000pr;
/
SHOW ERROR

