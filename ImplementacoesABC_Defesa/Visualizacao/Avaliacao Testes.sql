 select t.nu_vertices,
        t.no_modelo,
        t.vl_media_posteriori media_a_posteriori,
        t.nu_seg_processamento tempo_proc,
        t.KL_TEST_EST   KL, 
        t.KS_TEST_EST   KS, 
        t.KS_TEST_PVALUE KS_PVALUE, 
        t.CHISQ_TEST_EST CHIQ,
        t.NEFF           tam_efetivo,
        t.TX_ACEITACAO   
  from  tb_dabc_resultado_q t 
order by t.nu_vertices,
         t.no_modelo