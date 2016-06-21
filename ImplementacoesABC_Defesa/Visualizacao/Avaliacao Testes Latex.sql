 select t.no_modelo             || ' & ' ||  
        t.vl_media_posteriori   || ' & ' ||  
        t.nu_seg_processamento  || ' & ' || 
        t.KL_TEST_EST           || ' & ' || 
        t.KS_TEST_EST           || ' & ' || 
        t.KS_TEST_PVALUE        || ' & ' || 
        t.CHISQ_TEST_EST        || ' \\ \hline'
  from  tb_dabc_resultado_q t 
  where t.nu_vertices = 750
order by t.nu_vertices,
         t.nu_seg_processamento
         
