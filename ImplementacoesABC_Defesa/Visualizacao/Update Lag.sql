select 'update tb_dabc_lag set lag=' || round(qtde/neff) || ' where nu_vertices = ' || nu_vertices || ' and no_modelo =' || '''' || no_modelo || ''';' , qtde, neff
from (
select q.nu_vertices,q.no_modelo,q.neff,count(*) qtde
  from tb_dabc_resultado_q q,
       tb_dabc_amostra_posteriori a
where q.dt_processamento = a.dt_processamento
group by q.nu_vertices,q.no_modelo,q.neff
)
