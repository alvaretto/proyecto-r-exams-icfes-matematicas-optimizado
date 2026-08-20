#!/usr/bin/env Rscript
# Harness de VECTOR COMPLETO: mide de una sola vez todas las dimensiones acopladas.
# Un fix que mejora una componente y empeora otra NO es un fix (regla de parada).
suppressWarnings(suppressMessages(source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/bateria_eliminacion.R")))
RMD <- "sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_schoice_v1.Rmd"
N <- as.integer(Sys.getenv("VEC_N", "300"))
lin<-readLines(RMD,warn=FALSE); ini<-grep("^```\\{r data_generation",lin)[1]+1
fin<-ini-1+which(grepl("^```\\s*$",lin[ini:length(lin)]))[1]-1; CODE<-lin[ini:fin]
V<-list()
for(i in seq_len(N)){set.seed(1000L+7L*i);e<-new.env()
 ok<-tryCatch({eval(parse(text=CODE),envir=e);TRUE},error=function(x)FALSE)
 if(!ok||is.null(e$opciones))next
 V[[length(V)+1]]<-list(o=e$opciones,k=which(e$sol==1),p=e$paso_real,cod=e$err_real$codigo,
   ci=isTRUE(e$err_real$conserva_igualdad),vm=e$valor_mostrado,R=e$par$R,vp=e$valores_prop)}
cat("=== VECTOR COMPLETO | versiones:",length(V),"/",N,"===\n")
pasos<-vapply(V,function(v)v$p,1L)
# 1. semantica
mal<-sum(vapply(V,function(v) !isTRUE(all.equal(as.numeric(v$vp[1]),as.numeric(v$R))) ||
   any(!is.na(v$vp[-1]) & v$vp[-1]==v$R), logical(1)))
cat(sprintf("[SEM]  verificacion semantica ejecutable: %s\n", if(mal==0)"OK" else paste(mal,"FALLOS")))
# 2. magnitud |v|/R
r<-vapply(V,function(v)abs(v$vm)/v$R,numeric(1))
cat(sprintf("[MAG]  |v|/R mediana p1=%.1f p2=%.1f p3=%.1f | >40R: %.1f%% | solape max(p1,p2)=%.1f min(p3)=%.1f\n",
  median(r[pasos==1]),median(r[pasos==2]),median(r[pasos==3]),100*mean(r>40),
  max(r[pasos!=3]),min(r[pasos==3])))
# 3. H1 por rama
h1<-t(vapply(V,function(v){L<-nchar(v$o);k<-v$k
  c(mx=as.numeric(L[k]==max(L)&&sum(L==max(L))==1),mn=as.numeric(L[k]==min(L)&&sum(L==min(L))==1))},numeric(2)))
cat(sprintf("[H1]   agregado larga %.1f%% corta %.1f%%",100*mean(h1[,1]),100*mean(h1[,2]))); h1bad<-FALSE
for(pp in sort(unique(pasos))){ii<-which(pasos==pp)
  mx<-mean(h1[ii,1]);mn<-mean(h1[ii,2]); if(mx>0.45||mn>0.45)h1bad<-TRUE
  cat(sprintf(" | r%d %.0f/%.0f",pp,100*mx,100*mn))}
cat(if(h1bad)"   << ALGUNA RAMA >45%\n" else "   OK\n")
# 4. signo
gu<-function(x)grepl("-",x,fixed=TRUE)
cat(sprintf("[SIG]  clave con guion %.1f%% | senuelos %.1f%% | 'descartar con guion' acierta %.1f%%\n",
 100*mean(vapply(V,function(v)gu(v$o[v$k]),logical(1))),
 100*mean(unlist(lapply(V,function(v)gu(v$o[-v$k])))),
 100*mean(vapply(V,function(v){s<-which(!gu(v$o));if(!length(s))0 else as.numeric(v$k%in%s)/length(s)},numeric(1)))))
# 5. lexico agregado + por rama
FUN<-c("para","entre","toda","todo","que","los","las","del","con","por","una","uno","este","esta",
 "esto","sus","son","como","solo","otro","otra","mas","más","pero","así","asi","hay","debe","cada","sino")
tk<-function(s){s<-tolower(s);s<-gsub("[^a-záéíóúñü ]"," ",s);u<-unique(unlist(strsplit(s,"\\s+")));u[nchar(u)>2]}
lexbad<-FALSE
for(pp in c(0,sort(unique(pasos)))){
 idx<-if(pp==0)seq_along(V) else which(pasos==pp)
 if(length(idx)<20){cat(sprintf("[LEX]  %-10s n<20 NO CONCLUYENTE\n",if(pp==0)"agregado" else paste("rama",pp)));next}
 voc<-unique(unlist(lapply(V[idx],function(v)tk(paste(v$o,collapse=" ")))));best<-NULL
 for(t in setdiff(voc,FUN)){enc<-0;ex<-0
  for(v in V[idx]){h<-vapply(lapply(v$o,tk),function(z)t%in%z,logical(1))
   if(!any(h))next;enc<-enc+1;if(h[v$k]&&sum(h)==1)ex<-ex+1}
  if(enc>=20&&(is.null(best)||ex/enc>best$r))best<-list(t=t,r=ex/enc,n=enc)}
 if(is.null(best)){cat(sprintf("[LEX]  %-10s sin soporte\n",if(pp==0)"agregado" else paste("rama",pp)));next}
 if(best$r>=0.70)lexbad<-TRUE
 cat(sprintf("[LEX]  %-10s '%s' %.1f%% de %d  %s\n",if(pp==0)"agregado" else paste("rama",pp),
   best$t,100*best$r,best$n,if(best$r>=0.70)"<< FUGA" else "OK"))}
# 6. OBJ3 / OBJ4
cat(sprintf("[OBJ3] error real que CONSERVA la igualdad: %d/%d (la Solution ya no puede atribuirles rotura)\n",
  sum(vapply(V,function(v)v$ci,logical(1))),length(V)))
# 7. bateria §P7
B<-NULL; src<-readLines("auditoria_propia.R",warn=FALSE)
i1<-grep("^B <- list\\(",src); i2<-grep("^\\)$",src); i2<-i2[i2>i1][1]
eval(parse(text=paste(src[grep("^nums <- function",src):(i1-1)],collapse="\n")))
eval(parse(text=paste(src[i1:i2],collapse="\n")))
res<-evaluar_bateria(B,lapply(V,function(v)v$o),vapply(V,function(v)v$k,1L))
cat(sprintf("[P7]   max %.1f%% (%s) | techo %.1f%% sd %.1f | EXCESO %+.1f pp | atomico %+.1f pp | %s\n",
 100*res$tasa_max,res$regla_max,100*res$techo_nulo,100*res$sd_nulo,
 100*(res$tasa_max-res$techo_nulo),100*(res$tasa_max-0.25),res$veredicto))
cat("=== resumen: ", if(mal==0 && !h1bad && !lexbad && identical(res$veredicto,"PASS")) "VECTOR OK" else "VECTOR CON HALLAZGOS","\n")
