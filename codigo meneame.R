######### PROYECTO MENÉAME #########
#Borrar memoria
rm(list=ls())

#Quitamos los warnings
options(warn=-1)

#Librerias
library(dplyr)
library(car)
library(data.table)
library(ggplot2)
library(plotly)
library(lubridate)
library(tm)
library(wordcloud)

#Directorio
setwd(file.path("D:", "Programacion", "Python", "Proyecto meneame"))

#Funciones
ts_mensual <- function(db){
  db %>%
    mutate(date = format(fecha_publicacion, "%Y-%m")) %>%
    group_by(date) %>% 
    count(fecha_publicacion) %>% summarise(total=sum(n))
}

ts_anual <- function(db){
  db %>%
    mutate(date = format(fecha_publicacion, "%Y")) %>%
    group_by(date) %>% 
    count(fecha_publicacion) %>% summarise(total=sum(n))
}

sub_set <- function(df, colum, val){
  return (df[df[,colum]==val,])
}

#Cargamos DB
#dt_portada <- read.csv("portada 04-12-2017.csv", sep=";", header = TRUE, encoding="UTF-8")
dt_portada <- fread("portada 04-12-2017.csv", sep=";", header = TRUE, encoding="UTF-8")

#Cambiamos los espacios en blanco por NAN's
dt_portada <- dt_portada %>%
  mutate_if(is.factor, funs(factor(replace(., .=="", NA))))
dt_portada <- dt_portada[rowSums(is.na(dt_portada)) == 0,]

#Declaramos series temporales
dt_portada$fecha_envio <- as.POSIXct(dt_portada$fecha_envio, origin = "1970-01-01") 
dt_portada$fecha_publicacion <- as.POSIXct(dt_portada$fecha_publicacion, origin = "1970-01-01") 

######### WEB DE ORIGEN ######### 
#elmundo.es
dt_portada$web <-recode(dt_portada$web, "c('aula.elmundo.es', 'quiosco.elmundo.orbyt.es', 'elmundo.orbyt.es', 'elmundodeporte.elmundo.es',
                        'elmundomotor.elmundo.es', 'elmundosalud.elmundo.es', 'elmundovino.elmundo.es', 'estaticos.elmundo.es', 
                        'foros.elmundo.es', 'navegante2.elmundo.es', 'sv.elmundo.es', 'rsocial.elmundo.orbyt.es')='elmundo.es'")
#elpais.com
dt_portada$web <-recode(dt_portada$web, "c('bandaancha.elpais.es', 'blogs.elpais.com', 'blogs.smoda.elpais.com', 'ccaa.elpais.com', 
                        'cultura.elpais.com', 'deportes.elpais.com', 'economia.elpais.com', 'elpais.es', 'elviajero.elpais.com', 
                        'eskup.elpais.com', 'foros.elpais.com', 'internacional.elpais.com', 'lacomunidad.elpais.com', 
                        'pda.elpais.com', 'politica.elpais.com', 'smoda.elpais.com', 'sociedad.elpais.com', 
                        'tecnologia.elpais.com', 'tentaciones.elpais.com', 'www-org.elpais.com')='elpais.com'")

#europapress.es
dt_portada$web <-recode(dt_portada$web, "c('europapress.com', 'm.europapress.es', 'amp.europapress.es', 'europapress.cat', 
                        'blogs.europapress.es', 'europapress.tumblr.com', 'europapress.tv', 't.europapress.es')='europapress.es'")

#publico.es
dt_portada$web <-recode(dt_portada$web, "c('blogs.publico.es', 'tremending.publico.es', 'charlas.publico.es', 'video.publico.es', 
                        'especiales.publico.es', 'imagenes.publico-estaticos.es', 'imagenes.publico.es')='publico.es'")

#youtube.com
dt_portada$web <-recode(dt_portada$web, "c('es.youtube.com', 'br.youtube.com', 'mx.youtube.com', 'uk.youtube.com', 'au.youtube.com', 
                        'de.youtube.com', 'ie.youtube.com', 'ru.youtube.com', 'apiblog.youtube.com')='youtube.com'")

#20minutos.es
dt_portada$web <-recode(dt_portada$web, "c('blogs.20minutos.es', 'm.20minutos.es', '20minutos.tv', 'listas.20minutos.es', '20minutos.com', 
                        'clipset.20minutos.es', 'cdn.20minutos.es', 'foros.20minutos.es', 'amp.20minutos.es', 'estaticos.20minutos.es')='20minutos.es'")

#eldiario.es
dt_portada$web <-recode(dt_portada$web, "c('m.eldiario.es', 'vertele.eldiario.es', 'desmemoria.eldiario.es', 'images.eldiario.es')='eldiario.es'")

#elconfidencial.com
dt_portada$web <-recode(dt_portada$web, "c('blogs.elconfidencial.com', 'vanitatis.elconfidencial.com', 'blogs.vanitatis.elconfidencial.com', 
                        'gentleman.elconfidencial.com', 'nocache.elconfidencial.com')='elconfidencial.com'")

#abc.es
dt_portada$web <-recode(dt_portada$web, "c('sevilla.abc.es', 'abcblogs.abc.es', 'videos.abc.es', 'cordoba.abc.es', 
                        'hemeroteca.abc.es', 'participacion.abc.es', 'especiales.abc.es', 'lapalabraescrita.abc.es', 
                        'mundial-futbol.abc.es', 'sevilla-origin.abc.es', 'valencia.abc.es', 'videochat.abc.es', 
                        'abcdesevilla.es', 'abcdesevilla.xlsemanal.com', 'contenidosabcdesevilla.es')='abc.es'")

#cadenaser.com
dt_portada$web <-recode(dt_portada$web, "c('blogs.cadenaser.com', 'lacomunidad.cadenaser.com', 'play.cadenaser.com', 
                        'www-org.cadenaser.com', 'sdmedia.cadenaser.com')='cadenaser.com'")

#lavanguardia.es
dt_portada$web <-recode(dt_portada$web, "c('blogs.lavanguardia.es', 'yaencontre-lavanguardia.es', 'comunidad.lavanguardia.es', 
                        'lavanguardia.com', 'blogs.lavanguardia.com', 'videos.lavanguardia.com', 'enmemoria.lavanguardia.com', 
                        'hagoclic.lavanguardia.com')='lavanguardia.es'")

#lainformacion.com
dt_portada$web <-recode(dt_portada$web, "c('noticias.lainformacion.com', 'blogs.lainformacion.com', 'humor.lainformacion.com', 
                        '233grados.lainformacion.com', 'fotos.lainformacion.com', 'graficos.lainformacion.com', 
                        'videos.lainformacion.com', 'especiales.lainformacion.com', 'bolsa.lainformacion.com', 
                        'mundial.lainformacion.com', 'trabajo.practicopedia.lainformacion.com')='lainformacion.com'")

#vozpopuli.com
dt_portada$web <-recode(dt_portada$web, "c('tendencias.vozpopuli.com', 'bolsa.vozpopuli.com', 
                        'estatico.tendencias.vozpopuli.com', 'proveedor2.vozpopuli.com')='vozpopuli.com'")

#eleconomista.es
dt_portada$web <-recode(dt_portada$web, "c('ecodiario.eleconomista.es', 'ecoteuve.eleconomista.es', 
                        'ecoaula.eleconomista.es', 'informalia.eleconomista.es', 'listas.eleconomista.es')='eleconomista.es'")

#elperiodico.com
dt_portada$web <-recode(dt_portada$web, "c('elperiodico.es', 'epreader.elperiodico.com', 'staging.elperiodico.com', 'beta.elperiodico.es', 
                        'bolsa.elperiodico.com', 'elperiodico.cat', 'momentos.elperiodico.com', 'primerafila.elperiodico.com')='elperiodico.com'")

#elplural.com
dt_portada$web <-recode(dt_portada$web, "c('www2.elplural.com', 'amp.elplural.com', 't.elplural.com')='elplural.com'")

#lavozdegalicia.es
dt_portada$web <-recode(dt_portada$web, "c('blogs.lavozdegalicia.es', 'media.lavozdegalicia.es')='lavozdegalicia.es'")

#rtve.es
dt_portada$web <-recode(dt_portada$web, "c('blog.rtve.es', 'blogs.rtve.es', 'muchachadanui.rtve.es', 'extra.rtve.es', 
                        'eurovision2008.rtve.es', 'encuentrosdigitales.rtve.es', 'pluton.rtve.es', 
                        'resultados-elecciones.rtve.es')='rtve.es'")
#levante-emv.com
dt_portada$web <-recode(dt_portada$web, "c('comunidad.levante-emv.com', 'comunidades.levante-emv.com')='levante-emv.com'")

#gizmodo.com
dt_portada$web <-recode(dt_portada$web, "c('gizmodo.es', 'gizmodo.com', 'sploid.gizmodo.com', 'i.gizmodo.com', 
                        'factually.gizmodo.com', 'gizmodo.com.au', 'gizmodo.co.uk', 'gizmodo.com.br', 
                        'io9.gizmodo.com', 'lego.gizmodo.com', 'paleofuture.gizmodo.com', 'throb.gizmodo.com', 
                        'es.gizmodo.com')='gizmodo.com'")
#yahoo.com
dt_portada$web <-recode(dt_portada$web, "c('es.noticias.yahoo.com', 'es.news.yahoo.com', 'news.yahoo.com', 'es.eurosport.yahoo.com', 
                        'es.finance.yahoo.com', 'espanol.news.yahoo.com', 'es-us.noticias.yahoo.com', 'ar.news.yahoo.com', 
                        'uk.news.yahoo.com', 'es.biz.yahoo.com', 'finance.yahoo.com', 'es.cars.yahoo.com', 'au.news.yahoo.com', 
                        'es.tv.yahoo.com', 'es.vida-estilo.yahoo.com', 'sports.yahoo.com', 'ca.news.yahoo.com', 'es.answers.yahoo.com', 
                        'es.movies.yahoo.com', 'ar.noticias.yahoo.com', 'mx.news.yahoo.com', 'es-us.deportes.yahoo.com', 'es.cine.yahoo.com', 
                        'es.tendencias.yahoo.com', 'es.viajes.yahoo.com', 'espanol.sports.yahoo.com', 'fr.news.yahoo.com', 'yahoo.com', 
                        'asia.news.yahoo.com', 'autos.yahoo.com', 'beta.es.noticias.yahoo.com', 'biz.yahoo.com', 'br.noticias.yahoo.com', 
                        'cl.noticias.yahoo.com', 'en-maktoob.news.yahoo.com', 'es-us.omg.yahoo.com', 'es-us.vida-estilo.yahoo.com', 
                        'es.celebrity.yahoo.com', 'es.groups.yahoo.com', 'es.guide.yahoo.com', 'es.news.launch.yahoo.com', 'es.omg.yahoo.com', 
                        'es.video.yahoo.com', 'es.videogames.games.yahoo.com', 'espanol.pfinance.yahoo.com', 'fe7.story.media.ac4.yahoo.com', 
                        'flickr.mud.yahoo.com', 'fr.voyage.yahoo.com', 'gma.yahoo.com', 'groups.yahoo.com', 'it.notizie.yahoo.com', 
                        'messages.finance.yahoo.com', 'movies.yahoo.com', 'mx.mujer.yahoo.com', 'mx.news.finance.yahoo.com', 
                        'mx.noticias.yahoo.com', 'nz.lifestyle.yahoo.com', 'pe.noticias.yahoo.com', 'potw.news.yahoo.com', 'shine.yahoo.com', 
                        'tech.yahoo.com', 'uk.eurosport.yahoo.com', 've.noticias.yahoo.com', 'video.yahoo.com', 'yodel.yahoo.com')='yahoo.com'")

#bbc.com
dt_portada$web <-recode(dt_portada$web, "c('bbc.co.uk', 'news.bbc.co.uk', 'news8.thdo.bbc.co.uk', 'm.bbc.co.uk', 
                        'm.bbc.com', 'open.bbc.co.uk')='bbc.com'")

#elboletin.com
dt_portada$web <-recode(dt_portada$web, "c('elboletin.es')='elboletin.com'")

#elespanol.com
dt_portada$web <-recode(dt_portada$web, "c('bluper.elespanol.com', 'cronicaglobal.elespanol.com', 'omicrono.elespanol.com', 
                        'diariodeavisos.elespanol.com', 'navarra.elespanol.com', 'elandroidelibre.elespanol.com', 
                        'cocinillas.elespanol.com', 'datos.elespanol.com', 'lab.elespanol.com', 'reportajes.elespanol.com')='elespanol.com'")

#lasexta.com
dt_portada$web <-recode(dt_portada$web, "c('lasextanoticias.com', 'elintermedio.lasexta.com', 'amp.lasexta.com', 'cqc.lasexta.com', 
                        'lasextadeportes.com', 'quevidamastriste.lasexta.com', 'buenafuente.lasexta.com', 'salvados.lasexta.com')='lasexta.com'")

#marca.com
dt_portada$web <-recode(dt_portada$web, "c('jjoo.marca.com', 'marcawas5.recoletos.es', 'marca.es', 'marca.recoletos.com')='marca.com'")

#expansion.com
dt_portada$web <-recode(dt_portada$web, "c('app2.expansion.com', 'nauta360.expansion.com', 'origin-www.expansion.com', 
                        'php.expansion.com', 'blogs.expansion.com')='expansion.com'")

#libertaddigital.com
dt_portada$web <-recode(dt_portada$web, "c('libertaddigital.es', 'findesemana.libertaddigital.com', 'revista.libertaddigital.com', 
                        'blogs.libertaddigital.com', 'esradio.libertaddigital.com', 'exteriores.libertaddigital.com', 
                        'historia.libertaddigital.com', 'libertaddigital.tv', 'libros.libertaddigital.com', 
                        'tv.libertaddigital.com')='libertaddigital.com'")

#terra.es
dt_portada$web <-recode(dt_portada$web, "c('actualidad.terra.es', 'noticias.terra.es', 'noticias.terra.com', 'terra.cl', 'terra.com', 
                        'noticias.terra.com.ar', 'motor.terra.es', 'noticias.terra.com.pe', 'mujer.terra.es', 'terra.org', 
                        'comunidad.terra.es', 'personal.telefonica.terra.es', 'noticias.terra.cl', 'noticias.terra.com.co', 
                        'terra.com.ar', 'terra.com.pe', 'deportes.terra.es', 'economia.terra.com.co', 'entretenimiento.terra.es', 
                        'terra.com.mx', 'eltiempo.terra.com.co', 'blogs.terra.es', 'co.terra.com', 'economia.terra.cl', 
                        'fzhelenmamy.blogs.terra.es', 'megagalerias.terra.cl', 'noticias.terra.com.mx', 'vidayestilo.terra.es', 
                        'ar.terra.com', 'deportes.terra.com.ar', 'deportes.terra.com.co', 'deportesar.terra.com.ar',
                        'economia.terra.com', 'economia.terra.com.ar', 'elcomercio.terra.com.ec', 'enter.terra.com.co', 
                        'entretenimiento.terra.com.ar', 'entretenimiento.terra.com.pe', 'juegos.blogs.terra.es', 'm.terra.cl', 
                        'musica.terra.com.mx', 'musica.terra.es', 'navidadlae.terra.es', 'noticias.terra.com.br', 'noticiascl.terra.cl', 
                        'paginas.terra.com.br', 'pe.terra.com', 'tecnologia.terra.com.br', 'terra.com.pr', 'terra.tv', 'terratv.terra.cl', 
                        'vidayestilo.terra.cl', 'vidayestilo.terra.com.ar', 'www3.terra.com.ar')='terra.es'")

#microsiervos.com
dt_portada$web <-recode(dt_portada$web, "c('wtf.microsiervos.com', 'eco.microsiervos.com', 'avion.microsiervos.com', 'foto.microsiervos.com', 
                        'juegos.microsiervos.com')='microsiervos.com'")

#cincodias.com
dt_portada$web <-recode(dt_portada$web, "c('blogs.cincodias.com')='cincodias.com'")

#genbeta.com
dt_portada$web <-recode(dt_portada$web, "c('genbetadev.com', 'm.genbeta.com', 't.genbeta.com', 'genbetasocialmedia.com')='genbeta.com'")

#xataka.com
dt_portada$web <-recode(dt_portada$web, "c('magnet.xataka.com', 'xatakaciencia.com', 'xatakamovil.com', 'xatakafoto.com', 'm.xataka.com', 
                        'm.magnet.xataka.com', 'xatakandroid.com', 'xatakaon.com', 'm.xatakaciencia.com', 't.xataka.com', 'xatakahome.com', 
                        'm.xatakandroid.com', 'esports.xataka.com', 'm.xatakahome.com', 't.xatakaciencia.com', 't.xatakaon.com', 
                        'xatakawindows.com')='xataka.com'")

#adslzone.net
dt_portada$web <-recode(dt_portada$web, "c('adslzone.tv', 'tv.adslzone.net', 'linux.adslzone.net', 'software.adslzone.net')='adslzone.net'")

#rt.com
dt_portada$web <-recode(dt_portada$web, "c('actualidad.rt.com')='rt.com'")

#telegraph.co.uk
dt_portada$web <-recode(dt_portada$web, "c('blogs.telegraph.co.uk')='telegraph.co.uk'")

#huffingtonpost.es
dt_portada$web <-recode(dt_portada$web, "c('huffingtonpost.com', 'huffingtonpost.co.uk', 'huffingtonpost.fr',
                        'huffingtonpost.ca', 'huffingtonpost.com.au', 'huffingtonpost.com.mx', 'huffingtonpost.it')='huffingtonpost.es'")

#flickr.com
dt_portada$web <-recode(dt_portada$web, "c('farm1.static.flickr.com', 'farm3.static.flickr.com', 'farm2.static.flickr.com', 
                        'static.flickr.com', 'blog.flickr.com', 'blog.flickr.net', 'c2.staticflickr.com', 'farm8.staticflickr.com', 
                        'photos3.flickr.com')='flickr.com'")

#lasprovincias.es
dt_portada$web <-recode(dt_portada$web, "c('bicivalencia.lasprovincias.es', 'blogs.lasprovincias.es', 'campusparty.lasprovincias.es', 
                        'especiales.lasprovincias.es', 'firmas.lasprovincias.es', 'servicios.lasprovincias.es', 'valenciacf.lasprovincias.es', 
                        'www1.lasprovincias.es')='lasprovincias.es'")

#nytimes.com
dt_portada$web <-recode(dt_portada$web, "c('artsbeat.blogs.nytimes.com', 'bits.blogs.nytimes.com', 'cityroom.blogs.nytimes.com', 
                        'dealbook.blogs.nytimes.com', 'dealbook.nytimes.com', 'gadgetwise.blogs.nytimes.com', 'graphics.nytimes.com',
                        'graphics8.nytimes.com', 'green.blogs.nytimes.com', 'greeninc.blogs.nytimes.com', 'krugman.blogs.nytimes.com', 
                        'laughlines.blogs.nytimes.com', 'lens.blogs.nytimes.com', 'mediadecoder.blogs.nytimes.com', 'mobile.nytimes.com',
                        'niemann.blogs.nytimes.com', 'open.nytimes.com', 'opinionator.blogs.nytimes.com', 'pogue.blogs.nytimes.com', 
                        'prescriptions.blogs.nytimes.com', 'projects.nytimes.com', 'query.nytimes.com', 'thecaucus.blogs.nytimes.com', 
                        'thelede.blogs.nytimes.com', 'travel.nytimes.com', 'well.blogs.nytimes.com')='nytimes.com'")

#lasprovincias.es
dt_portada$web <-recode(dt_portada$web, "c('bicivalencia.lasprovincias.es', 'blogs.lasprovincias.es', 'campusparty.lasprovincias.es', 
                        'especiales.lasprovincias.es', 'firmas.lasprovincias.es', 'servicios.lasprovincias.es', 'valenciacf.lasprovincias.es', 
                        'www1.lasprovincias.es')='lasprovincias.es'")

#as.com
dt_portada$web <-recode(dt_portada$web, "c('futbol.as.com', 'masdeporte.as.com', 'motor.as.com', 'www-org.as.com')='as.com'")


#meristation.com
dt_portada$web <-recode(dt_portada$web, "c('zonaforo.meristation.com', 'meristation.as.com', 'meristation.com.mx')='meristation.com'")

#valenciaplaza.com
dt_portada$web <-recode(dt_portada$web, "c('epoca1.valenciaplaza.com')='valenciaplaza.com'")

#formulatv.com
dt_portada$web <-recode(dt_portada$web, "c('blogs.formulatv.com')='formulatv.com'")

#theguardian.com
dt_portada$web <-recode(dt_portada$web, "c('guardian.co.uk', 'observer.guardian.co.uk', 'm.guardian.co.uk', 'technology.guardian.co.uk', 
                        'arts.guardian.co.uk', 'film.guardian.co.uk', 'lifeandhealth.guardian.co.uk', 'politics.guardian.co.uk', 
                        'travel.guardian.co.uk')='theguardian.com'")

#diariodemallorca.es
dt_portada$web <-recode(dt_portada$web, "c('ocio.diariodemallorca.es', 'blog.diariodemallorca.es', 'comunidades.diariodemallorca.es', 
                        'videos.diariodemallorca.es')='diariodemallorca.es'")

#elcorreodigital.com
dt_portada$web <-recode(dt_portada$web, "c('blogs.elcorreodigital.com', 'elcorreodigital.finanzas.com', 'elcorreodigital.hoytecnologia.com',
                        'servicios.elcorreodigital.com', 'elcorreodigital.hoyinversion.com', 'eurocopa2008.elcorreodigital.com', 
                        'cibernauta.elcorreodigital.com', 'hoysport.elcorreodigital.com', 'salud.elcorreodigital.com')='elcorreodigital.com'")

#elcorreo.com
dt_portada$web <-recode(dt_portada$web, "c('blogs.elcorreo.com', 'ciencia.elcorreo.com', 'especiales.elcorreo.com', 'info.elcorreo.com', 
                        'm.elcorreo.com', 'actualidad.elcorreo.com', 'bilbaobasket.elcorreo.com', 'vamosacorrer.elcorreo.com')='elcorreo.com'")

#ideal.es
dt_portada$web <-recode(dt_portada$web, "c('blogs.ideal.es', 'deportebase.ideal.es', 'hoysport.ideal.es', 'salud.ideal.es', 
                        'ubeda.ideal.es')='ideal.es'")

#bandaancha.eu
dt_portada$web <-recode(dt_portada$web, "c('backstage.bandaancha.eu', 'bandaancha.st', 'rwx.bandaancha.st')='bandaancha.eu'")

#infolibre.es
dt_portada$web <-recode(dt_portada$web, "c('blog.infolibre.es', 'elblogdexnet.infolibre.es', 'porlovisto.infolibre.es')='infolibre.es'")

#laverdad.es
dt_portada$web <-recode(dt_portada$web, "c('blog.laverdad.es', 'blogs.laverdad.es', 'canales.laverdad.es', 'lospiesenlatierra.laverdad.es', 
                        'verano.laverdad.es', 'videos.laverdad.es')='laverdad.es'")

#eluniversal.com
dt_portada$web <-recode(dt_portada$web, "c('sociales.eluniversal.com', 'buscador.eluniversal.com', 'deportes.eluniversal.com', 
                        'economia.eluniversal.com', 'eluniversal.com', 'espectaculos.eluniversal.com', 'espectaculos.eluniversal.com', 
                        'internacional.eluniversal.com', 'm.eluniversal.com', 'politica.eluniversal.com', 
                        'sociales.eluniversal.com.com')='eluniversal.com'")

#naukas.com
dt_portada$web <-recode(dt_portada$web, "c('cienciaenelbar.naukas.com', 'danielmarin.naukas.com', 'edocet.naukas.com', 
                        'elprofedefisica.naukas.com', 'francis.naukas.com', 'fuga.naukas.com', 'irreductible.naukas.com', 'jmmulet.naukas.com', 
                        'listadelaverguenza.naukas.com', 'maikelnai.naukas.com', 'mati.naukas.com', 'pacobellido.naukas.com', 
                        'zoologik.naukas.com')='naukas.com'")

#telecinco.es
dt_portada$web <-recode(dt_portada$web, "c('12meses12causas.telecinco.es', 'aquihaytomate.telecinco.es', 'blogs.telecinco.es', 
                        'cine.telecinco.es', 'diariode.telecinco.es', 'dutifri.telecinco.es', 'f1.informativos.telecinco.es', 'gente5.telecinco.es', 
                        'granhermano.telecinco.es', 'informativos.telecinco.es', 'mitele.telecinco.es')='telecinco.es'")

#antena3.com
dt_portada$web <-recode(dt_portada$web, "c('antena3noticias.com', 'blogs.antena3.com', 'foros.antena3.com')='antena3.com'")

#clarin.com
dt_portada$web <-recode(dt_portada$web, "c('blogs.clarin.com', 'edant.clarin.com', 'ieco.clarin.com', 'jjoo.clarin.com', 
                        'old.clarin.com', 'ole.clarin.com', 'revistaenie.clarin.com', 'weblogs.clarin.com')='clarin.com'")

#aldea-irreductible.blogspot.com
dt_portada$web <-recode(dt_portada$web, "c('aldea-irreductible.blogspot.com.es')='aldea-irreductible.blogspot.com'")

#infobae.com
dt_portada$web <-recode(dt_portada$web, "c('america.infobae.com', 'blogs.infobae.com', 'infobae.com.ar', 'opinion.infobae.com', 
                        'saludable.infobae.com')='infobae.com'")

#theinquirer.es
dt_portada$web <-recode(dt_portada$web, "c('es.theinquirer.net', 'theinquirer.es', 'theinquirer.net', 'theinquirer.org', 
                        'uk.theinquirer.net')='theinquirer.es'")

#fogonazos.es
dt_portada$web <-recode(dt_portada$web, "c('fogonazos.blogspot.com')='fogonazos.es'")

#periodistadigital.com
dt_portada$web <-recode(dt_portada$web, "c('blogs.periodistadigital.com')='periodistadigital.com'")

#google.com
dt_portada$web <-recode(dt_portada$web, "c('afp.google.com', 'ap.google.com', 'base.google.com', 'books.google.com', 
                        'canadianpress.google.com', 'chrome.google.com', 'code.google.com', 'desktop.google.com', 
                        'developers.google.com', 'docs.google.com', 'drive.google.com', 'earth.google.com', 'finance.google.com', 
                        'google.com.mt', 'groups.google.com', 'images.google.com', 'labs.google.com', 'mail.google.com', 
                        'maps.google.com', 'music.google.com', 'news.google.com', 'news.google.com.co', 'pack.google.com', 
                        'pages.google.com', 'picasaweb.google.com', 'play.google.com', 'plus.google.com', 'productforums.google.com', 
                        'quickdraw.withgoogle.com', 'research.google.com', 'services.google.com', 'sites.google.com', 'sketchup.google.com', 
                        'spreadsheets.google.com', 'support.google.com', 'translate.google.com', 'video.google.com', 
                        'wave.google.com', 'google.dirson.com')='google.com'")

#eitb.com
dt_portada$web <-recode(dt_portada$web, "c('blog.eitb.com', 'blog.eitb24.com', 'blogseitb.com', 'eitb.com', 'eitb.eus', 'eitb.tv', 
                        'eitb24.com', 'mediablog.eitb24.com')='eitb.com'")

#imgur.com
dt_portada$web <-recode(dt_portada$web, "c('ayesee.imgur.com', 'halytech.imgur.com', 'i.imgur.com', 'imgurfun.net', 
                        'sinasterisco.imgur.com')='imgur.com'")

#diariodesevilla.es
dt_portada$web <-recode(dt_portada$web, "c('diariodesevilla.com')='diariodesevilla.es'")

#tercerainformacion.es
dt_portada$web <-recode(dt_portada$web, "c('blogs.tercerainformacion.es')='tercerainformacion.es'")

#cuatro.com
dt_portada$web <-recode(dt_portada$web, "c('blog.cuatro.com', 'blogs.cuatro.com', 'foros.cuatro.com', 'play.cuatro.com')='cuatro.com'")

#amazings.es
dt_portada$web <-recode(dt_portada$web, "c('amazings.com')='amazings.es'")

#barrapunto.com
dt_portada$web <-recode(dt_portada$web, "c('softlibre.barrapunto.com', 'preguntas.barrapunto.com', 'espana.barrapunto.com', 
                        'empleo.barrapunto.com', 'debian.barrapunto.com', 'ciencia.barrapunto.com', 'ciberderechos.barrapunto.com', 
                        'bp25.barrapunto.com', 'americas.barrapunto.com')='barrapunto.com'")

#wired.com
dt_portada$web <-recode(dt_portada$web, "c('blog.wired.com', 'howto.wired.com', 'news.wired.com', 'wired.co.uk', 'wired.reddit.com', 
                        'wireservice.wired.com')='wired.com'")

#consumer.es
dt_portada$web <-recode(dt_portada$web, "c('escuelas.consumer.es', 'museos.consumer.es', 'revista.consumer.es')='consumer.es'")

#wikipedia.org
dt_portada$web <-recode(dt_portada$web, "c('ca.wikipedia.org', 'en.wikipedia.org', 'es.wikipedia.org', 'eu.wikipedia.org', 
                        'fr.wikipedia.org', 'gl.wikipedia.org', 'it.wikipedia.org', 'mail.wikipedia.org', 
                        'static.wikipedia.org')='wikipedia.org'")

#independent.co.uk
dt_portada$web <-recode(dt_portada$web, "c('environment.independent.co.uk', 'i100.independent.co.uk', 
                        'news.independent.co.uk')='independent.co.uk'")

#mimesacojea.com
dt_portada$web <-recode(dt_portada$web, "c('mimesacojea.blogspot.com')='mimesacojea.com'")

#washingtonpost.com
dt_portada$web <-recode(dt_portada$web, "c('blog.washingtonpost.com', 'blogs.washingtonpost.com', 'knowmore.washingtonpost.com', 
                        'm.washingtonpost.com', 'voices.washingtonpost.com', 'wpcomics.washingtonpost.com')='washingtonpost.com'")

#finanzas.com
dt_portada$web <-recode(dt_portada$web, "c('blogs.finanzas.com', 'foro.finanzas.com', 'laverdad.finanzas.com', 
                        'xlsemanal.finanzas.com')='finanzas.com'")

#engadget.com
dt_portada$web <-recode(dt_portada$web, "c('engadgetmobile.com', 'es.engadget.com', 'spanish.engadget.com', 
                        'transportation.engadget.com')='engadget.com'")

#prnoticias.com
dt_portada$web <-recode(dt_portada$web, "c('prnoticias.es')='prnoticias.com'")

#astroseti.org
dt_portada$web <-recode(dt_portada$web, "c('astrobiologia.astroseti.org', 'ciencia.astroseti.org', 
                        'esa.astroseti.org')='astroseti.org'")

#vice.com
dt_portada$web <-recode(dt_portada$web, "c('broadly.vice.com', 'i-d.vice.com', 'motherboard.vice.com', 'news.vice.com', 
                        'thecreatorsproject.vice.com')='vice.com'")

#larepublica.com
dt_portada$web <-recode(dt_portada$web, "c('afindemes.republica.com', 'blogs.republica.com')='larepublica.com'")

#cnn.com
dt_portada$web <-recode(dt_portada$web, "c('apple20.blogs.fortune.cnn.com', 'archives.cnn.com', 'brainstormtech.blogs.fortune.cnn.com', 
                        'cnn.netscape.cnn.com', 'cnnespanol.cnn.com', 'cnnpressroom.blogs.cnn.com', 'edition.cnn.com', 'ireport.cnn.com', 
                        'larrykinglive.blogs.cnn.com', 'mexico.cnn.com', 'money.cnn.com', 'news.blogs.cnn.com', 'sportsillustrated.cnn.com', 
                        'us.cnn.com')='cnn.com'")

#boredpanda.com
dt_portada$web <-recode(dt_portada$web, "c('boredpanda.es', 'boredpanda.org')='boredpanda.com'")

#nasa.com
dt_portada$web <-recode(dt_portada$web, "c('antwrp.gsfc.nasa.gov', 'apod.nasa.gov', 'blogs.nasa.gov', 'ciencia.nasa.gov', 'climate.nasa.gov', 
                        'data.giss.nasa.gov', 'earthasart.gsfc.nasa.gov', 'earthobservatory.nasa.gov', 'eobglossary.gsfc.nasa.gov', 
                        'eol.jsc.nasa.gov', 'eyes.jpl.nasa.gov', 'imagine.gsfc.nasa.gov', 'ipac.jpl.nasa.gov', 'jpl.nasa.gov', 
                        'lars-lab.jpl.nasa.gov', 'm.earthobservatory.nasa.gov', 'map.gsfc.nasa.gov', 'mars.nasa.gov', 
                        'marsrovers.jpl.nasa.gov', 'microgravity.grc.nasa.gov', 'neo.jpl.nasa.gov', 'opensource.arc.nasa.gov', 
                        'photojournal.jpl.nasa.gov', 'rapidfire.sci.gsfc.nasa.gov', 'saturn.jpl.nasa.gov', 'science.nasa.gov', 
                        'software.nasa.gov', 'sohowww.nascom.nasa.gov', 'solarsystem.nasa.gov', 'space.jpl.nasa.gov', 'spaceflight.nasa.gov', 
                        'sunearth.gsfc.nasa.gov', 'svs.gsfc.nasa.gov', 'visibleearth.nasa.gov')='nasa.gov'")

#periodismohumano.com
dt_portada$web <-recode(dt_portada$web, "c('alianzas.periodismohumano.com', 'consumeymuere.periodismohumano.com', 
                        'guatemalacomunitaria.periodismohumano.com', 'losrostrosdelosrecortes.periodismohumano.com', 
                        'minotauro.periodismohumano.com', 'pandoras.periodismohumano.com', 'polvodeestrellas.periodismohumano.com', 
                        'tomalapalabra.periodismohumano.com', 'traslapolitica.periodismohumano.com', 
                        'trata.periodismohumano.com')='periodismohumano.com'")

#univision.com
dt_portada$web <-recode(dt_portada$web, "c('dinero.univision.com', 'entretenimiento.univision.com', 'feeds.univision.com', 
                        'nfl.univision.com', 'noticias.univision.com')='univision.com'")

#reuters.com
dt_portada$web <-recode(dt_portada$web, "c('es.reuters.com', 'reuters.com', 'lta.reuters.com', 'es.today.reuters.com', 
                        'lta.today.reuters.com', 'reuters.es', 'uk.reuters.com', 'today.reuters.com', 'blogs.reuters.com', 
                        'af.reuters.com', 'mobile.reuters.com', 'today.reuters.co.uk', 'about.reuters.com', 'in.reuters.com', 
                        'in.today.reuters.com', 'africa.reuters.com', 'ca.reuters.com', 'fr.reuters.com', 'i.today.reuters.com',
                        'mx.reuters.com', 'newsandinsight.thomsonreuters.com', 'us.mobile.reuters.com')='reuters.com'")

#Gráfico de nº de portadas
webs <- as.data.frame(table(dt_portada$web))
webs <- webs[order(-webs$Freq),] 
rownames(webs) <- NULL
table(webs$Freq)

#Estadisticos
summary(webs$Freq)

#Gráfico de origen
ggplot(data=webs[c(1:25),], aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", fill="gray28") + theme_classic() +
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)+
   theme(axis.text.x = element_text(angle = 40, hjust = 1, size=12))  + 
  scale_y_continuous(expand = c(0,0)) + labs(x = "Origen", y="Nº de noticias en portada", title="Origen de las noticias de portada")
ggsave("origen_noticias.png", width = 14, height = 6)

#Gráfico de árbol de usuarios
dt_tree <- webs %>% group_by(Var1) %>% summarise(n=sum(Freq))
dt_tree$Var1 <- as.character(dt_tree$Var1)
dt_tree$Var1[dt_tree$n < 250] <- "otros"
dt_tree <- dt_tree %>% group_by(Var1) %>% summarise(n=sum(n))


p1 <- plot_ly(dt_tree, labels = ~(factor(Var1)), values = ~n, type = "pie", textinfo = 'none') %>% 
  layout(title = "Medios con mas noticias en portada", showlegend = T, font=list(size = 13))
p1
#api_create(p1, filename = "men_orig_port")

#Liberamos memoria borrando variables
rm(dt_tree, webs)



######### Usuarios ######### 
df_users <- as.data.frame(sort(table(dt_portada$usuario), decreasing = T))
table(df_users$Freq)

#Estadisticos
summary(df_users$Freq)

#Gráfico de usuarios
ggplot(data=df_users[c(1:25),], aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", fill="gray28") + theme_classic() +
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size=12))  + 
  scale_y_continuous(expand = c(0,0)) + labs(x = "Usuario", y="Nº de noticias en portada", title="Usuarios con mas noticias en portada")
ggsave("usuarios.png", width = 14, height = 6)

#Gráfico de tarta de usuarios
dt_tree <- df_users %>% group_by(Var1) %>% summarise(n=sum(Freq))
dt_tree$Var1 <- as.character(dt_tree$Var1)
dt_tree$Var1[dt_tree$n < 300] <- "otros"
dt_tree <- dt_tree %>% group_by(Var1) %>% summarise(n=sum(n))


p2 <- plot_ly(dt_tree, labels = ~(factor(Var1)), values = ~n, type = "pie", rotation=320, textinfo = 'none') %>% 
  layout(title = "Usuarios con mas noticias en portada", showlegend = T, font=list(size = 13))  
p2
#api_create(p2, filename = "men_users")

#Liberamos memoria borrando variables
rm(dt_tree, df_users)


######### Sub ######### 
#Estandarizamos los datos anteriores a la implementacion de los subs el 27 de marzo de 2014
dt_portada$sub[dt_portada$fecha_publicacion < "2014-03-26"] <-"mnm"

#Eliminamos las noticias no clasificadas
dt_sub <- dt_portada[!dt_portada$sub == "mnm", ] 

#Dejamos las noticias por sub y año
dt_sub <- dt_sub[,c(8, 16)] %>%
  group_by(month=floor_date(fecha_publicacion, "year"), sub) %>% select(-c(fecha_publicacion)) %>% 
  mutate(unique_types = n_distinct(sub))
dt_sub <- as.data.frame(table(dt_sub$sub, dt_sub$month))
dt_sub <- dt_sub[!dt_sub$Freq == 0,]

#Nº de subs con noticias en portada
length(unique(dt_sub$Var1))

#Total de noticias en portada de cada sub
dt_pie <- dt_sub %>% group_by(Var1) %>% summarise(n=sum(Freq))
dt_pie$Var1 <- as.character(dt_pie$Var1)
dt_pie$Var1[dt_pie$n < 70] <- "otros"
dt_pie <- dt_pie %>% group_by(Var1) %>% summarise(n=sum(n))

p3 <- plot_ly(dt_pie, labels = ~(factor(Var1)), values = ~n, type = "pie") %>% 
  layout(title = "Subs a los que pertenecen las noticias de portada", showlegend = T, font=list(size = 13))
p3
#api_create(p3, filename = "men_subs")

rm(dt_pie, dt_sub)


######### Series temporales de origen de noticias ######### 
#Evolucion mensual de noticias de portada
mes <- ts_mensual(dt_portada)
mes$date <- as.Date(paste(mes$date, "01", sep="-"), "%Y-%m-%d")
mes <- mes[!mes$date == "2017-12-01",]
plot_ly(mes, x = ~date, y = ~total, mode = 'lines', type = "scatter") %>% 
  layout(title = "Evolución mensual de las noticias de portada", showlegend = F, font=list(size = 13), xaxis=list(title="Año"))

#Evolucion anual de noticias de portada
ano <- ts_anual(dt_portada)
ano$date <- as.Date(paste(ano$date, "01-01", sep="-"), "%Y-%m-%d")
plot_ly(ano, x = ~date, y = ~total, mode = 'lines', type = "scatter") %>% 
  layout(title = "Evolución anual de las noticias de portada", showlegend = F, font=list(size = 13), xaxis=list(title="Año"))

#Liberamos espacio
rm("ano", "mes")

MyMerge <- function(x, y){
  #Funcion que fusiona df
  df <- merge(x, y,  by="date", all=T)
  return(df)
}

gen_df_ts_m <- function(x, anual=FALSE){
  #Funcion que genera el DF con el envío mensual de noticias. Requiere función MyMerge. 
  #Requiere x, que es el nº de medios que queremos en orden decreciente. Si anual = TRUE, se hace la serie anual en vez de mensual
  webl <- names(sort(table(dt_portada$web), decreasing = T))[1:x]
  
  l_obj <- c()
  l_str <- c()
  for(i in webl){
    ar <- strsplit(i, "[.]")
    ar <- gsub("-", "_", ar[[1]][1])
    if(anual==T){
      eval(parse(text=paste("ts_a_", ar, " <- dt_portada %>% sub_set('web', '", i, "') %>% ts_anual ", sep="")))
      ini <- "ts_a_"
    }else{
    eval(parse(text=paste("ts_m_", ar, " <- dt_portada %>% sub_set('web', '", i, "') %>% ts_mensual ", sep="")))
      ini <- "ts_m_"
    }
    
    #Construccion lista objetos
    if(length(l_obj) == 0){
      l_obj <- paste(ini, ar, sep="")
    }else {
      l_obj <- paste(l_obj, paste(ini, ar, sep=""), sep=",")
    }
    
    #Construcción lista strings
    if(length(l_str) == 0){
      l_str <- paste(paste("'", ini, sep=""), ar, "'",sep="")
    }else {
      l_str <- paste(l_str, paste(paste("'", ini, sep=""), ar, "'",sep=""), sep=",")
    }
  }
  l_obj <- paste("list(", l_obj, ")", sep="")
  l_str <- paste("c(", l_str, ")", sep="")
  
  ts_m_df20 <- eval(parse(text=l_obj))
  media_20 <- eval(parse(text=l_str))
  
  df_ts_m_20 <- Reduce(MyMerge, ts_m_df20)
  colnames(df_ts_m_20) <- c("date", media_20)
  df_ts_m_20[is.na(df_ts_m_20)] <- 0
  if(anual==T){
    df_ts_m_20$date  <- as.Date(paste(df_ts_m_20$date , '01-01', sep='-'), '%Y-%m-%d')
  }else{
    df_ts_m_20$date  <- as.Date(paste(df_ts_m_20$date , '01', sep='-'), '%Y-%m-%d')
  }
  
  df_ts_m_20 <- df_ts_m_20[!df_ts_m_20$date == "2017-12-01",]
  return(df_ts_m_20)
}

gen_ts_m_plot <- function(dataframe_ts){
  webl <- names(sort(table(dt_portada$web), decreasing = T))[1:length(dataframe_ts)]
  
  AEDE <- c('ts_m_elpais', 'ts_m_elmundo','ts_m_eleconomista', 'ts_m_lavanguardia', 'ts_m_abc', 'ts_m_elperiodico', 'ts_m_marca',
            'ts_m_lavozdegalicia', 'ts_m_levante_emv',  'ts_m_expansion', 'ts_m_elcorreo', 'ts_m_cincodias', 'ts_m_larazon', 
            'ts_a_elpais', 'ts_a_elmundo','ts_a_eleconomista', 'ts_a_lavanguardia', 'ts_a_abc', 'ts_a_elperiodico', 'ts_a_marca',
            'ts_a_lavozdegalicia', 'ts_a_levante_emv',  'ts_a_expansion', 'ts_a_elcorreo', 'ts_a_cincodias', 'ts_a_larazon')
  
  for(i in (1:(length(dataframe_ts)-1))){
    if(colnames(dataframe_ts)[i+1] %in% AEDE){
      ar <- paste("add_trace(dt_portada, y = ~", colnames(dataframe_ts)[i+1], ", legendgroup = 'AEDE', name = '", webl[i], "')", sep="")
    }else{
      ar <- paste("add_trace(dt_portada, y = ~", colnames(dataframe_ts)[i+1], ", name = '", webl[i], "')", sep="")
    }
    
    if(exists("l_plots") == F){
      l_plots <- ar
    }else {
      l_plots <- paste(l_plots, ar, sep=" %>% ")
    }
  }
  
  return(eval(parse(text=paste("plot_ly(dataframe_ts, x = ~date, y = rowSums(dataframe_ts[2:length(dataframe_ts)]), mode = 'lines', type = 'scatter', name = 'Total de envíos') %>% 
                               layout(title = 'Evolucion temporal', showlegend = T, xaxis=list(title='Fecha'), yaxis=list(title='Nº de portadas'))", 
                        l_plots, sep=" %>%"))))
}

#Generamos el gráfico mensual y anual de envíos
dt_ts_m <- gen_df_ts_m(30)
p4 <- gen_ts_m_plot(dt_ts_m)
p4
#api_create(p4, filename = "men_tsm")

dt_ts_a <- gen_df_ts_m(30, T)
p5 <- gen_ts_m_plot(dt_ts_a)
p5
#api_create(p5, filename = "men_tsa")

#Liberamos memoria
rm(dt_ts_a, dt_ts_m)


######### Tiempo que tardan en subir las noticias ######### 
#Creamos un nuevo df con las noticias y el tiempo que tardan en subir
dt_subida <- dt_portada[, c(4, 5, 16)]
dt_subida$t_subida_min <- as.numeric(difftime(dt_portada$fecha_publicacion, dt_portada$fecha_envio), units="mins")
dt_subida$t_subida_horas <- as.numeric(difftime(dt_portada$fecha_publicacion, dt_portada$fecha_envio), units="hours")
dt_subida$t_subida_dia <- as.numeric(difftime(dt_portada$fecha_publicacion, dt_portada$fecha_envio), units="days")

print("Media (En horas)")
mean(dt_subida$t_subida_horas)
print("Maximo (En dias)")
max(dt_subida$t_subida_dia)
print("Minimo (En minutos)")
min(dt_subida$t_subida_min)

plot_ly(type = "box", width = 450, height = 500) %>% 
  layout(autosize = F, title = "Horas hasta llegar a portada", showlegend = F, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Nº de Horas")) %>%
  add_boxplot(data=dt_subida, y = ~t_subida_horas, name = "Todas") 

#Por origen
dt_subida_webs <- dt_subida[dt_subida$web %in% names(sort(table(dt_portada$web), decreasing = T))[1:15], ]

plot_ly(dt_subida_webs, y = ~t_subida_horas, color = ~web, type = "box", xaxis = list(tickangle = 45)) %>% 
  layout(title = "Horas hasta llegar a portada | Por origen", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Nº de Horas"), margin = list(b = 110))


#Por subs
#Eliminamos las noticias no clasificadas
dt_subida_subs <- dt_subida[!dt_subida$sub == "mnm", ] 
dt_subida_subs[!dt_subida_subs$sub %in% c("actualidad", "astronomia", "ciencia", "cultura", "ocio", "tecnología"), "sub"] <- "otros" 

p6 <- plot_ly(dt_subida_subs, y = ~t_subida_horas, color = ~sub, type = "box", xaxis = list(tickangle = 45)) %>% 
  layout(title = "Horas hasta llegar a portada | Por sub", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Nº de Horas"))
p6
#export(p6, "subs.png")
#api_create(p6, filename = "men_time_sub")


#Por usuario
dt_subida_users <- dt_subida[dt_subida$usuario %in% c("mezvan", "Tanatos", "Ratoncolorao", "jm22381", "camachosoft", "juvenal", "Ripio", "--136875--", "disconubes", "aberron"), ]

plot_ly(dt_subida_users, y = ~t_subida_horas, color = ~usuario, type = "box", xaxis = list(tickangle = 45)) %>% 
  layout(title = "Horas hasta llegar a portada | Por usuario", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Nº de Horas"), margin = list(b = 70))

#Liberamos memoria
rm(dt_subida, dt_subida_users, dt_subida_webs, dt_subida_subs)



######### Votos, karma y clicks ######### 
dt_karma <- dt_portada[, c(4, 5, (9:16))]
dt_karma$clicks <- as.integer(dt_karma$clicks)
dt_karma$comentarios <- as.integer(dt_karma$comentarios)
summary(dt_karma[, c(3:9)])

p7 <- plot_ly(type = "box") %>% 
  layout(autosize = F, title = "Nº de votos | Total de karma", showlegend = F, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Total")) %>%
  add_boxplot(data=dt_karma, y = ~meneos, name = "Meneos") %>%
  add_boxplot(data=dt_karma, y = ~votos_positivos, name = "V. Positivos") %>%
  add_boxplot(data=dt_karma, y = ~votos_anonimos, name = "V. Anónimos") %>%
  add_boxplot(data=dt_karma, y = ~votos_negativos, name = "V. Negativos") %>%
  add_boxplot(data=dt_karma, y = ~karma, name = "Karma") %>%
  add_boxplot(data=dt_karma, y = ~comentarios, name = "Comentarios")
p7
#export(p7, "stats.png")
#api_create(p7, filename = "men_stats")

#Por origen  
dt_karma_webs <- dt_karma[dt_karma$web %in% names(sort(table(dt_portada$web), decreasing = T))[1:15], ]

  #Votos negativos
  plot_ly(dt_karma_webs, y = ~votos_negativos, color = ~web, type = "box", xaxis = list(tickangle = 45)) %>% 
    layout(title = "Votos negativos | Por origen", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Nº de votos"), margin = list(b = 110))
  
  #Meneos
  plot_ly(dt_karma_webs, y = ~meneos, color = ~web, type = "box", xaxis = list(tickangle = 45)) %>% 
    layout(title = "Meneos | Por origen", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Nº de meneos"), margin = list(b = 110))
  
  #Karma
  plot_ly(dt_karma_webs, y = ~karma, color = ~web, type = "box", xaxis = list(tickangle = 45)) %>% 
    layout(title = "Karma | Por origen", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Total de karma"), margin = list(b = 110))

  
#Por usuario
dt_karma_users <- dt_karma[dt_karma$usuario %in% c("mezvan", "Tanatos", "Ratoncolorao", "jm22381", "camachosoft", "juvenal", "Ripio", "--136875--", "disconubes", "aberron"), ]
  
  #Votos negativos
  plot_ly(dt_karma_users, y = ~votos_negativos, color = ~usuario, type = "box", xaxis = list(tickangle = 45)) %>% 
    layout(title = "Votos negativos | Por usuario", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Nº de votos"), margin = list(b = 70))
  
  #Meneos
  plot_ly(dt_karma_users, y = ~meneos, color = ~usuario, type = "box", xaxis = list(tickangle = 45)) %>% 
    layout(title = "Meneos | Por usuario", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Nº de meneos"), margin = list(b = 70))
  
  #Karma
  p8 <- plot_ly(dt_karma_users, y = ~karma, color = ~usuario, type = "box", xaxis = list(tickangle = 45)) %>% 
    layout(title = "Karma | Por usuario", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Total de karma"), margin = list(b = 70))  
  p8
  #export(p8, "users_karma.png")
  
  
#Por subs
#Eliminamos las noticias no clasificadas
dt_karma_subs <- dt_karma[!dt_karma$sub == "mnm", ] 
dt_karma_subs[!dt_karma_subs$sub %in% c("actualidad", "astronomia", "ciencia", "cultura", "ocio", "tecnología"), "sub"] <- "otros" 

  #Votos negativos
  plot_ly(dt_karma_subs, y = ~votos_negativos, color = ~sub, type = "box", xaxis = list(tickangle = 45)) %>% 
    layout(title = "Votos negativos | Por sub", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Nº de votos"), margin = list(b = 50))
  
  #Meneos
  p9 <- plot_ly(dt_karma_subs, y = ~meneos, color = ~sub, type = "box", xaxis = list(tickangle = 45)) %>% 
    layout(title = "Meneos | Por sub", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Nº de meneos"), margin = list(b = 50))
  p9
  #export(p9, "subs_meneos.png")
  
  #Karma
  plot_ly(dt_karma_subs, y = ~karma, color = ~sub, type = "box", xaxis = list(tickangle = 45)) %>% 
    layout(title = "Karma | Por sub", showlegend = T, font=list(size = 13), xaxis=list(title=""), yaxis=list(title="Total de karma"), margin = list(b = 50))

  
#Evolucion temporal votos y karma
gen_ts_month_values<- function(x) {
  dt_portada$meneos <- as.integer(dt_portada$meneos)
  dt_portada$votos_positivos <- as.integer(dt_portada$votos_positivos)
  dt_portada$votos_negativos <- as.integer(dt_portada$votos_negativos)
  dt_portada$karma <- as.integer(dt_portada$karma)
  dt_portada$clicks <- as.integer(dt_portada$clicks)
  dt_portada$votos_anonimos <- as.integer(dt_portada$votos_anonimos)
  sal <- dt_portada %>%
    mutate(date = format(fecha_publicacion, "%Y-%m")) %>%
    group_by(date) %>% summarise(tot_men=sum(meneos), tot_pos=sum(votos_positivos), tot_anom=sum(votos_anonimos), tot_neg=sum(votos_negativos), tot_kar=sum(karma), tot_clicks=sum(clicks))
  sal <- sal[!sal$date == "2017-12",]
  
  plot_ly(sal, x = ~date, y = ~tot_men, mode = 'lines', type = 'scatter', name = 'Meneos') %>% 
    layout(title = 'Evolucion temporal', showlegend = T,  yaxis=list(title='Valor total'), xaxis=list(title='')) %>%
    add_trace(sal, y = ~tot_pos, name = 'Votos Positivos') %>% 
    add_trace(sal, y = ~tot_anom, name = 'Votos Anonimos') %>% 
    add_trace(sal, y = ~tot_neg, name = 'Votos Negativos') %>% 
    add_trace(sal, y = ~tot_kar, name = 'Karma') %>% 
    add_trace(sal, y = ~tot_clicks, name = 'Clicks')
}


p10 <- gen_ts_month_values()
p10
#export(p10, "subs.png")
#api_create(p10, filename = "ts_stats")


#Liberamos memoria
rm(dt_karma, dt_karma_subs, dt_karma_users, dt_karma_webs)



######### Nubes de palabras ######### 
rmstrange <- function(x) stringr::str_replace_all(x,"[^[:graph:]]", " ")
rmacentos <- function(x)  chartr("áàâäéëèêíïìîóöòôúüùûñ", "aaaaeeeeiiiioooouuuun", x)

gen_cloud <- function(dataf,x){
  #Titulares - Total
  corpus_tit <- Corpus(VectorSource(dataf), readerControl = list(reader = readPlain, language = "es"))
  corpus_tit <- tm_map(corpus_tit, content_transformer(removePunctuation))
  corpus_tit <- tm_map(corpus_tit, content_transformer(tolower))
  corpus_tit <- tm_map(corpus_tit, content_transformer(rmstrange))
  corpus_tit <- tm_map(corpus_tit, content_transformer(rmacentos))
  corpus_tit <- tm_map(corpus_tit, removeWords, c(stopwords("spanish"), "eng", "cómo", "ser", "ing", "pide", "tras", "así", "dice", "solo", "cada", "gran", "asi", "â€œ", "casi",  "the", "100", "mas"))
  corpus_tit <- tm_map(corpus_tit, content_transformer(stripWhitespace))
  corpus_tit <- tm_map(corpus_tit, content_transformer(trimws))
  
  corpus_tit <- TermDocumentMatrix(corpus_tit)
  
  corpus_tit <- removeSparseTerms(corpus_tit, .997)
  findFreqTerms(corpus_tit, lowfreq=1)                             
  
  corpus_tit <- weightTfIdf(corpus_tit,normalize=FALSE)
  corpus_tit <- as.matrix(corpus_tit)
  dim(corpus_tit)
  summary(rowSums(corpus_tit))
  
  corpus_tit <- sort(rowSums(corpus_tit), decreasing=TRUE)
  if(dataf==dt_portada$noticia){
  corpus_tit <- corpus_tit[-c(28)]
  names(corpus_tit) <- c("años", "España", "euros", "millones", "dos", "gobierno", "Madrid", "mundo", "policía","Rajoy", "google", 
                         "internet", "nuevo", "EE.UU", "hace", "historia", "ley", "tres","humor", "españoles", "nueva", "puede", "hombre", 
                         "español", "día", "primera", "año","mujer", "guerra", "personas", "vídeo", "PSOE", "país", "vida", "juez", "caso",
                         "vez", "sgae", "hacer", "europa", "casa", "primer", "crisis", "española", "dinero","menos", "después", "denuncia", 
                         "quiere", "muere", "niños", "agua", "cuatro", "muerte","segun", "linux", "fotos", "Barcelona", "pagar", "sistema", 
                         "mundial", "mayor", "ahora","Valencia", "joven", "Aguirre", "coche", "presidente", "web", "trabajo", "cárcel", 
                         "libre","horas", "mejor", "civil", "china", "ayuntamiento", "podemos", "real", "detenido", "luz","cinco", "público", 
                         "pública", "Zapatero", "diez", "hoy", "días", "ETA", "podría","estan", "imágenes", "alcalde", "windows", "partido", 
                         "microsoft", "congreso", "iglesia", "meses","tierra", "empresa", "descubren", "mujeres", "jóvenes", "Francia", 
                         "Alemania", "deja", "niño","científicos", "trabajadores", "red", "banco", "cat", "Rey", "software", "foto", 
                         "gana","guardia", "será", "anuncia", "seguridad", "tener", "nunca", "datos", "energía", "grupo","corrupción",
                         "canon", "nacional", "paro", "piden", "fin", "ciudad", "veces", "precio","familia", "hijo", "gratis", "también", 
                         "huelga", "grandes", "Bárcenas", "seis", "madre","dólares", "gente", "niña", "campana", "bajo", "unidos", "empresas",
                         "cáncer", "ver","avión", "viñeta", "sol", "carta", "elecciones", "Japón", "sanidad", "cambio", "tiempo","parte", 
                         "padre", "Israel", "toda", "inglés", "TVE", "calle", "medio", "metro","mes", "usuarios", "prisión", "solar", "uso", 
                         "amenaza", "vuelve", "ciencia", "pueblo","sueldo", "telefónica", "prensa", "muertos", "ministro", "poder", "mientras")
  }
  png(paste("wc_", x, ".png", sep=""), width=1500,height=1500, res = 300)
  wordcloud(words=names(corpus_tit), freq=corpus_tit, random.order = FALSE, colors = RColorBrewer::brewer.pal(8, "Set2"), max.words=700)
  dev.off()
}


#Todas las noticias
gen_cloud(dt_portada$noticia, "total")

#Por año
for(i in (2005:2017)){
  cat("Generando nube de palabras del año -> ", i, "\n")
  df <- dt_portada[(dt_portada$fecha_publicacion > paste(as.character(i),"-01-01", sep="") & dt_portada$fecha_publicacion < paste(as.character(i+1),"-01-01", sep="")), 2] 
  gen_cloud(df, as.character(i))
}

#200 con mas clicks
df <- dt_portada[complete.cases(dt_portada[,10]), ]
df$clicks <- as.integer(df$clicks)
df <- df[order(df$clicks, decreasing = T),][c(1:200), 2]
gen_cloud(df, "200_mas_clicks")

#200 con mas meneos
df <- dt_portada[order(dt_portada$meneos, decreasing = T),][c(1:200), 2]
gen_cloud(df, "200_mas_meneos")

#200 con mas karma
df <- dt_portada[order(dt_portada$karma, decreasing = T),][c(1:200), 2]
gen_cloud(df, "200_mas_karma")

#200 con mas votos negativos
df <- dt_portada[order(dt_portada$votos_negativos, decreasing = T),][c(1:200), 2]
gen_cloud(df, "200_mas_negativos")

#200 con mas comentarios
df <- dt_portada[order(dt_portada$comentarios, decreasing = T),][c(1:200), 2]
gen_cloud(df, "200_mas_comentarios")

#200 con mas votos positivos
df <- dt_portada[order(dt_portada$votos_positivos, decreasing = T),][c(1:200), 2]
gen_cloud(df, "200_mas_positivos")

#200 con mas votos anonimos
df <- dt_portada[order(dt_portada$votos_anonimos, decreasing = T),][c(1:200), 2]
gen_cloud(df, "200_mas_anonimos")