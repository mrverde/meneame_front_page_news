# -*- coding: utf-8 -*-
import scrapy


class PortadaSpider(scrapy.Spider):
    name = 'portada'
    allowed_domains = ['meneame.net']
    start_urls = ['https://meneame.net/']

    def parse(self, response):
      
      def extraer(field):
        if field:
          pass
        else:
          field = ''
        return field
        
      news = response.xpath('.//*[@class="news-summary"]')
      for new in news:
        try:
          index = extraer(new.xpath('.//h2/a/@class').extract_first())
          index = extraer(index.replace('l:', ''))
        except:
          index = ''
          
        try:
          noticia = extraer(new.xpath('.//h2/a/text()').extract_first())
          noticia = extraer(noticia.replace('"','-').replace("'", '-').replace(';', ','))
        except:
          noticia = ''
        
        try:
          link_n = extraer(new.xpath('.//h2/a/@href').extract_first())
          link_n = link_n + " "
        except:
          link_n = ''
          
        try:
          web = extraer(new.xpath('.//*[@class="showmytitle"]//text()').extract_first())
        except:
          web = ''
        
        try:
          user = extraer(new.xpath('.//*[@class="news-submitted"]/a/text()').extract_first())
          user = extraer(user.replace(';', ','))
        except:
          user = ''
        
        try:
          id_user = extraer(new.xpath('.//*[@class="news-submitted"]/a/@class').extract_first())
          id_user = extraer(id_user.replace('tooltip u:', ''))
        except:
          id_user = ''
          
        try:
          f_envio = extraer(new.xpath('.//*[@class="ts visible"]/@data-ts').extract()[0::2][0])
        except:
          f_envio = ''
        
        try:
          f_pub = extraer(new.xpath('.//*[@class="ts visible"]/@data-ts').extract()[1::2][0])
        except:
          f_pub = ''
        
        try:
          meneos = extraer(new.xpath('.//*[@class="votes"]/a/text()').extract_first())
        except:
          meneos = ''
          
        try:
          clicks = extraer(new.xpath('.//*[@class="clics"]/text()').extract_first())
          clicks = extraer(clicks.replace(' ', '').replace('clics', ''))
        except:
          clicks = ''
          
        try:
          coment = extraer(new.xpath('.//*[@class="comments"]/text()')[1::2][0].extract())
          coment = extraer(coment.replace(' ', '').replace('comentarios', ''))
        except:
          coment = ''
          
        try:
          v_pos = extraer(new.xpath('.//*[@class="votes-up"]/span/strong/text()')[1::2][0].extract())
        except:
          v_pos = ''
          
        try:
          v_anom = extraer(new.xpath('.//*[@class="wideonly votes-anonymous"]/span/strong/text()')[1::2][0].extract())
        except:
          v_anom = ''
          
        try:
          v_neg =  extraer(new.xpath('.//*[@class="votes-down"]/span/strong/text()')[1::2][0].extract())
        except:
          v_neg = ''
        
        try:
          karma = extraer(new.xpath('.//*[@class="karma-value"]/text()')[1::2][0].extract())
          karma = extraer(karma.replace(' ', ''))
        except:
          karma = ''
          
        try:
          seccion = extraer(new.xpath('.//*[@class="subname"]/text()')[1::2][0].extract())
          seccion = extraer(seccion.replace(' ', ''))
        except:
          seccion = ''
        
        try:
          extracto = extraer(new.xpath('.//*[@class="news-content"]/text()').extract_first())
          extracto = extraer(extracto.replace(';', ',').replace('"','-').replace("'", '-'))
        except:
          extracto = ''
        

                  
        yield {'index': index,
               'noticia': noticia, 
               'link_noticia': link_n,
               'web': web,
               'usuario': user,
               'id_usuario' : id_user,
               'fecha_envio' : f_envio,
               'fecha_publicacion' : f_pub,
               'meneos' : meneos,
               'clicks' : clicks,
               'comentarios': coment,
               'votos_positivos' : v_pos,
               'votos_anonimos' : v_anom,
               'votos_negativos' : v_neg,
               'karma' : karma,
               'sub' : seccion,
               'extracto' : extracto}
      
      sig_pag =  response.xpath('.//*[contains(text(), "siguiente »")]/@href').extract_first()
      abs_sig_pag = response.urljoin(sig_pag)
      yield scrapy.Request(abs_sig_pag)