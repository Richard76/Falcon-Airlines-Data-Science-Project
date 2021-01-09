window["viz_KHG1hGM3"]= {"name":"KHG1hGM3","displayName":"Inflightwifi_service","dependencies":["Full_Dataset"],"transformName":"Full_Dataset_iXJ7aWP4","options":{"marker":"bar","x":{"name":"Inflightwifi_service","validName":"`Inflightwifi_service`","isNumeric":false,"isDate":false,"type":"factor","func":"none","validLabel":"c1","label":"c1"},"displayName":"Inflightwifi_service","columns":[{"name":"Satisfaction","type":"factor","escapedName":"Satisfaction","isNumeric":false,"isDate":false},{"name":"Gender","type":"factor","escapedName":"Gender","isNumeric":false,"isDate":false},{"name":"CustomerType","type":"factor","escapedName":"CustomerType","isNumeric":false,"isDate":false},{"name":"TypeTravel","type":"factor","escapedName":"TypeTravel","isNumeric":false,"isDate":false},{"name":"Class","type":"factor","escapedName":"Class","isNumeric":false,"isDate":false},{"name":"Age","type":"numeric","escapedName":"Age","isNumeric":true,"isDate":false},{"name":"Flight_Distance","type":"numeric","escapedName":"Flight_Distance","isNumeric":true,"isDate":false},{"name":"DepartureDelayin_Mins","type":"numeric","escapedName":"DepartureDelayin_Mins","isNumeric":true,"isDate":false},{"name":"ArrivalDelayin_Mins","type":"numeric","escapedName":"ArrivalDelayin_Mins","isNumeric":true,"isDate":false},{"name":"Gate_location","type":"factor","escapedName":"Gate_location","isNumeric":false,"isDate":false},{"name":"Seat_comfort","type":"factor","escapedName":"Seat_comfort","isNumeric":false,"isDate":false},{"name":"Departure_Arrival_time_convenient","type":"factor","escapedName":"Departure_Arrival_time_convenient","isNumeric":false,"isDate":false},{"name":"Food_drink","type":"factor","escapedName":"Food_drink","isNumeric":false,"isDate":false},{"name":"Inflightwifi_service","type":"factor","escapedName":"Inflightwifi_service","isNumeric":false,"isDate":false},{"name":"Inflight_entertainment","type":"factor","escapedName":"Inflight_entertainment","isNumeric":false,"isDate":false},{"name":"Online_support","type":"factor","escapedName":"Online_support","isNumeric":false,"isDate":false},{"name":"Ease_of_Onlinebooking","type":"factor","escapedName":"Ease_of_Onlinebooking","isNumeric":false,"isDate":false},{"name":"Onboard_service","type":"factor","escapedName":"Onboard_service","isNumeric":false,"isDate":false},{"name":"Leg_room_service","type":"factor","escapedName":"Leg_room_service","isNumeric":false,"isDate":false},{"name":"Baggage_handling","type":"factor","escapedName":"Baggage_handling","isNumeric":false,"isDate":false},{"name":"Checkin_service","type":"factor","escapedName":"Checkin_service","isNumeric":false,"isDate":false},{"name":"Cleanliness","type":"factor","escapedName":"Cleanliness","isNumeric":false,"isDate":false},{"name":"Online_boarding","type":"factor","escapedName":"Online_boarding","isNumeric":false,"isDate":false},{"name":"CustomerID","type":"numeric","escapedName":"CustomerID","isNumeric":true,"isDate":false},{"name":"ArrivalDelay_NA","type":"numeric","escapedName":"ArrivalDelay_NA","isNumeric":true,"isDate":false},{"name":"delay_summary","type":"factor","escapedName":"delay_summary","isNumeric":false,"isDate":false},{"name":"Age_category","type":"factor","escapedName":"Age_category","isNumeric":false,"isDate":false}],"xReflineType":"none","y":{"name":".dummy.column.name.for.count.column","validName":".dummy.column.name.for.count.column","isNumeric":true,"isDate":false,"type":"numrows","func":"count","label":"c2","validLabel":"c2"},"yUse2ndAxis":null,"colorBucketGroup":"all","xBucketGroup":"all","yBucketGroup":null,"pivotShowSubtotals":null,"y0":null,"yReflineRangeType":"none","sampleDataSize":0,"data":{"c3":["Neutral or Dissatisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Satisfied","Satisfied","Satisfied","Satisfied","Satisfied","Satisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Satisfied","Satisfied","Satisfied","Satisfied","Satisfied","Satisfied"],"c1":["excellent","good","acceptable","need improvement","poor","extremely poor","excellent","good","acceptable","need improvement","poor","extremely poor","excellent","good","acceptable","need improvement","poor","extremely poor","excellent","good","acceptable","need improvement","poor","extremely poor"],"c2":[null,null,null,null,null,null,null,null,null,null,null,null,6753,7994,9382,9447,7527,53,13505,14165,9817,9447,2784,43],"..nrow":[null,null,null,null,null,null,null,null,null,null,null,null,6753,7994,9382,9447,7527,53,13505,14165,9817,9447,2784,43],"..is.order.row":["TRUE","TRUE","TRUE","TRUE","TRUE","TRUE","TRUE","TRUE","TRUE","TRUE","TRUE","TRUE",null,null,null,null,null,null,null,null,null,null,null,null]},"query":"try(jsonlite::toJSON({.dqdf <- `Full_Dataset_iXJ7aWP4`;\n.dqdf %>% \ndplyr::ungroup() %>% \n(function(x){ colnames(x)<-make.unique(colnames(x)); return (x)}) %>% \ndplyr::mutate_if(bit64::is.integer64, as.numeric) %>% \ndplyr::mutate_if(lubridate::is.period, lubridate::as.difftime) %>% \ndplyr::mutate_if(lubridate::is.interval, lubridate::as.difftime) %>% \ndplyr::mutate_if(lubridate::is.difftime, as.numeric) %>% \ndplyr::mutate(c3=`_tam_convert_na`((function(x) {\n  if (!is.factor(x)) {\n    x <- factor(x) \n  }; \n  if (length(unique(x)) <= 20) {\n    return (x); \n  };\n  return (\n    forcats::fct_lump(\n      x, \n      n=20,\n      ties.method =\"first\",\n      other_level = \"Others\"\n    )\n  )\n})(Satisfaction), drop.unused.levels=FALSE, na.alt.text=\"(NA)\"),c1=`_tam_convert_na`(`Inflightwifi_service`, drop.unused.levels=FALSE, na.alt.text=\"(NA)\"),c2=1,..nrow=1) %>% \ndplyr::filter(!is.na(c3) & !is.na(c1)) %>% \ndplyr::group_by (c3,c1) %>% \ndplyr::summarise(c2=dplyr::n(),..nrow=dplyr::n()) %>% \ndplyr::ungroup() %>% \ndplyr::mutate() %>% \ntidyr::complete(c3,c1) %>% \ndplyr::ungroup() %>% \ndplyr::arrange(dplyr::desc(c3),c1) %>% \n(function(df) {   `_tam_setCurrentVizDataCache`(df, type=\"groupby\", colnames=c(\"c3\"=\"Satisfaction\", \"c1\"=\"Inflightwifi_service\", \"c2\"=\"(Number of Rows)\", \"..nrow\"=\"[remove]\"))\n  return (df);}) %>% \ndplyr::mutate(c3= (function(x) { if (!is.factor(x)) { return(forcats::fct_rev(factor(x))) } else { return(forcats::fct_rev(x))} })(c3)) %>% \n  `_tam_add_order_rows`( target=c(\"c3\",\"c1\"), \"c3\", \"c1\") %>% \n `_tam_convertLogicalToCharacter`()\n}\n, na=\"null\" , dataframe=\"columns\", digits=10))","_desktopVersion":"6.3.4.2","height":500,"width":800,"color":{"name":"Satisfaction","displayName":"","validName":"Satisfaction","type":"factor","isNumeric":false,"isDate":false,"func":"none","label":"c3","validLabel":"c3"},"colorbucket":null},"timestamp":"2021-01-09T20:37:30.729Z","updatedBy":"","thumbnail":null,"snapshot":null,"publicTitle":"","publicDescription":"","publicDataSourceURL":"","withSteps":false,"privateShare":true,"sharedURL":"","previousSharedURL":"","sharedDate":null,"publishSizeOption":null,"insightMetaInfo":{"version":1,"url":null,"isPrivate":false,"thumbnail":null,"title":"","description":"","learnMoreUrl":null,"exampleUrl":null,"sharedFileTypes":[],"withSteps":false,"dataSourceURL":""},"preprocessor":"","_tags":null,"filterCommand":{"name":"filter","conditions":[]},"variables":[],"tabIndex":11,"_isDirty":false,"_createdVersion":"6.3.4.2","annotation":"","_tempAnnotation":"","_tempDisplayName":"","_thumbnailFileName":"","_containsInvalidColumns":null,"filename":"KHG1hGM3.json"}