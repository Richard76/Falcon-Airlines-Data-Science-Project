window["viz_yrw8igc4"]= {"name":"yrw8igc4","displayName":"TypeTravel","dependencies":["Full_Dataset"],"transformName":"Full_Dataset_iXJ7aWP4","options":{"marker":"bar","x":{"name":"TypeTravel","validName":"`TypeTravel`","isNumeric":false,"isDate":false,"type":"factor","func":"none","validLabel":"c1","label":"c1"},"displayName":"TypeTravel","columns":[{"name":"Satisfaction","type":"factor","escapedName":"Satisfaction","isNumeric":false,"isDate":false},{"name":"Gender","type":"factor","escapedName":"Gender","isNumeric":false,"isDate":false},{"name":"CustomerType","type":"factor","escapedName":"CustomerType","isNumeric":false,"isDate":false},{"name":"TypeTravel","type":"factor","escapedName":"TypeTravel","isNumeric":false,"isDate":false},{"name":"Class","type":"factor","escapedName":"Class","isNumeric":false,"isDate":false},{"name":"Age","type":"numeric","escapedName":"Age","isNumeric":true,"isDate":false},{"name":"Flight_Distance","type":"numeric","escapedName":"Flight_Distance","isNumeric":true,"isDate":false},{"name":"DepartureDelayin_Mins","type":"numeric","escapedName":"DepartureDelayin_Mins","isNumeric":true,"isDate":false},{"name":"ArrivalDelayin_Mins","type":"numeric","escapedName":"ArrivalDelayin_Mins","isNumeric":true,"isDate":false},{"name":"Gate_location","type":"factor","escapedName":"Gate_location","isNumeric":false,"isDate":false},{"name":"Seat_comfort","type":"factor","escapedName":"Seat_comfort","isNumeric":false,"isDate":false},{"name":"Departure_Arrival_time_convenient","type":"factor","escapedName":"Departure_Arrival_time_convenient","isNumeric":false,"isDate":false},{"name":"Food_drink","type":"factor","escapedName":"Food_drink","isNumeric":false,"isDate":false},{"name":"Inflightwifi_service","type":"factor","escapedName":"Inflightwifi_service","isNumeric":false,"isDate":false},{"name":"Inflight_entertainment","type":"factor","escapedName":"Inflight_entertainment","isNumeric":false,"isDate":false},{"name":"Online_support","type":"factor","escapedName":"Online_support","isNumeric":false,"isDate":false},{"name":"Ease_of_Onlinebooking","type":"factor","escapedName":"Ease_of_Onlinebooking","isNumeric":false,"isDate":false},{"name":"Onboard_service","type":"factor","escapedName":"Onboard_service","isNumeric":false,"isDate":false},{"name":"Leg_room_service","type":"factor","escapedName":"Leg_room_service","isNumeric":false,"isDate":false},{"name":"Baggage_handling","type":"factor","escapedName":"Baggage_handling","isNumeric":false,"isDate":false},{"name":"Checkin_service","type":"factor","escapedName":"Checkin_service","isNumeric":false,"isDate":false},{"name":"Cleanliness","type":"factor","escapedName":"Cleanliness","isNumeric":false,"isDate":false},{"name":"Online_boarding","type":"factor","escapedName":"Online_boarding","isNumeric":false,"isDate":false},{"name":"CustomerID","type":"numeric","escapedName":"CustomerID","isNumeric":true,"isDate":false},{"name":"ArrivalDelay_NA","type":"numeric","escapedName":"ArrivalDelay_NA","isNumeric":true,"isDate":false},{"name":"delay_summary","type":"factor","escapedName":"delay_summary","isNumeric":false,"isDate":false},{"name":"Age_category","type":"factor","escapedName":"Age_category","isNumeric":false,"isDate":false}],"xReflineType":"none","y":{"name":".dummy.column.name.for.count.column","validName":".dummy.column.name.for.count.column","isNumeric":true,"isDate":false,"type":"numrows","func":"count","label":"c2","validLabel":"c2"},"yUse2ndAxis":null,"colorBucketGroup":"all","xBucketGroup":"all","yBucketGroup":null,"pivotShowSubtotals":null,"y0":null,"yReflineRangeType":"none","sampleDataSize":0,"data":{"c3":["Satisfied","Satisfied","Satisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Satisfied","Satisfied","Satisfied","Neutral or Dissatisfied","Neutral or Dissatisfied","Neutral or Dissatisfied"],"c1":["Business travel","Personal Travel","Unknown","Business travel","Personal Travel","Unknown","Business travel","Personal Travel","Unknown","Business travel","Personal Travel","Unknown"],"c2":[null,null,null,null,null,null,32991,11776,4994,23490,13572,4094],"..nrow":[null,null,null,null,null,null,32991,11776,4994,23490,13572,4094],"..is.order.row":["TRUE","TRUE","TRUE","TRUE","TRUE","TRUE",null,null,null,null,null,null]},"query":"try(jsonlite::toJSON({.dqdf <- `Full_Dataset_iXJ7aWP4`;\n.dqdf %>% \ndplyr::ungroup() %>% \n(function(x){ colnames(x)<-make.unique(colnames(x)); return (x)}) %>% \ndplyr::mutate_if(bit64::is.integer64, as.numeric) %>% \ndplyr::mutate_if(lubridate::is.period, lubridate::as.difftime) %>% \ndplyr::mutate_if(lubridate::is.interval, lubridate::as.difftime) %>% \ndplyr::mutate_if(lubridate::is.difftime, as.numeric) %>% \ndplyr::mutate(c3=`_tam_convert_na`((function(x) {\n  if (!is.factor(x)) {\n    x <- factor(x) \n  }; \n  if (length(unique(x)) <= 20) {\n    return (x); \n  };\n  return (\n    forcats::fct_lump(\n      x, \n      n=20,\n      ties.method =\"first\",\n      other_level = \"Others\"\n    )\n  )\n})(Satisfaction), drop.unused.levels=FALSE, na.alt.text=\"(NA)\"),c1=`_tam_convert_na`(`TypeTravel`, drop.unused.levels=FALSE, na.alt.text=\"(NA)\"),c2=1,..nrow=1) %>% \ndplyr::filter(!is.na(c3) & !is.na(c1)) %>% \ndplyr::group_by (c3,c1) %>% \ndplyr::summarise(c2=dplyr::n(),..nrow=dplyr::n()) %>% \ndplyr::ungroup() %>% \ndplyr::mutate() %>% \ntidyr::complete(c3,c1) %>% \ndplyr::group_by(c3) %>% \ndplyr::mutate(c2= ifelse(is.na(c2), 0, c2),..nrow= ifelse(is.na(..nrow), 0, ..nrow)) %>% \ndplyr::ungroup() %>% \ndplyr::ungroup() %>% \ndplyr::arrange(c3,c1) %>% \n(function(df) {   `_tam_setCurrentVizDataCache`(df, type=\"groupby\", colnames=c(\"c3\"=\"Satisfaction\", \"c1\"=\"TypeTravel\", \"c2\"=\"(Number of Rows)\", \"..nrow\"=\"[remove]\"))\n  return (df);}) %>% \n  `_tam_add_order_rows`( target=c(\"c3\",\"c1\"), \"c3\", \"c1\") %>% \n `_tam_convertLogicalToCharacter`()\n}\n, na=\"null\" , dataframe=\"columns\", digits=10))","_desktopVersion":"6.3.4.2","height":500,"width":800,"color":{"name":"Satisfaction","displayName":"","validName":"Satisfaction","type":"factor","isNumeric":false,"isDate":false,"func":"none","label":"c3","validLabel":"c3"},"colorbucket":null,"facet":null,"barstyle":"group"},"timestamp":"2021-01-09T20:18:40.607Z","updatedBy":"","thumbnail":null,"snapshot":null,"publicTitle":"","publicDescription":"","publicDataSourceURL":"","withSteps":false,"privateShare":true,"sharedURL":"","previousSharedURL":"","sharedDate":null,"publishSizeOption":null,"insightMetaInfo":{"version":1,"url":null,"isPrivate":false,"thumbnail":null,"title":"","description":"","learnMoreUrl":null,"exampleUrl":null,"sharedFileTypes":[],"withSteps":false,"dataSourceURL":""},"preprocessor":"","_tags":null,"filterCommand":{"name":"filter","conditions":[]},"variables":[],"tabIndex":2,"_isDirty":false,"_createdVersion":"6.3.4.2","annotation":"","_tempAnnotation":"","_tempDisplayName":"","_thumbnailFileName":"","_containsInvalidColumns":null,"filename":"yrw8igc4.json"}