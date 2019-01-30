test_that("calculateLagGroupedDF", {
	expected = data.frame(
		PlantID = c(
			rep("1", 7),
			rep("2", 7)
		),
		Date = as.Date(c(
			"2013-02-26",
			"2013-07-25",
			"2013-12-14",
			"2014-05-11",
			"2014-06-25",
			"2015-01-09",
			"2015-05-26",
			"2013-02-26",
			"2013-07-25",
			"2013-12-14",
			"2014-05-11",
			"2014-06-25",
			"2015-01-09",
			"2015-05-26"
		)),
		Height_t = c(
			3,
			5.5,
			7,
			8,
			11,
			13,
			8.5,
			30,
			55,
			70,
			80,
			110,
			130,
			85
		),
		Height_t_1 = c(
			NA,   
			3,   
			5.5,   
			7,   
			8,  
			11,  
			13,  
			NA,  
			30,
			55, 
			70, 
			80,
			110,
			130
		),
		Size_t = c(
			1,
			2,
			3,
			4,
			5,
			6,
			7,
			10,
			20,
			30,
			40,
			50,
			60,
			70
		),
		Size_t_1 = c(
			NA,
			1,  
			2,  
			3,  
			4,  
			5,  
			6, 
			NA, 
			10,
			20, 
			30, 
			40, 
			50, 
			60
		)
	)
	test = data.frame(
		PlantID = c(
			rep("1", 7),
			rep("2", 7)
		),
		Date = as.Date(c(
			"2013-02-26",
			"2013-07-25",
			"2013-12-14",
			"2014-05-11",
			"2014-06-25",
			"2015-01-09",
			"2015-05-26",
			"2013-02-26",
			"2013-07-25",
			"2013-12-14",
			"2014-05-11",
			"2014-06-25",
			"2015-01-09",
			"2015-05-26"
		)),
		Height_t = c(
			 3,
			 5.5,
			 7,
			 8,
			 11,
			 13,
			 8.5,
			 30,
			 55,
			 70,
			 80,
			 110,
			 130,
			 85
		),
		Size_t = c(
			1,
			2,
			3,
			4,
			5,
			6,
			7,
			10,
			20,
			30,
			40,
			50,
			60,
			70
		)
	) %>% 
		arrange(.data$Date) 
	test <- calculateLagGroupedDF(
			x = test,
			vars = c(
				"Size_t", 
				"Height_t", 
				"Cone_t", 
				"Cylinder_Tall_t", 
				"Cylinder_t", 
				"Elliptic_Cylinder_t"
			), 
			arrange.variable = "Date", 
			grouping.variable = "PlantID"
		) %>%
		dplyr::select_(.dots=names(expected))
	
	expect_identical(test, expected)
})
