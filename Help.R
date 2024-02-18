product_tab1 <- "SELECT 
    p.ListPrice,
    p.Name,
    aa.CategoryName
FROM 
    production_product AS p
LEFT JOIN 
    (
        SELECT
            ps.ProductSubcategoryID,
            pc.Name AS CategoryName
        FROM
            production_productsubcategory AS ps
        LEFT JOIN
            production_productcategory AS pc ON ps.ProductCategoryID = pc.ProductCategoryID
    ) AS aa ON p.ProductSubcategoryID = aa.ProductSubcategoryID
WHERE 
    p.ProductSubcategoryID IS NOT NULL;
"


query_demo_tab1 <- "SELECT *
FROM (
    SELECT
        PersonID,
        TerritoryID
    FROM
        sales_customer
    WHERE
        PersonID IS NOT NULL
) AS sales_customer
LEFT JOIN (
    SELECT
        BusinessEntityID,
        ExtractValue(Demographics, '/IndividualSurvey/TotalPurchaseYTD') AS TotalPurchaseYTD,
        ExtractValue(Demographics, '/IndividualSurvey/DateFirstPurchase') AS DateFirstPurchase,
        ExtractValue(Demographics, '/IndividualSurvey/BirthDate') AS BirthDate,
        ExtractValue(Demographics, '/IndividualSurvey/MaritalStatus') AS MaritalStatus,
        ExtractValue(Demographics, '/IndividualSurvey/YearlyIncome') AS YearlyIncome,
        ExtractValue(Demographics, '/IndividualSurvey/Gender') AS Gender,
        ExtractValue(Demographics, '/IndividualSurvey/TotalChildren') AS TotalChildren,
        ExtractValue(Demographics, '/IndividualSurvey/NumberChildrenAtHome') AS NumberChildrenAtHome,
        ExtractValue(Demographics, '/IndividualSurvey/Education') AS Education,
        ExtractValue(Demographics, '/IndividualSurvey/Occupation') AS Occupation,
        ExtractValue(Demographics, '/IndividualSurvey/HomeOwnerFlag') AS HomeOwnerFlag,
        ExtractValue(Demographics, '/IndividualSurvey/NumberCarsOwned') AS NumberCarsOwned,
        ExtractValue(Demographics, '/IndividualSurvey/CommuteDistance') AS CommuteDistance
    FROM
        person_person
) AS person_person ON sales_customer.PersonID = person_person.BusinessEntityID;
"



query_sales_trends_tab2 <- "SELECT
    sales_salesorderdetail.OrderQty, sales_salesorderdetail.LineTotal,
    production_product.Name AS ProductName, production_product.ProductLine,  production_product.Class, production_product.Style,  
    sales_salesorderheader.OrderDate, sales_salesorderheader.Status, 
    sales_salesterritory.Name AS NameTerritory, sales_salesterritory.CountryRegionCode, sales_salesterritory.Group,
    production_productcategory.Name As CategoryName
FROM
    (sales_salesorderdetail
    LEFT JOIN (production_product 
LEFT JOIN
    (production_productsubcategory 
	 LEFT JOIN production_productcategory ON production_productsubcategory.ProductCategoryID = production_productcategory.ProductCategoryID)
     ON production_product.ProductSubcategoryID = production_productsubcategory.ProductSubcategoryID) ON sales_salesorderdetail.ProductID = production_product.ProductID)
LEFT JOIN
    (sales_salesorderheader
    LEFT JOIN sales_salesterritory ON sales_salesorderheader.TerritoryID = sales_salesterritory.TerritoryID)
ON
    sales_salesorderdetail.SalesOrderID = sales_salesorderheader.SalesOrderID"
