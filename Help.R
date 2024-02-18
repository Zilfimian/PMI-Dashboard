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
