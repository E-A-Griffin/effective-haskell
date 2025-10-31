{-# LANGUAGE LambdaCase #-}

module MaybeInt where

data CustomerInfo = CustomerInfo
  { customerName :: String
  , customerBalance :: Int
  }

data EmployeeInfo = EmployeeInfo
  { employeeName :: String
  , employeeManagerName :: String
  , employeeSalary :: Int
  }

data Person
  = Customer CustomerInfo
  | Employee EmployeeInfo

data MaybeInt = NoInt | SomeInt Int

-- Not ideal style but doing for fun
getPersonBalance, getPersonSalary :: Person -> MaybeInt
(getPersonBalance, getPersonSalary)
  = ( \case
        Customer c -> SomeInt $ customerBalance c
        Employee _ -> NoInt
    , \case
        Customer _ -> NoInt
        Employee e -> SomeInt $ employeeSalary e
    )
