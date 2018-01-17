namespace Database

open System.Data.SqlClient

module Database =

    let getItems() =

     use foo = new SqlConnection("server=dev-clive2\sql2014; initial catalog=StackItems; Integrated Security=SSPI;")
     foo.Open()
     
     use command = foo.CreateCommand()

     command.CommandText <- "SELECT * FROM items"

     use reader = command.ExecuteReader()
      
     seq {
         while reader.Read()
            do yield reader.GetString(0)
     }
     |> Seq.toList

    let addToBag(item) = 
     use foo = new SqlConnection("server=dev-clive2\sql2014; initial catalog=StackItems; Integrated Security=SSPI;")
     foo.Open()
     
     use command = foo.CreateCommand()

     command.CommandText <- "INSERT INTO bag VALUES (@p0)"
     command.Parameters.Add(SqlParameter("p0", item))

     command.ExecuteNonQuery()