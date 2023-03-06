Imports System.Math
Imports System.IO
Imports System.Text.RegularExpressions

Module Module1


    Public Structure User
        Dim entry As Boolean
        Dim username, passcode, legalName, company, userLevel, phoneNumber, Team As String

        Sub SignIn()


            username = ""
            Dim usernameInput, password As String
            'PROBABLY MAKE CASE SENSITIVE AND ALLOW FOR SPECIAL CHARACTERS LATER <3'
            Dim subj As ConsoleKey
            Dim reader, reader2 As Odbc.OdbcDataReader
            Dim checkForUser As New Odbc.OdbcCommand("", conn)
            Dim assignTeam As New Odbc.OdbcCommand("", conn)
            Do
                Console.Write("Sign in to initiate use
username: ")
                usernameInput = SQLmaintenance(Console.ReadLine(), 30)
                Console.Write("password: ")
                password = ""
                Console.ForegroundColor = Console.BackgroundColor
                subj = Console.ReadKey.Key
                Console.ForegroundColor = ConsoleColor.White
                While subj <> ConsoleKey.Enter
                    If subj = ConsoleKey.Backspace And password.Length > 0 Then
                        password = password.Remove(password.Length - 1)
                        Console.Clear()
                        Console.Write("Sign in to initiate use
username: " & usernameInput & "
password: ")
                        For i = 0 To password.Length - 1
                            Console.Write(".")
                        Next
                        Console.SetCursorPosition(password.Length + 9, 2)
                    ElseIf subj > 62 And subj < 91 Or subj > 47 And subj < 58 Then
                        If subj > 62 And subj < 91 Then
                            password &= subj.ToString
                        ElseIf subj > 47 And subj < 58 Then
                            password &= Mid(subj.ToString, 2, 1)
                        End If
                        Console.Clear()
                        Console.Write("Sign in to initiate use
username: " & usernameInput & "
password: ")
                        For i = 0 To password.Length - 1
                            Console.Write(".")
                        Next
                        Console.SetCursorPosition(password.Length + 9, 2)
                    End If
                    Console.ForegroundColor = Console.BackgroundColor
                    subj = Console.ReadKey.Key
                    Console.ForegroundColor = ConsoleColor.White
                End While
                subj = ConsoleKey.A
                Console.SetCursorPosition(0, 3)
                checkForUser.CommandText = ("SELECT * FROM Users WHERE Username = '" & usernameInput & "' AND Passcode = '" & LCase(password) & "';")
                reader = checkForUser.ExecuteReader
                If reader.Read() Then
                    legalName = reader("Legal name")
                    company = reader("Company")
                    phoneNumber = reader("Phone Num")
                    userLevel = reader("User Level")
                    username = usernameInput
                    entry = True
                    assignTeam.CommandText = ("SELECT TeamID FROM TeamMapping WHERE Username = '" & usernameInput & "';")
                    reader2 = assignTeam.ExecuteReader
                    Team = reader2.Read
                    Console.WriteLine("Welcome " & legalName & ". Press any key to return to the main menu.")
                    Console.ReadKey()
                    Console.Clear()
                Else
                    Console.WriteLine("No user found! Check credentials and try again. 
Press any key to restart
Press Backspace to return")
                    reader.Close()
                    subj = Console.ReadKey.Key
                    Console.Clear()
                End If
            Loop Until subj = ConsoleKey.Backspace Or entry

        End Sub

        Function Greet() As String
            Return legalName
        End Function
        Function SignedIn() As Boolean
            Return entry
        End Function

        Function GiveCompany()
            Return company
        End Function
        Function PowerLevel() As Integer
            If userLevel = "Admin" Then
                Return 3
            ElseIf userLevel = "Manager" Then
                Return 2
            ElseIf userLevel = "Employee" Then
                Return 1
            Else
                Return 0
            End If
        End Function
        Function returnUsername()
            Return username
        End Function
        Function checkPassword()
            Dim password As String
            Dim subj As ConsoleKey
            Console.Write("Enter password: ")
            password = ""
            Console.ForegroundColor = Console.BackgroundColor
            subj = Console.ReadKey.Key
            Console.ForegroundColor = ConsoleColor.White
            While subj <> ConsoleKey.Enter

                If subj = ConsoleKey.Backspace And password.Length > 0 Then
                    password = password.Remove(password.Length - 1)
                    Console.Clear()
                    Console.Write("Enter password: ")
                    For i = 0 To password.Length - 1
                        Console.Write(".")
                    Next
                    Console.SetCursorPosition(password.Length + 9, 2)
                ElseIf subj > 62 And subj < 91 Then
                    password = password & subj.ToString
                    Console.Clear()
                    Console.Write("Enter password: ")
                    For i = 0 To password.Length - 1
                        Console.Write(".")
                    Next
                    Console.SetCursorPosition(password.Length + 9, 2)
                End If
                Console.ForegroundColor = Console.BackgroundColor
                subj = Console.ReadKey.Key
                Console.ForegroundColor = ConsoleColor.White
            End While
            If password = UCase(passcode) Then
                Return True
            Else
                Return False
            End If
        End Function

    End Structure


    Public Class Node
        Private Neighbourhood As New List(Of String)
        Dim costFromStart As Integer
        Dim name As String
        Dim PreviousNode As String

        Sub New(ByVal identity As String, ByVal town As Integer(,), ByVal townSize As Integer, ByVal distanceFromStartinput As Integer, ByVal prevnode As String)
            name = identity
            If identity <> "." Then
                Dim whereInMatrix As Integer = IDtoInteger(identity) - 1
                If whereInMatrix <= townSize Then
                    For i = 0 To townSize - 1
                        If town(whereInMatrix, i) <> Integer.MaxValue Then
                            If IntegertoID(i + 1) <> name Then
                                Neighbourhood.Add(IntegertoID(i + 1))
                            End If
                        End If
                    Next
                End If
            End If
            PreviousNode = prevnode
            costFromStart = distanceFromStartinput
        End Sub
        '___________________________________________________________________'
        Sub NodeCopy(ByVal Swap As Node)
            Neighbourhood = Swap.Neighbours
            name = Swap.ID
            costFromStart = Swap.Cost
            PreviousNode = Swap.Previous
        End Sub
        Sub Newcost(ByVal costUpdate)
            costFromStart = costUpdate
        End Sub
        Sub SetStartNode()
            costFromStart = 0
            PreviousNode = "_"
        End Sub
        Sub SetDefaultNode()
            name = "."
            costFromStart = Integer.MaxValue
            Neighbourhood.Clear()
            PreviousNode = "."
        End Sub
        Sub ChangePrevious(ByRef neighbourino As String)
            PreviousNode = neighbourino
        End Sub
        '___________________________________________________________________'

        Function Neighbours() As List(Of String)
            Return Neighbourhood
        End Function
        Function ID() As String
            Return name
        End Function
        Function Cost() As Integer
            Return costFromStart
        End Function

        Function Previous() As String
            Return PreviousNode
        End Function

    End Class





    Public Structure Employee
        Dim Home As String
        Dim currentPosition As String
        Dim MinutesWorked As Integer
        Dim NodePaths As List(Of String)
        Dim Quals As List(Of String)
        Sub CostUpdate(ByVal newCost As Integer)
            MinutesWorked = newCost
        End Sub
        Sub PositionUpdate(ByVal newPosition As String)
            currentPosition = newPosition
        End Sub
    End Structure
    Public Structure Job
        Dim Destination As String
        Dim Qualification As String
        Dim UJC As Integer
    End Structure
    Dim conn As New System.Data.Odbc.OdbcConnection("DRIVER={MySQL ODBC 5.3 ANSI Driver};SERVER=localhost;PORT=3306;DATABASE=ProjDb;USER=root;PASSWORD=root;OPTION=3;")




    Function SQLmaintenance(ByRef inputString As String, ByVal lengthlimit As Integer) As String
        Dim outputstring As String = inputString
        Dim noInjection As New Regex("'+|\\+|#+|`+")
        Dim stringPasses As Boolean
        Do
            If noInjection.IsMatch(outputstring) Or outputstring.Length > lengthlimit Then
                Console.WriteLine("Error Occured. Please re-enter this field. Entries must meet the following requirements
entry must be no longer than " & lengthlimit & "
The following characters are not permtted:
` ' \ #")
                outputstring = Console.ReadLine
            Else
                stringPasses = True
            End If
        Loop Until stringPasses
        Return outputstring
    End Function
    Function MatrixMaker(ByVal nodetotal As Integer) As Integer(,)
        Console.WriteLine("Generating Matrix...")
        Dim matrixpasses, notAlone As Boolean
        Dim adjacencymatrix(nodetotal, nodetotal) As Integer
        Do

            For i = 0 To nodetotal - 1
                Do

                    For j = 0 To nodetotal - 1

                        If i = j Then
                            adjacencymatrix(i, j) = Integer.MaxValue
                        Else
                            If Int(Rnd() * Int(Sqrt(nodetotal) / 4) + 1) = 1 Then
                                If adjacencymatrix(i, j) = 0 Then
                                    Select Case Int(Rnd() * Int(Sqrt(nodetotal)) + 1)
                                        Case = 1
                                            adjacencymatrix(i, j) = Int(Rnd() * 8) + 2
                                        Case < 5
                                            adjacencymatrix(i, j) = Int(Rnd() * 88) + 11
                                        Case Else
                                            adjacencymatrix(i, j) = Int(Rnd() * 899) + 100
                                    End Select
                                    adjacencymatrix(j, i) = adjacencymatrix(i, j)
                                End If
                            Else
                                adjacencymatrix(i, j) = Integer.MaxValue
                                adjacencymatrix(j, i) = adjacencymatrix(i, j)
                            End If
                        End If


                    Next
                    For x = 0 To nodetotal - 1
                        notAlone = False
                        For y = 0 To nodetotal - 1
                            If adjacencymatrix(x, y) <> Integer.MaxValue Then
                                notAlone = True
                            End If
                        Next
                        If notAlone = False Then
                            Exit For
                        End If
                    Next
                Loop Until notAlone
            Next

            matrixpasses = MatrixValid2(nodetotal, adjacencymatrix)
        Loop Until matrixpasses
        Console.Clear()
        Return adjacencymatrix
    End Function

    Function IDtoInteger(ByVal id As String) As Integer
        Dim runningInteger As Integer
        For i = 1 To id.Length
            runningInteger += (Asc(Mid(id, i, 1)) - 64) * 26 ^ (id.Length - i)
        Next
        Return runningInteger
    End Function
    Function IntegertoID(ByVal startingnum As Integer)
        Dim runningString As String = ""
        Dim runningInteger As Integer = startingnum
        Dim keepInOrder As Integer = 0
        Dim stringfinished As Boolean
        While Not stringfinished
            If runningInteger < 26 ^ 7 And runningInteger > 26 ^ 6 Then
                If runningInteger Mod (26 ^ 6) = 0 Then
                    keepInOrder = 1
                End If
                runningString &= Chr(Int(runningInteger / 26 ^ 6) + 64 - keepInOrder)
                runningInteger = (runningInteger + keepInOrder) Mod (26 ^ 6)
                keepInOrder = 0
            ElseIf runningInteger < 26 ^ 6 And runningInteger > 26 ^ 5 Then
                If runningInteger Mod (26 ^ 5) = 0 Then
                    keepInOrder = 1
                End If
                runningString &= Chr(Int(runningInteger / 26 ^ 5) + 64 - keepInOrder)
                runningInteger = (runningInteger + keepInOrder) Mod (26 ^ 5)
                keepInOrder = 0
            ElseIf runningInteger < 26 ^ 5 And runningInteger > 26 ^ 4 Then
                If runningInteger Mod (26 ^ 4) = 0 Then
                    keepInOrder = 1
                End If
                runningString &= Chr(Int(runningInteger / 26 ^ 4) + 64 - keepInOrder)
                runningInteger = (runningInteger + keepInOrder) Mod (26 ^ 4)
                keepInOrder = 0
            ElseIf runningInteger < 26 ^ 4 And runningInteger > 26 ^ 3 Then
                If runningInteger Mod (26 ^ 3) = 0 Then
                    keepInOrder = 1
                End If
                runningString &= Chr(Int(runningInteger / 26 ^ 3) + 64 - keepInOrder)
                runningInteger = (runningInteger + keepInOrder) Mod (26 ^ 3)
                keepInOrder = 0
            ElseIf runningInteger < 26 ^ 3 And runningInteger > 26 ^ 2 Then
                If runningInteger Mod (26 ^ 2) = 0 Then
                    keepInOrder = 1
                End If
                runningString &= Chr(Int(runningInteger / 26 ^ 2) + 64 - keepInOrder)
                runningInteger = (runningInteger + keepInOrder) Mod (26 ^ 2)
                keepInOrder = 0
            ElseIf runningInteger < 26 ^ 2 And runningInteger > 26 Then
                If runningInteger Mod 26 = 0 Then
                    keepInOrder = 1
                End If
                runningString &= Chr(Int(runningInteger / 26) + 64 - keepInOrder)
                runningInteger = (runningInteger) Mod 26
            ElseIf runningInteger <= 26 Then
                If runningInteger = 0 Then
                    runningString &= "Z"
                Else
                    runningString &= Chr(runningInteger + 64)
                End If
                stringfinished = True
            End If
        End While
        Return runningString
    End Function
    Function MatrixValid(ByRef nodetotal As Integer, ByRef adjacencymatrix(,) As Integer)
        Dim unvisited As New Dictionary(Of String, Node)
        Dim visited As New Dictionary(Of String, Node)
        Dim nodelist, abstractText As New List(Of String)
        For i = 2 To nodetotal
            nodelist.Add(IntegertoID(i))
        Next
        Try
            abstractText = DijkstraChain(unvisited, visited, nodetotal, adjacencymatrix, nodetotal, nodelist, "A", 1)
        Catch Matrixfail As KeyNotFoundException
            Return False
        End Try
        Return True
    End Function
    Function MatrixValid2(ByRef nodetotal As Integer, ByRef adjacencymatrix(,) As Integer)

        Dim unvisited As New Dictionary(Of String, Node)
        Dim visited As New Dictionary(Of String, Node)
        Dim nodetorestart As String = "A"
        Dim placeholderNode, nodeForPathMaking, currentnode As Node
        Dim charOfNextNodeToCheck As String
        Dim counter, counter2 As Integer
        Dim path As New List(Of String)
        currentnode = New Node(nodetorestart, adjacencymatrix, nodetotal, Integer.MaxValue, "_")
        unvisited.Add(nodetorestart, currentnode)
        unvisited(nodetorestart).SetStartNode()
        For i = 0 To nodetotal - 1
            If Not unvisited.ContainsKey(IntegertoID(i + 1)) Then
                unvisited.Add(IntegertoID(i + 1), New Node((IntegertoID(i + 1)), adjacencymatrix, nodetotal, Integer.MaxValue, "_"))
            End If
        Next
        While unvisited.Count <> 0
            path.Clear()
            counter2 += 1
            For i = 1 To nodetotal
                If unvisited.ContainsKey(IntegertoID(i)) Then
                    charOfNextNodeToCheck = IntegertoID(i)
                End If
            Next
            While unvisited.Count <> 0
                nodeForPathMaking = New Node(".", adjacencymatrix, nodetotal, Integer.MaxValue, "_")
                placeholderNode = New Node(".", adjacencymatrix, nodetotal, Integer.MaxValue, "_")
                For Each key In unvisited.Keys
                    If placeholderNode.Cost > unvisited(key).Cost Then
                        placeholderNode.NodeCopy(unvisited(key))
                    End If
                Next

                For Each neighbour In placeholderNode.Neighbours
                    If unvisited.ContainsKey(neighbour) Then
                        If placeholderNode.Cost + adjacencymatrix(IDtoInteger(placeholderNode.ID) - 1, IDtoInteger(neighbour) - 1) < (unvisited(neighbour).Cost) Then
                            unvisited(neighbour).Newcost(placeholderNode.Cost + adjacencymatrix(IDtoInteger(placeholderNode.ID) - 1, IDtoInteger(neighbour) - 1))
                            unvisited(neighbour).ChangePrevious(placeholderNode.ID)
                        End If
                    End If
                Next

                visited.Add(placeholderNode.ID, New Node(placeholderNode.ID, adjacencymatrix, nodetotal, placeholderNode.Cost, placeholderNode.Previous))

                unvisited.Remove(placeholderNode.ID)
            End While
        End While

        For i = 1 To nodetotal
            If visited(IntegertoID(i)).Cost = Integer.MaxValue Then
                Console.WriteLine(visited(IntegertoID(i)))
                Console.ReadKey()
                Return False
            End If
        Next
        Return True

    End Function
    Sub DijkstraExample()

        Randomize()
        Dim choice1Passed As Boolean
        Dim choice As ConsoleKey
        Dim listToMatricize As New List(Of Integer)
        Dim scale, iterations, tempInt As Integer
        Dim unvisited As New Dictionary(Of String, Node)
        Dim visited As New Dictionary(Of String, Node)
        Dim adjacencymatrix(,) As Integer
        Dim regionName, tempstring As String
        Dim nodelist As New List(Of String)
        Dim abstractText As New List(Of String)


        Console.WriteLine("Would you like to
1. Generate a random region
2. Use an existing region")

        Do
            choice = Console.ReadKey.Key
            If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then
                Do
                    Console.WriteLine("How many nodes do you want (3-500)")
                    scale = Console.ReadLine
                    If Not (scale > 2 And scale < 501) Then
                        Console.WriteLine("Invalid. Please enter a value between 3 and 500 inclusive.")
                    End If
                Loop Until scale > 2 And scale < 501
                Console.WriteLine("How many iterations do you want to test?")
                iterations = Console.ReadLine
                For i = 1 To iterations
                    Console.WriteLine("What node number would you like to be destination number " & i & "?")
                    Console.WriteLine("Remember to keep it between 1 and " & scale)
                    Do
                        tempInt = Console.ReadLine()
                    Loop Until tempInt > 0 And tempInt < scale + 1
                    nodelist.Add(IntegertoID(tempInt))

                Next
                adjacencymatrix = MatrixMaker(scale)
                choice1Passed = True
            ElseIf choice = ConsoleKey.D2 Or choice = ConsoleKey.NumPad2 Then
                Console.WriteLine("What's the name of the region you want to use?")
                regionName = Console.ReadLine
                Dim workersearch As New Odbc.OdbcCommand("SELECT `Region Scale` FROM `Regions` WHERE `Region Name` = '" & regionName & "';", conn)
                Dim check = workersearch.ExecuteReader
                check.Read()
                scale = check("Region Scale") * 40 + 100
                ReDim adjacencymatrix(scale, scale)
                Using reader As BinaryReader =
                    New BinaryReader(File.Open((regionName & ".region"), FileMode.Open))
                    For i = 0 To scale - 1
                        For j = 0 To scale - 1
                            adjacencymatrix(i, j) = reader.ReadInt32
                        Next
                    Next
                End Using
                Console.WriteLine("How many iterations do you want to test?")
                iterations = Console.ReadLine
                For i = 1 To iterations
                    Console.WriteLine("What node number would you like to be destination number " & i & "?")
                    Console.WriteLine("Remember to keep it between A and " & IntegertoID(scale) & ")")
                    Do
                        tempstring = Console.ReadLine()

                    Loop Until IDtoInteger(tempstring) > 0 And IDtoInteger(tempstring) < scale + 1
                    nodelist.Add(tempstring)

                Next
                choice1Passed = True
            End If
        Loop Until choice1Passed

        abstractText = DijkstraChain(unvisited, visited, scale, adjacencymatrix, iterations, nodelist, "A", 1)


        For i = 0 To scale - 1
            Console.Write(IntegertoID(i + 1) & ":   ")
            For j = 0 To scale - 1
                If adjacencymatrix(i, j) = Integer.MaxValue Then
                    Console.Write("N/A, ")
                Else
                    Console.Write(adjacencymatrix(i, j) & ", ")
                End If
            Next
            Console.WriteLine()
        Next

        For i = 0 To abstractText.Count - 1
            Console.WriteLine(abstractText(i))
        Next
        Console.WriteLine("Press any key to end the simulation")

        Console.ReadKey()
        Console.Clear()
    End Sub

    Function DijkstraChain(ByRef unvisited As Dictionary(Of String, Node), ByRef visited As Dictionary(Of String, Node), ByRef nodetotal As Integer, ByRef adjacencymatrix(,) As Integer, ByRef iterations As Integer, ByRef nodelist As List(Of String), ByVal StartNode As String, ByVal mode As Integer)
        Dim nodetorestart As String = StartNode
        Dim currentnode, placeholderNode As Node
        Dim tempid As String
        Dim counter As Integer
        Dim abstractText As New List(Of String)
        Dim path As New List(Of String)
        While iterations > 0 And nodelist.Count <> 0
            path.Clear()
            unvisited.Clear()
            visited.Clear()
            currentnode = New Node(nodetorestart, adjacencymatrix, nodetotal, Integer.MaxValue, "_")
            unvisited.Add(nodetorestart, currentnode)
            unvisited(nodetorestart).SetStartNode()
            For i = 0 To nodetotal - 1
                tempid = IntegertoID(i + 1)
                If Not unvisited.ContainsKey(tempid) Then
                    unvisited.Add(tempid, New Node((tempid), adjacencymatrix, nodetotal, Integer.MaxValue, "_"))
                End If
            Next

            For i = 0 To nodetotal - 1
                For j = 0 To nodetotal - 1
                    If adjacencymatrix(i, j) = 0 Then
                        adjacencymatrix(i, j) = Integer.MaxValue
                    End If
                Next
            Next


            While unvisited.Count <> 0 And nodelist.Count <> 0
                placeholderNode = New Node(".", adjacencymatrix, nodetotal, Integer.MaxValue, "_")
                For Each key In unvisited.Keys
                    If placeholderNode.Cost > unvisited(key).Cost Then
                        placeholderNode.NodeCopy(unvisited(key))
                    End If
                Next

                For Each neighbour In placeholderNode.Neighbours
                    If unvisited.ContainsKey(neighbour) Then
                        If placeholderNode.Cost + adjacencymatrix(IDtoInteger(placeholderNode.ID) - 1, IDtoInteger(neighbour) - 1) < (unvisited(neighbour).Cost) Then
                            unvisited(neighbour).Newcost(placeholderNode.Cost + adjacencymatrix(IDtoInteger(placeholderNode.ID) - 1, IDtoInteger(neighbour) - 1))
                            unvisited(neighbour).ChangePrevious(placeholderNode.ID)
                        End If
                    End If
                Next
                visited.Add(placeholderNode.ID, New Node(placeholderNode.ID, adjacencymatrix, nodetotal, placeholderNode.Cost, placeholderNode.Previous))

                unvisited.Remove(placeholderNode.ID)


            End While
            If mode = 2 Then
                Return visited(nodelist(0)).Cost
            End If
            abstractText.Add("The Shortest Distance from node " & nodetorestart & " to node " & nodelist(0) & " is " & visited(nodelist(0)).Cost & " and the node journey is as follows")

            path.Add(visited(nodelist(0)).ID)
            placeholderNode.NodeCopy(visited(nodelist(0)))

            While placeholderNode.Previous <> "_"
                path.Add(placeholderNode.Previous)
                placeholderNode.NodeCopy(visited(placeholderNode.Previous))
                counter += 1
            End While
            For i = path.Count - 1 To 0 Step -1
                abstractText.Add(path(i))
            Next
            iterations -= 1
            If nodelist.Count > 0 Then
                nodetorestart = nodelist(0)
            End If
            nodelist.RemoveAt(0)

        End While
        Return abstractText
    End Function

    Sub NewCompany()
        Randomize()
        Dim codeUsed As Boolean
        Dim query As New Odbc.OdbcCommand("SELECT CompanyID FROM Companies", conn)
        Dim checkunused As Odbc.OdbcDataReader
        Dim randomcode As String = ""
        Dim companyName As String
        Console.WriteLine("What is the name of your Company?")
        companyName = SQLmaintenance(Console.ReadLine, 100)
        Do
            For i = 1 To 5
                Select Case (Int(Rnd() * 30) + 1) Mod 6
                    Case 0
                        randomcode &= Int(Rnd() * 10)
                    Case Else
                        randomcode &= Chr(Int(Rnd() * 25) + 65)
                End Select
            Next
            checkunused = query.ExecuteReader
            While checkunused.Read
                If checkunused("CompanyID") = randomcode Then
                    codeUsed = True
                End If
            End While
            checkunused.Close()
        Loop Until Not codeUsed
        Dim nonquery As New Odbc.OdbcCommand("INSERT INTO Companies(CompanyID,Company) VALUES ('" & randomcode & "', '" & companyName & "');", conn)
        nonquery.ExecuteNonQuery()
        Console.WriteLine("Compnay creation successful, your company ID is " & randomcode & ". KEEP THIS SAFE! You will need it to register employees.")
        Console.WriteLine()
        Console.WriteLine("An Admin account must now be registered to the company, please enter your details")

        AddEmployee("Admin")
    End Sub

    Sub AddEmployee(ByVal userLevel As String)

        Dim truth1 As Boolean
        Dim query As String = ""
        Dim companycode As String
        Dim check, check2 As Odbc.OdbcDataReader
        Console.WriteLine("What is your Legal Name?")
        query &= ("'" & SQLmaintenance(Console.ReadLine, 100) & "', ")
        Dim compsearch As New Odbc.OdbcCommand("SELECT CompanyID FROM Companies", conn)
        check = compsearch.ExecuteReader

        Do
            Console.WriteLine("What is your company ID (Receive this via your company's administrative team")
            companycode = Console.ReadLine
            While check.Read And Not truth1
                If check("CompanyID") = companycode Then
                    truth1 = True
                End If
            End While
            If truth1 Then
                Dim compnameget As New Odbc.OdbcCommand("SELECT Company FROM Companies WHERE CompanyID = '" & companycode & "'", conn)
                check2 = compnameget.ExecuteReader
                check2.Read()
                query = query & ("'" & check2("Company") & "', ")
            Else
                Console.WriteLine("Company not found. Retry code or contact company representative.")
                check.Close()
                check = compsearch.ExecuteReader
            End If
        Loop Until truth1
        query &= ("'" & userLevel & "', ")
        Console.WriteLine("Please enter a Username")
        query &= ("'" & SQLmaintenance(Console.ReadLine, 30) & "', ")
        Console.WriteLine("Please enter a Passcode")
        query &= ("'" & SQLmaintenance(Console.ReadLine, 45) & "', ")
        Console.WriteLine("Please enter a Unique User Code (1-2 letters, signifies your home address)")
        query &= ("'" & UCase(SQLmaintenance(Console.ReadLine, 2)) & "', ")
        Console.WriteLine("Please enter a Phone number")
        query &= ("'" & SQLmaintenance(Console.ReadLine, 11) & "'")
        Dim command As New Odbc.OdbcCommand("INSERT INTO Users(`Legal Name`,`Company`,`User Level`,`Username`,`Passcode`, `UUC`, `Phone Num`) Values (" & query & ");", conn)
        command.ExecuteNonQuery()
        Console.WriteLine("Successfully Added! Press any key to return to Main Menu.")
        Console.ReadKey()
        Console.Clear()
    End Sub

    Sub AddClient(ByVal company As String)
        Dim choice As ConsoleKey
        Dim query As String = ""
        Console.WriteLine("What is the Client's Company Name?")
        Dim client As String = SQLmaintenance(Console.ReadLine, 200)
        query &= ("'" & client & "', ")
        Console.WriteLine("What is the Client's address?")
        query &= ("'" & SQLmaintenance(Console.ReadLine, 200) & "', ")
        Console.WriteLine("What is the Name of the Region the client is positioned in?")
        query &= ("'" & SQLmaintenance(Console.ReadLine, 50) & "'")
        Console.WriteLine("Do you have the clients credit details, and permission to hold them? (Y/N)")
        While choice <> ConsoleKey.Y And choice <> ConsoleKey.N
            choice = Console.ReadKey.Key
            If choice = ConsoleKey.Y Then
                Console.WriteLine("What is the Client's Credit Card Number?")
                query &= (", '" & SQLmaintenance(Console.ReadLine, 16) & "', ")
                Console.WriteLine("What is the Client's Credit Card Expiration Date?")
                query &= ("'" & SQLmaintenance(Console.ReadLine, 5) & "', ")
                Console.WriteLine("What is the Client's CVC?")
                query &= ("'" & SQLmaintenance(Console.ReadLine, 3) & "'")
            End If
        End While
        Dim command As New Odbc.OdbcCommand("INSERT INTO Clients(`ClientName`, `address`, `Region Name`) Values (" & query & ");", conn)
        If choice = ConsoleKey.Y Then
            command.CommandText = "INSERT INTO Clients(`ClientName`, `address`, `Region Name`, `Creditnum`, `ExpiryDate`, `CVC`) Values (" & query & ");"
        End If
        command.ExecuteNonQuery()
        Dim command2 As New Odbc.OdbcCommand("INSERT INTO CompanyClient(`Company`, `clientname`) Values ('" & company & "', '" & client & "');", conn)
        command2.ExecuteNonQuery()
        Console.WriteLine("Please enter a Unique Client Code (1-2 letters, signifies their map address)")
        Dim ucc As String = SQLmaintenance(Console.ReadLine(), 2)
        Dim command3 As New Odbc.OdbcCommand("INSERT INTO `UCCs`(`UCC`, `clientname`) Values ('" & ucc & "', '" & client & "');", conn)
        command3.ExecuteNonQuery()
        Console.WriteLine("Client Successfully Added!")
        Console.WriteLine("Press any key to return to your hub")
        Console.ReadKey()
    End Sub


    Sub TeamComp(ByVal company As String)
        Dim goAhead As Boolean
        Dim teamsame As String = ""
        Dim choice, answer As ConsoleKey
        Dim tempstring As String
        Dim nonquery As String = "INSERT INTO `TeamMapping`(`Username`, `TeamID`) Values ("
        Dim TeamGatherer As New Odbc.OdbcCommand("SELECT `TeamID` FROM TeamMapping ORDER BY TeamID DESC", conn)
        Dim teamReader As Odbc.OdbcDataReader = TeamGatherer.ExecuteReader
        Dim MemberGatherer As New Odbc.OdbcCommand("SELECT `TeamID`, `UUC`, `Legal Name`, `User Level`, `Phone Num` FROM Users, TeamMapping WHERE Users.username = TeamMapping.username and company = '" & company & "' ORDER BY 'User Level' ASC;", conn)
        Dim memberReader As Odbc.OdbcDataReader = MemberGatherer.ExecuteReader
        Dim TeamlessGatherer As New Odbc.OdbcCommand("SELECT `UUC`, `Legal Name`, `User Level`, `Phone Num` FROM `users` WHERE Users.username NOT IN(SELECT `Username` FROM `TeamMapping`);", conn)
        Dim teamlessReader As Odbc.OdbcDataReader = TeamlessGatherer.ExecuteReader
        Dim TempCommand As New Odbc.OdbcCommand("", conn)
        Dim tempreader As Odbc.OdbcDataReader
        While teamReader.Read
            If teamsame <> teamReader("teamID") Then
                goAhead = True
                Console.WriteLine(teamReader("teamID"))
                teamsame = teamReader("teamID")
            Else
                goAhead = False
            End If
            While memberReader.Read And goAhead
                If memberReader("TeamID") = teamReader("TeamID") Then
                    Console.WriteLine(memberReader("UUC") & "  |  " & memberReader("User Level") & "  |  " & memberReader("Legal Name") & "  |  " & memberReader("Phone Num") & "  |  ")
                End If
            End While
            memberReader.Close()
            memberReader = MemberGatherer.ExecuteReader
        End While
        teamReader.Close()
        teamReader = TeamGatherer.ExecuteReader
        While teamlessReader.Read
            If Not goAhead Then
                Console.WriteLine("TEAMLESS")
                goAhead = True
            End If
            Console.WriteLine(teamlessReader("UUC") & "  |  " & teamlessReader("User Level") & "  |  " & teamlessReader("Legal Name") & "  |  " & teamlessReader("Phone Num") & "  |  ")
        End While
        Console.WriteLine()
        Console.WriteLine("Are These Teams Adequate? (Y/N)")
        Console.ForegroundColor = Console.BackgroundColor
        answer = Console.ReadKey.Key
        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine()

        If answer = ConsoleKey.N Then
            Do
                Console.WriteLine("Do you wish to
1. Add a new Team
2. Reassign workers to another Team
0. Return to menu")
                choice = Console.ReadKey.Key
                If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then
                    NewTeam(company)
                ElseIf choice = ConsoleKey.D2 Or choice = ConsoleKey.NumPad2 Then
                    Console.WriteLine()
                    While teamReader.Read
                        If teamsame <> teamReader("teamID") Then
                            goAhead = True
                            Console.WriteLine(teamReader("teamID"))
                            teamsame = teamReader("teamID")
                        Else
                            goAhead = False
                        End If
                        While memberReader.Read And goAhead
                            If memberReader("TeamID") = teamReader("TeamID") Then
                                Console.WriteLine(memberReader("UUC") & "  |  " & memberReader("User Level") & "  |  " & memberReader("Legal Name") & "  |  " & memberReader("Phone Num") & "  |  ")
                            End If
                        End While
                        memberReader.Close()
                        memberReader = MemberGatherer.ExecuteReader
                    End While
                    teamReader.Close()
                    teamReader = TeamGatherer.ExecuteReader
                    Do
                        Console.WriteLine("Do you wish to
1. Add a user to a team
2. Remove a user from an existing team
0. Return to menu")
                        choice = Console.ReadKey.Key
                        If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then
                            goAhead = False
                            Do
                                Console.WriteLine("Write the UUC of the user you'd like to add to a team")
                                tempstring = Console.ReadLine
                                TempCommand.CommandText = ("SELECT `UUC` FROM `Users`")
                                tempreader = TempCommand.ExecuteReader
                                While tempreader.Read And goAhead = False
                                    If tempreader("UUC") = tempstring Then
                                        goAhead = True
                                    End If
                                End While
                                If Not goAhead Then
                                    Console.WriteLine("User not found. Try again")
                                End If
                            Loop Until goAhead
                            tempreader.Close()
                            nonquery &= "(SELECT Username FROM Users WHERE UUC = '" & tempstring & "'), '"
                            goAhead = False
                            Do
                                Console.WriteLine("Write the Team ID of the team you'd like to add t")
                                tempstring = Console.ReadLine
                                TempCommand.CommandText = ("SELECT `TeamID` FROM `Teams`")
                                tempreader = TempCommand.ExecuteReader
                                While tempreader.Read And goAhead = False
                                    If tempreader("TeamID") = tempstring Then
                                        goAhead = True
                                    End If
                                End While
                                If Not goAhead Then
                                    Console.WriteLine("Team ID not found. Try again")
                                End If
                                tempreader.Close()
                            Loop Until goAhead
                            tempreader.Close()
                            nonquery &= tempstring & "');"
                            TempCommand.CommandText = nonquery
                            TempCommand.ExecuteNonQuery()
                            Console.WriteLine("Complete. Press any key to return to menu.")
                            Console.ReadKey()
                            choice = ConsoleKey.D0
                        ElseIf choice = ConsoleKey.D2 Or choice = ConsoleKey.NumPad2 Then
                            goAhead = False
                            Do
                                Console.WriteLine("Please enter the UUC of the user you'd like to leave teamless")
                                tempstring = Console.ReadLine
                                TempCommand.CommandText = ("SELECT `UUC` FROM `Users`")
                                tempreader = TempCommand.ExecuteReader
                                While tempreader.Read And goAhead = False
                                    If tempreader("UUC") = tempstring Then
                                        goAhead = True
                                    End If
                                End While
                                If Not goAhead Then
                                    Console.WriteLine("User not found. Try again")
                                End If
                            Loop Until goAhead
                            tempreader.Close()
                            TempCommand.CommandText = "DELETE FROM TeamMapping WHERE Username = (SELECT `Username` FROM `Users` WHERE `UUC` = '" & tempstring & "');"
                            TempCommand.ExecuteNonQuery()
                            Console.WriteLine("Complete. Press any key to return to menu.")
                            Console.ReadKey()
                        End If
                    Loop Until choice = ConsoleKey.D0 Or choice = ConsoleKey.NumPad0
                End If
            Loop Until choice = ConsoleKey.D0 Or choice = ConsoleKey.NumPad0
        End If
    End Sub

    Sub NewTeam(ByVal company As String)
        Dim choice As ConsoleKey
        Dim userInput, teamCodeInput As String
        Dim userValid As Odbc.OdbcDataReader
        Dim userFound, reconsidered As Boolean
        Dim searchQuery As New Odbc.OdbcCommand("", conn)
        Dim TeamAdd As New Odbc.OdbcCommand("", conn)
        Do
            Console.WriteLine("What is the Legal Name of the first employee in your new team?")
            userInput = SQLmaintenance(Console.ReadLine, 30)
            searchQuery.CommandText = "SELECT `Username`, `Legal Name` FROM Users WHERE `Legal Name` = '" & userInput & "';"
            userValid = searchQuery.ExecuteReader
            userValid.Read()
            If userValid("Legal Name") = userInput Then
                Console.WriteLine("User Found")
                userFound = True
                Console.WriteLine("What is your 8 character team code?")
                teamCodeInput = SQLmaintenance(Console.ReadLine, 8)
                TeamAdd.CommandText = "INSERT INTO Teams Values ('" & company & "', '" & teamCodeInput & "');"
                TeamAdd.ExecuteNonQuery()
                TeamAdd.CommandText = "INSERT INTO TeamMapping Values ('" & teamCodeInput & "', '" & userValid("Username") & "');"
                TeamAdd.ExecuteNonQuery()
            Else
                Console.WriteLine("User not found. Press Esc to quit team making, or any other key to reenter the teammate's Legal Name.")
                choice = Console.ReadKey.Key

            End If
        Loop Until userFound Or reconsidered Or choice = ConsoleKey.Escape
        If choice <> ConsoleKey.Escape Then
            choice = ConsoleKey.A
            Do
                Console.WriteLine("Press Esc to quit team making, or any other key to add another team member")
                choice = Console.ReadKey.Key
                If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then

                    Console.WriteLine("What is the Legal Name of the next employee in your team?")
                    userInput = SQLmaintenance(Console.ReadLine, 30)
                    searchQuery.CommandText = "SELECT ' FROM Users Where `Legal Name` = '" & userInput & "';"
                    userValid = searchQuery.ExecuteReader
                    If userValid("Legal Name") = userInput Then
                        Console.WriteLine("User Found")
                        userFound = True
                        TeamAdd.CommandText = "INSERT INTO TeamMapping Values ('" & teamCodeInput & "', '" & userInput & "');"
                        TeamAdd.ExecuteNonQuery()
                    Else
                        Console.WriteLine("User not found. Press Esc to quit team making, or any other key to reenter the teammate's Legal Name.")
                        choice = Console.ReadKey.Key
                    End If
                End If
            Loop Until choice = ConsoleKey.Escape
        End If
    End Sub




    Sub SearchUsers(ByVal username As String)
        Dim passcode As String
        Dim query As String = "SELECT * From Users WHERE Username = "
        Dim checker As Odbc.OdbcDataReader
        query &= ("'" & username & "' AND Passcode = ")
        Console.WriteLine("Please enter your Passcode")
        passcode = SQLmaintenance(Console.ReadLine, 45)
        query &= ("'" & passcode & "';")
        Dim searchTerm As New Odbc.OdbcCommand(query, conn)
        checker = searchTerm.ExecuteReader
        If checker.Read() Then
            Console.WriteLine("
            " & checker("UUC") & " | " & checker("Legal Name") & " | " & checker("Company") & " | " & checker("User Level") & " | " & checker("Username") & " | " & checker("Passcode") & " | " & checker("Phone Num"))
            Console.WriteLine("
Press any key to return to main menu")
            Console.ReadKey()
        Else
            Console.WriteLine("No match for these credentials. Check your credentials and try again")
            Console.ReadKey()

        End If

    End Sub

    Sub THE_BIG_ONE(ByVal company As String)
        Dim counter, tempcost, marker As Integer
        Dim employeeAccountedFor As Boolean
        Dim tempPos As String
        Dim unvisited, visited As New Dictionary(Of String, Node)
        Dim tempInputList As New List(Of String)
        Dim ToBeDone As New List(Of Job)
        Dim teamToDoIt As New List(Of Employee)
        Dim tempJob As Job = Nothing
        Dim tempEmployee As Employee = Nothing
        tempEmployee.Quals = New List(Of String)
        tempEmployee.NodePaths = New List(Of String)
        Dim UndoneTerm As New Odbc.OdbcCommand("SELECT `UCCs`.`UCC`, `JobTypes`.`QualName`, `JobsDone`.`UJC` FROM `UCCs`, `JobsDone`, `JobTypes`, `CompanyClient` WHERE `Uccs`.`ClientName` = `JobsDone`.`ClientName` AND `JobsDone`.`JobComplete` = FALSE AND `JobsDone`.`Username` IS NULL AND `JobsDone`.`JobID` = `JobTypes`.`JobID` AND `UCCs`.`ClientName` = `CompanyClient`.`ClientName` and `CompanyClient`.`Company` = '" & company & "' ORDER BY `JobsDone`.`JobComplete` ASC;", conn)
        Dim UpdateTerm As New Odbc.OdbcCommand("", conn)
        Dim TeamAssembler As New Odbc.OdbcCommand("SELECT `UUC`, `QualName` FROM `Users`, `TeamMapping`, `Regions`, `Clients`, `JobsDone`, `Qualmapping`, `Teams` WHERE `JobsDone`.`ClientName` = `Clients`.`ClientName` AND `Clients`.`Region Name` = `Regions`.`Region Name` AND `Regions`.`TeamID` = `TeamMapping`.`TeamID` AND `TeamMapping`.`Username` = `Users`.`Username` AND `JobsDone`.`JobComplete` = 0 AND `Users`.`Username` = `Qualmapping`.`Username` AND `Users`.`User Level` = 'Employee' AND `TeamMapping`.`TeamID` = `Teams`.`TeamID` AND `Teams`.`Company` = '" & company & "' ORDER BY UUC ASC;", conn)
        Dim searcher As Odbc.OdbcDataReader
        Dim regionTerm As New Odbc.OdbcCommand("SELECT `Regions`.`Region Name`, `Region Scale` FROM `Regions`, `Clients`, `JobsDone` WHERE `Regions`.`Region Name` = `Clients`.`Region Name` and `Clients`.`ClientName` = `JobsDone`.`ClientName` ORDER BY `JobsDone`.`JobComplete` ASC;", conn)
        Dim regionLoad = regionTerm.ExecuteReader
        regionLoad.Read()
        Dim scale As Integer = regionLoad("Region Scale") * 40 + 100
        Dim regionName As String = regionLoad("Region Name")
        Dim adjacencymatrix(scale, scale) As Integer
        Using reader As BinaryReader =
                    New BinaryReader(File.Open((regionName & ".region"), FileMode.Open))
            For i = 0 To scale - 1
                For j = 0 To scale - 1
                    adjacencymatrix(i, j) = reader.ReadInt32
                Next
            Next
        End Using
        searcher = UndoneTerm.ExecuteReader
        While searcher.Read
            tempJob.Destination = searcher("UCC")
            tempJob.Qualification = searcher("QualName")
            tempJob.UJC = searcher("UJC")
            ToBeDone.Add(tempJob)
        End While


        searcher.Close()
        searcher = TeamAssembler.ExecuteReader
        While searcher.Read
            counter += 1

            employeeAccountedFor = False
            If teamToDoIt.Count > 0 Then
                For i = 0 To teamToDoIt.Count - 1
                    If teamToDoIt(i).Home = searcher("UUC") Then
                        teamToDoIt(i).Quals.Add(searcher("QualName"))
                        employeeAccountedFor = True
                    End If
                Next
            End If
            If Not employeeAccountedFor Then
                tempEmployee.Home = searcher("UUC")
                tempEmployee.PositionUpdate(searcher("UUC"))
                tempEmployee.NodePaths.Add(searcher("UUC"))
                tempEmployee.Quals.Add(searcher("QualName"))
                teamToDoIt.Add(tempEmployee)
            End If
            tempEmployee.Quals = New List(Of String)
            tempEmployee.NodePaths = New List(Of String)
        End While


        For i = 0 To ToBeDone.Count - 1
            tempcost = Integer.MaxValue
            For j = 0 To teamToDoIt.Count - 1
                If teamToDoIt(j).Quals.Contains(ToBeDone(i).Qualification) Then
                    tempInputList.Clear()
                    tempInputList.Add(ToBeDone(i).Destination)
                    If tempcost > DijkstraChain(unvisited, visited, scale, adjacencymatrix, 1, tempInputList, teamToDoIt(j).currentPosition, 2) Then
                        tempcost = DijkstraChain(unvisited, visited, scale, adjacencymatrix, 1, tempInputList, teamToDoIt(j).currentPosition, 2)
                        tempPos = teamToDoIt(j).currentPosition
                    End If
                End If
            Next
            For k = 0 To teamToDoIt.Count - 1
                If teamToDoIt(k).currentPosition = tempPos Then
                    teamToDoIt(k).CostUpdate(tempcost)
                    teamToDoIt(k).PositionUpdate(tempPos)
                    teamToDoIt(k).NodePaths.AddRange(DijkstraChain(unvisited, visited, scale, adjacencymatrix, 1, tempInputList, teamToDoIt(k).currentPosition, 1))
                    marker = k
                End If
            Next
            For l = 0 To teamToDoIt.Count - 1
                If teamToDoIt(l).currentPosition = tempPos Then
                    teamToDoIt(l).CostUpdate(tempcost)
                    teamToDoIt(l).PositionUpdate(tempPos)
                    teamToDoIt(l).NodePaths.Add(teamToDoIt(l).currentPosition)
                    l = marker
                End If
            Next
            tempInputList.Clear()
            tempInputList.Add(teamToDoIt(marker).Home)
            If teamToDoIt(marker).MinutesWorked + DijkstraChain(unvisited, visited, scale, adjacencymatrix, 1, tempInputList, teamToDoIt(marker).currentPosition, 2) > 480 Then
                teamToDoIt(marker).NodePaths.Add(DijkstraChain(unvisited, visited, scale, adjacencymatrix, 1, tempInputList, teamToDoIt(marker).currentPosition, 1))
                teamToDoIt(marker).PositionUpdate(teamToDoIt(marker).Home)
                teamToDoIt.RemoveAt(marker)
            End If
            UpdateTerm.CommandText = ("UPDATE `JobsDone` SET `JobsDone`.`Username` = (SELECT `Username` FROM `Users` WHERE `Users`.`UUC` =  '" & teamToDoIt(marker).Home & "') WHERE UJC = '" & ToBeDone(i).UJC & "'; ")
            UpdateTerm.ExecuteNonQuery()

        Next



        Console.WriteLine("Done")
        Console.ReadKey()
    End Sub



    Sub RegionCreator()

        Dim team, regionName As String
        Dim regionMatrix(,) As Integer
        Dim query As String = ""
        Dim scale As Integer
        Dim isUnique As Boolean
        Dim choice As ConsoleKey
        Do
            Console.WriteLine("What would you like to call this region?")
            regionName = SQLmaintenance(Console.ReadLine, 50)
            Try
                isUnique = False
                Using reader As BinaryReader =
            New BinaryReader(File.Open((regionName & ".region"), FileMode.Open))
                End Using
            Catch noFile As FileNotFoundException
                isUnique = True
            End Try
            If Not isUnique Then
                Console.WriteLine("Region Already exists. Do you want to
1. Update Region
2. Enter new Region Name

Or press Backspace to go back")
                choice = Console.ReadKey.Key
                If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then
                    isUnique = True
                End If
            ElseIf choice = ConsoleKey.Backspace Then
                Exit Sub
            End If
        Loop Until isUnique

        query &= "'" & regionName & "', "
        Console.WriteLine("What Scale (Between 1 and 10) do you want your region to be?")
        Do
            scale = Console.ReadLine
            If scale < 1 Or scale > 10 Then
                Console.WriteLine("Please stay within the bounds of 1 and 10")
            End If
        Loop Until scale > 0 And scale < 11
        query &= "'" & scale & "', "
        Console.WriteLine("What's the Team Code of the Team that you want to cover this region?")
        team = SQLmaintenance(Console.ReadLine, 8)
        query &= "'" & team & "'"
        Dim command2 As New Odbc.OdbcCommand("INSERT INTO Regions Values (" & query & ");", conn)
        command2.ExecuteNonQuery()
        scale = 100 + scale * 40
        regionMatrix = MatrixMaker(scale)
        Using writer As BinaryWriter =
                New BinaryWriter(File.Open(regionName & ".region", FileMode.OpenOrCreate))
            For i = 0 To scale - 1
                For j = 0 To scale - 1
                    writer.Write(regionMatrix(i, j))
                Next


            Next

        End Using
        Console.WriteLine("Region Created. Press any key to exit")
        Console.ReadKey()
    End Sub

    Sub AddJob()
        Dim choice1Passes, choice2Passes As Boolean
        Dim jobID, codeRead, client, uccRead, spacer As String
        Dim IDTerm As New Odbc.OdbcCommand("SELECT `JobID`, `JobDesc` FROM `JobTypes`;", conn)
        Dim UCCTerm As New Odbc.OdbcCommand("SELECT `ClientName`, `UCC` FROM `UCCs`;", conn)
        Dim teamTerm As New Odbc.OdbcCommand("SELECT `TeamID` FROM `Regions`;", conn)
        Dim searcher As Odbc.OdbcDataReader
        Do
            searcher = IDTerm.ExecuteReader
            Console.WriteLine("Add the job type that the job is, or enter ? to see all job types.")
            jobID = SQLmaintenance(Console.ReadLine, 8)
            If jobID = "?" Then
                While searcher.Read
                    spacer = "       "
                    codeRead = searcher("JobID")
                    spacer = codeRead & Mid(spacer, codeRead.Length, 8 - codeRead.Length)
                    Console.WriteLine(spacer & searcher("JobDesc"))
                End While
                searcher.Close()
                searcher = IDTerm.ExecuteReader
            Else
                While searcher.Read
                    If jobID = searcher("JobID") Then
                        choice1Passes = True
                    End If
                End While
                If Not choice1Passes Then
                    Console.WriteLine("Job type not found. Press any key to try again.")
                    Console.ReadKey()
                End If
            End If
            searcher.Close()
        Loop Until choice1Passes


        Do
            searcher = UCCTerm.ExecuteReader
            Console.WriteLine("Enter the UCC of the client to be served or press ? to see all UCCs")
            uccRead = SQLmaintenance(Console.ReadLine, 8)
            If uccRead = "?" Then
                While searcher.Read
                    spacer = "   "
                    uccRead = searcher("UCC")
                    spacer = uccRead & spacer
                    Console.WriteLine(spacer & searcher("ClientName"))
                End While
                searcher.Close()
                searcher = UCCTerm.ExecuteReader
            Else
                While searcher.Read
                    If uccRead = searcher("UCC") Then
                        choice2Passes = True
                    End If
                End While
                If Not choice2Passes Then
                    Console.WriteLine("Client not found. Press any key to try again.")
                    Console.ReadKey()
                End If
            End If
            searcher.Close()
        Loop Until choice2Passes

        searcher = UCCTerm.ExecuteReader
        While searcher.Read
            If uccRead = searcher("UCC") Then
                client = searcher("ClientName")
            End If
        End While

        Dim jobEntry As New Odbc.OdbcCommand("INSERT INTO `JobsDone` (`JobID`, `ClientName`) VALUES(" & jobID & ", '" & client & "');", conn)
        jobEntry.ExecuteNonQuery()
        Console.WriteLine("Job Added Successfully. Press any key to return to the main menu.")
        Console.ReadKey()
        Console.Clear()
    End Sub
    Sub productivitycheck(ByVal company As String)
        Dim choice As ConsoleKey
        Dim TempCommand As New Odbc.OdbcCommand("", conn)
        Dim tempreader As Odbc.OdbcDataReader
        Do
            Console.WriteLine("Would you like to view:
1. Completed Jobs
2. Uncompleted Assigned Jobs
3. Uncompleted Unassigned Jobs
0 Return To Main Menu")
            choice = Console.ReadKey.Key
            Console.Clear()

            If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then
                TempCommand.CommandText = "SELECT `UJC`, `Legal Name`, `ClientName`, `JobID`, `WorkDate`, `WorkStart`, `WorkEnd` FROM `JobsDone`, `Users` WHERE `JobsDone`.`Username` = `Users`.`Username` AND `JobsDone`.`JobComplete` = True;"
                tempreader = TempCommand.ExecuteReader
                If tempreader.Read Then
                    tempreader.Close()
                    tempreader = TempCommand.ExecuteReader
                    Console.WriteLine("Guide: The table will be arranged as follows
Unique Job Code  |  Legal Name  |  Client Name |  JobID  |  Work Date  |  Work Start  |  Work End

Press any key to continue")
                    Console.ReadKey()
                    Console.Clear()
                    Console.WriteLine("Guide: The table will be arranged as follows
Unique Job Code  |  Legal Name  |  Client Name |  JobID  |  Work Date  |  Work Start  |  Work End

Press any key to continue")
                    While tempreader.Read
                        Console.WriteLine(tempreader("UJC") & "  |  " & tempreader("Legal Name") & "  |  " & tempreader("ClientName") & "  |  " & tempreader("JobID") & "  |  " & Convert.ToString(tempreader("WorkDate")) & "  |  " & Convert.ToString(tempreader("WorkStart")) & "  |  " & Convert.ToString(tempreader("WorkEnd")))
                    End While
                Else
                    Console.WriteLine("No data to be shown")
                End If
                Console.WriteLine("


Press any key to return to the productivity menu")
                Console.ReadKey()
                Console.Clear()
                tempreader.Close()

            ElseIf choice = ConsoleKey.D2 Or choice = ConsoleKey.NumPad2 Then
                TempCommand.CommandText = "SELECT `UJC`, `Legal Name`, `ClientName`, `JobID`FROM `JobsDone`, `Users` WHERE `JobsDone`.`Username` = `Users`.`Username` AND `JobsDone`.`JobComplete` = False;"
                tempreader = TempCommand.ExecuteReader
                If tempreader.Read Then
                    tempreader.Close()
                    tempreader = TempCommand.ExecuteReader
                    Console.WriteLine("Guide: The table will be arranged as follows
Unique Job Code  |  Legal Name  |  Client Name |  JobID  

Press any key to continue")
                    Console.ReadKey()
                    Console.Clear()
                    Console.WriteLine("Guide: The table will be arranged as follows
Unique Job Code  |  Legal Name  |  Client Name |  JobID  

Press any key to continue")
                    While tempreader.Read
                        Console.WriteLine(tempreader("UJC") & "  |  " & tempreader("Legal Name") & "  |  " & tempreader("ClientName") & "  |  " & tempreader("JobID"))
                    End While
                Else
                    Console.WriteLine("No data to be shown")
                End If
                Console.WriteLine("


Press any key to return to the productivity menu")
                Console.ReadKey()
                Console.Clear()
                tempreader.Close()
            ElseIf choice = ConsoleKey.D3 Or choice = ConsoleKey.NumPad3 Then
                TempCommand.CommandText = "SELECT `UJC`, `ClientName`, `JobID`FROM `JobsDone` WHERE Username IS NULL AND `JobsDone`.`JobComplete` = False;"
                tempreader = TempCommand.ExecuteReader
                If tempreader.Read Then
                    tempreader.Close()
                    tempreader = TempCommand.ExecuteReader
                    Console.WriteLine("Guide: The table will be arranged as follows
Unique Job Code  |  Client Name |  JobID  

Press any key to continue")
                    Console.ReadKey()
                    Console.Clear()
                    Console.WriteLine("Guide: The table will be arranged as follows
Unique Job Code  |  Client Name |  JobID  

Press any key to continue")
                    While tempreader.Read
                        Console.WriteLine(tempreader("UJC") & "  |  " & tempreader("ClientName") & "  |  " & tempreader("JobID"))
                    End While
                Else
                    Console.WriteLine("No data to be shown")
                End If
                Console.WriteLine("


Press any key to return to the productivity menu")
                Console.ReadKey()
                tempreader.Close()
                Console.Clear()
            End If

        Loop Until choice = ConsoleKey.D0 Or choice = ConsoleKey.NumPad0
        Console.Clear()
    End Sub
    Sub JobLog(ByVal username As String)
        Dim doYouEvenWorkHere As Boolean
        Dim UJCtoMark As Integer
        Dim jobstart As String
        Dim line As String = ""
        Console.WriteLine("Please enter the UJC number of the job you've completed")
        Console.WriteLine("UJC       |JobID|Client Name")
        Dim jobstoMark As New Odbc.OdbcCommand("SELECT `UJC`, `JobID`, `ClientName` FROM `JobsDone` WHERE `Username` = '" & username & "' AND `JobComplete` = FALSE;", conn)
        Dim searcher As Odbc.OdbcDataReader = jobstoMark.ExecuteReader
        While searcher.Read
            doYouEvenWorkHere = True
            line = searcher("UJC")
            For i = 1 To 10 - Convert.ToString(searcher("UJC")).Length
                line &= " "
            Next
            line &= "|" & searcher("JobID")
            For i = 1 To 5 - Convert.ToString(searcher("JobID")).Length
                line &= " "
            Next
            line &= "|" & searcher("ClientName")
            Console.WriteLine(line)

        End While
        searcher.Close()
        If doYouEvenWorkHere Then
            UJCtoMark = Console.ReadLine
            jobstoMark.CommandText = "SELECT `UJC` FROM `JobsDone` WHERE `Username` = '" & username & "' AND `JobComplete` = FALSE AND `UJC`= " & UJCtoMark & ";"
            searcher = jobstoMark.ExecuteReader
            searcher.Read()
            Console.WriteLine("What time did you start the job (in the format XX:XX)?")
            jobstart = Console.ReadLine
            Dim jobCompleter As New Odbc.OdbcCommand("UPDATE `JobsDone` SET `JobComplete` = TRUE, `WorkDate` = CAST(CURRENT_TIMESTAMP As Date), `WorkStart` = '" & jobstart & "',`WorkEnd` = CAST(CURRENT_TIMESTAMP As Time(0)) WHERE `UJC` = " & searcher("UJC") & ";", conn)
            jobCompleter.ExecuteNonQuery()
            Console.WriteLine("Job Completion Logged! Press any key to return to main menu.")
        Else
            Console.Clear()
            Console.WriteLine("You currently have no assigned jobs. Press any key to return to main menu.")
        End If
        Console.ReadKey()
        Console.Clear()
    End Sub
    Sub QualificationHub(ByVal company As String)
        Dim choice As ConsoleKey
        Dim tempcommand As New Odbc.OdbcCommand("", conn)
        Dim tempreader As Odbc.OdbcDataReader
        Dim user, qual As String
        Do
            Console.WriteLine("Would You like to
1. View Qualifications
2. View or add Employee qualifications
0. Return to Main Menu")
            choice = Console.ReadKey.Key
            Console.WriteLine()
            If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then
                tempcommand.CommandText = "SELECT * FROM Qualifications"
                tempreader = tempcommand.ExecuteReader
                While tempreader.Read
                    Console.WriteLine(tempreader("QualName") & "  |  " & tempreader("QualDesc"))
                End While
                Console.WriteLine("Press 0 to return to main menu, or press any other key to return to qualification menu")
                choice = Console.ReadKey.Key
                tempreader.Close()
                Console.Clear()
            ElseIf choice = ConsoleKey.D2 Or choice = ConsoleKey.NumPad2 Then
                tempcommand.CommandText = "SELECT `Legal name`, `Qualname` FROM QualMapping, Users WHERE Users.username = qualmapping.username and users.company = '" & company & "';"
                tempreader = tempcommand.ExecuteReader
                While tempreader.Read
                    Console.WriteLine(tempreader("Legal Name") & "  |  " & tempreader("QualName"))
                End While
                tempreader.Close()
                Console.WriteLine("Press 1 to Add a qualification, press 0 to return to main menu, or press any other key to return to qualification menu")
                choice = Console.ReadKey.Key
                Console.WriteLine()

                If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then
                    Console.WriteLine("Which user would you like to add a qualification to?")
                    user = SQLmaintenance(Console.ReadLine, 30)
                    Console.WriteLine("Which Qualification would you like to add?")
                    qual = SQLmaintenance(Console.ReadLine, 30)
                    tempcommand.CommandText = "INSERT INTO qualmapping Values((Select Username from users where `legal name` = '" & user & "' AND company = '" & company & "'), '" & qual & "');"
                    tempcommand.ExecuteNonQuery()
                End If
                tempreader.Close()
                Console.WriteLine("Done

Press any key to return to qualification menu")
                Console.ReadKey()
                Console.Clear()
            End If
        Loop Until choice = ConsoleKey.D0 Or choice = ConsoleKey.NumPad0
    End Sub
    Sub ShowJobs(ByVal username As String)
        Dim adjacencymatrix(,) As Integer
        Dim TempCommand As New Odbc.OdbcCommand("", conn)
        Dim tempreader As Odbc.OdbcDataReader
        Dim tempCurrentPos As String
        Dim tempDestinationlist As New List(Of String)
        tempDestinationlist.Add("")
        TempCommand.CommandText = "SELECT UUC FROM `Users` WHERE `Username` = '" & username & "';"
        tempreader = TempCommand.ExecuteReader
        tempreader.Read()
        tempCurrentPos = tempreader("UUC")
        tempreader.Close()
        TempCommand.CommandText = "SELECT * FROM `JobsDone` WHERE Username = '" & username & "' AND Jobcomplete = FALSE;"
        tempreader = TempCommand.ExecuteReader
        If tempreader.Read Then
            Console.WriteLine("UJC| JobID |  Client Name")
        End If
        tempreader.Close()
        tempreader = TempCommand.ExecuteReader
        While tempreader.Read

            Console.WriteLine(tempreader("UJC") & "|  " & tempreader("JobID") & "  |  " & tempreader("ClientName"))
        End While
        tempreader.Close()
        TempCommand.CommandText = "SELECT `UCC` FROM `UCCs`, `JobsDone` WHERE `JobsDone`.`Username` = '" & username & "' AND `JobsDone`.`WorkDate` = CAST(CURRENT_TIMESTAMP As Date) AND `JobsDone`.`ClientName` = `UCCs`.`ClientName` AND `JobsDone`.`JobComplete` = TRUE;"
        tempreader = TempCommand.ExecuteReader
        While tempreader.Read
            tempCurrentPos = tempreader("UCC")
        End While
        tempreader.Close()
        TempCommand.CommandText = "SELECT `UCC` FROM `UCCs`, `JobsDone` WHERE `JobsDone`.`Username` = '" & username & "' AND `JobsDone`.`ClientName` = `UCCs`.`ClientName` AND `JobsDone`.`JobComplete` = FALSE;"
        tempreader = TempCommand.ExecuteReader

        If tempreader.Read() Then
            tempDestinationlist(0) = tempreader("UCC")
        End If
        tempreader.Close()
        TempCommand.CommandText = "SELECT `Region Scale`, `Region Name` FROM `Regions`, `TeamMapping` WHERE `TeamMapping`.`Username` = '" & username & "' AND `TeamMapping`.`TeamID` = `Regions`.`TeamID`;"
        tempreader = TempCommand.ExecuteReader
        tempreader.Read()
        Dim scale As Integer = tempreader("Region Scale") * 40 + 100
        ReDim adjacencymatrix(scale, scale)
        Using reader As BinaryReader =
            New BinaryReader(File.Open((tempreader("Region Name") & ".region"), FileMode.Open))
            For i = 0 To scale - 1
                For j = 0 To scale - 1
                    adjacencymatrix(i, j) = reader.ReadInt32
                Next
            Next
        End Using
        If tempDestinationlist(0) = "" Then
            tempDestinationlist(0) = tempCurrentPos
        End If
        If DijkstraChain(New Dictionary(Of String, Node), New Dictionary(Of String, Node), scale, adjacencymatrix, 1, tempDestinationlist, tempCurrentPos, 2) <> 0 Then
            Console.WriteLine("Your next journey is as follows:

")
            Dim abstractText As List(Of String) = DijkstraChain(New Dictionary(Of String, Node), New Dictionary(Of String, Node), scale, adjacencymatrix, 1, tempDestinationlist, tempCurrentPos, 1)
            For i = 0 To abstractText.Count - 1
                Console.WriteLine(abstractText(i))
            Next
            Console.WriteLine("

Press any key to return to the main menu")
            Console.ReadKey()
            Console.Clear()
        Else
            Console.WriteLine("You currently have nowhere to go to.")
            Console.WriteLine("

Press any key to return to the main menu")
            Console.ReadKey()
            Console.Clear()
        End If
    End Sub
    Sub Main()
        conn.Open()
        Dim signedIn As Integer = 0
        Dim choice As ConsoleKey
        Dim currentUser As New User()
        Do
            If currentUser.PowerLevel() = 0 Or signedIn = 0 Or signedIn = 2 Then
                Do

                    Console.Clear()
                    Console.WriteLine("1 to register
2 to register a company
3 to Sign In
0 to exit")
                    choice = Console.ReadKey.Key
                    Console.Clear()

                    If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then
                        AddEmployee("Employee")
                    ElseIf choice = ConsoleKey.D2 Or choice = ConsoleKey.NumPad2 Then
                        NewCompany()
                    ElseIf choice = ConsoleKey.D3 Or choice = ConsoleKey.NumPad3 Then
                        If signedIn = 0 Then
                            currentUser.SignIn()
                            If currentUser.returnUsername = "" Then
                                signedIn = 0
                            Else
                                signedIn = 1
                            End If
                        ElseIf signedIn = 1 Then
                            Console.WriteLine("You are already signed in")
                        ElseIf signedIn = 2 Then
                            Console.WriteLine("Sign in as " & currentUser.Greet & "? (Y/N)")
                            choice = Console.ReadKey.Key
                            Console.Clear()

                            If choice = ConsoleKey.Y Then
                                currentUser.checkPassword()
                            ElseIf choice = ConsoleKey.N Then
                                currentUser.SignIn()
                                signedIn = 1
                            End If
                        End If

                    End If

                Loop Until choice = ConsoleKey.D0 Or choice = ConsoleKey.NumPad0 Or signedIn = 1

            ElseIf currentUser.PowerLevel = 1 Or currentUser.PowerLevel = 2 And signedIn = 1 Then
                Do
                    If signedIn = 1 Then
                        Console.WriteLine("Welcome " & currentUser.Greet() & ".")
                    End If
                    Console.WriteLine("
1 to view your personal data
2 to Log A Completed Job
3 to View Your Next Jobs
Backspace to Log Out")
                    choice = Console.ReadKey.Key
                    Console.Clear()

                    If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then
                        SearchUsers(currentUser.returnUsername)
                    ElseIf choice = ConsoleKey.D2 Or choice = ConsoleKey.NumPad2 Then
                        JobLog(currentUser.returnUsername)
                    ElseIf choice = ConsoleKey.D3 Or choice = ConsoleKey.NumPad3 Then
                        ShowJobs(currentUser.returnUsername)
                    ElseIf choice = ConsoleKey.Backspace Then
                        Console.WriteLine("You've been logged out.")
                        signedIn = 2
                        Console.ReadKey()
                    End If
                Loop Until choice = ConsoleKey.D0 Or choice = ConsoleKey.NumPad0 Or signedIn = 2


            ElseIf currentUser.PowerLevel = 3 And signedIn = 1 Then
                If signedIn = 1 Then
                    Console.WriteLine("Welcome " & currentUser.Greet() & ".")
                End If
                Console.WriteLine("
1 to View or Edit Team Composition
2 to Add or Edit a Region
3 to add a Client
4 to Add a Job
5 to View Productivity Data
6 to Calculate working day
7 to Demonstrate Pathfinding
8 to Manage Qualifications
Backspace to Log Out")

                choice = Console.ReadKey.Key
                Console.Clear()

                If choice = ConsoleKey.D1 Or choice = ConsoleKey.NumPad1 Then
                    TeamComp(currentUser.GiveCompany)
                ElseIf choice = ConsoleKey.D2 Or choice = ConsoleKey.NumPad2 Then
                    RegionCreator()
                ElseIf choice = ConsoleKey.D3 Or choice = ConsoleKey.NumPad3 Then
                    AddClient(currentUser.GiveCompany)
                ElseIf choice = ConsoleKey.D4 Or choice = ConsoleKey.NumPad4 Then
                    AddJob()
                ElseIf choice = ConsoleKey.D5 Or choice = ConsoleKey.NumPad5 Then
                    productivitycheck(currentUser.GiveCompany)
                ElseIf choice = ConsoleKey.D6 Or choice = ConsoleKey.NumPad6 Then
                    THE_BIG_ONE(currentUser.GiveCompany)
                ElseIf choice = ConsoleKey.D7 Or choice = ConsoleKey.NumPad7 Then
                    DijkstraExample()
                ElseIf choice = ConsoleKey.D8 Or choice = ConsoleKey.NumPad8 Then
                    QualificationHub(currentUser.GiveCompany)
                ElseIf choice = ConsoleKey.Backspace Then
                    Console.WriteLine("You've been logged out.")
                    signedIn = 2
                    Console.ReadKey()
                End If
            End If
        Loop Until choice = ConsoleKey.D0 Or choice = ConsoleKey.NumPad0
        conn.Close()

    End Sub

End Module

