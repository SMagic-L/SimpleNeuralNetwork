Class NeuralNetwork
    Private Const DATAFILE_SIGN As String = "SimpleNeuralNetwork_DATAFILE_DO_NOT_EDIT"

    Private Function IsNewOK(ByRef layerNum As Integer, ByRef cellNum As Integer()) As Boolean
        '检查输入
        If layerNum <= 0 Then
            Throw New ArgumentException("神经网络层数layerNum不是正数")
            Return False
        End If
        If cellNum.Length < layerNum Then
            Throw New ArgumentException("神经元个数cellNum的组数小于层数layerNum")
            Return False
        End If
        For Each s In cellNum
            If s <= 0 Then
                Throw New ArgumentException("神经元个数cellNum不是正数")
                Return False
            End If
        Next

        Return True
    End Function

    Private Sub Initialize(ByRef layerNum As Integer, ByRef cellNum As Integer())
        '初始化layers
        ReDim Layers(layerNum - 1)
        For i = 0 To layerNum - 1
            Layers(i) = New NeuralLayer(cellNum(i))
        Next

        '初始化weights
        ReDim Weight(layerNum - 1)
        For i = 1 To layerNum - 1
            Weight(i) = New NeuralWeight(Layers(i - 1), Layers(i))
        Next

        '初始化其他属性
        Data = Nothing
        LearningRate = 0.5
        Generation = 1
    End Sub

    Sub New(layerNum As Integer, cellNum As Integer())
        '检查输入
        If Not IsNewOK(layerNum, cellNum) Then
            Exit Sub
        End If

        '初始化
        Initialize(layerNum, cellNum)

    End Sub

    Sub New(ByRef dataContent As String())
        '检查是否合规
        If dataContent(0) <> DATAFILE_SIGN Then
            Throw New ArgumentException("不是有效的SimpleNeuralNetwork数据文件")
            Exit Sub
        End If

        '获取layerNum 与 cellNum
        Dim cellNumStr As String() = dataContent(1).Split({" "}, StringSplitOptions.RemoveEmptyEntries)
        Dim layerNum As Integer = cellNumStr.GetLength(0)
        Dim cellNum As Integer()
        ReDim cellNum(layerNum - 1)
        For i = 0 To layerNum - 1
            cellNum(i) = cellNumStr(i)
        Next i

        '检查输入
        If Not IsNewOK(layerNum, cellNum) Then
            Exit Sub
        End If

        '初始化
        Initialize(layerNum, cellNum)

        '写入weights
        Dim s As Integer = 0
        Dim tmp As String()
        For l = 1 To layerNum - 1
            For i = 1 To cellNum(l - 1)
                tmp = dataContent(2 + i + s).Split({" "}, StringSplitOptions.RemoveEmptyEntries)
                For j = 1 To cellNum(l)
                    Weight(l).Item.SetValue(tmp(j - 1), i - 1, j - 1)
                Next j
            Next i
            s += cellNum(l - 1) + 1
        Next l

    End Sub

    Public Sub Train()
        '检查输入
        If Not IsOKForTrain() Then
            Exit Sub
        End If

        '声明变量
        Dim outerLayerIndex As Integer = Layers.GetUpperBound(0)
        Dim dE_dW As Single(,)

        For g = 1 To Generation
            For n = 0 To Data.GetUpperBound(1)
                '正反传播，计算误差
                SetCurrentNetwork(n)

                '计算dEdW以更新weight
                For l = outerLayerIndex To 1 Step -1
                    ReDim dE_dW(Weight(l).Item.GetUpperBound(0), Weight(l).Item.GetUpperBound(1))
                    GetCurrentdEdW(dE_dW, l)
                    UpdateCurrentWeight(dE_dW, l)
                Next l
            Next n
        Next g

        Save(Date.Now)

    End Sub

    Public Function Query(key As Single()) As Single()
        Dim i As Integer

        Layers(0).O = key
        For i = 1 To Layers.GetUpperBound(0)
            Layers(i).O = Sigmoid(Weight(i).Multiply(Layers(i - 1).O))
        Next i

        Return Layers(Layers.GetUpperBound(0)).O
    End Function

    Public Sub Save(title As String)
        Dim MyFileStream As New IO.FileStream(IO.Directory.GetCurrentDirectory & title & ".txt", IO.FileMode.OpenOrCreate)
        Dim MyStreamWriter As New IO.StreamWriter(MyFileStream)

        MyStreamWriter.WriteLine(DATAFILE_SIGN)

        '神经元个数
        For i = 0 To Layers.GetUpperBound(0)
            MyStreamWriter.Write(Layers(i).CellNumber)
            MyStreamWriter.Write(" ")
        Next
        MyStreamWriter.WriteLine()
        MyStreamWriter.Flush()

        '神经元权重信息
        For l = 1 To Layers.GetUpperBound(0)
            MyStreamWriter.WriteLine("=========================")
            For i = 0 To Weight(l).Item.GetUpperBound(0)
                For j = 0 To Weight(l).Item.GetUpperBound(1)
                    MyStreamWriter.Write(Weight(l).Item(i, j))
                    MyStreamWriter.Write(" ")
                Next
                MyStreamWriter.WriteLine()
            Next
            MyStreamWriter.Flush()
        Next l

        MyStreamWriter.Dispose()
        MyFileStream.Dispose()
    End Sub

    Private Overloads Function Sigmoid(x As Single) As Single
        Return 1 / (1 + Math.E ^ (-x))
    End Function

    Private Overloads Function Sigmoid(x As Single()) As Single()
        Dim result As Single()
        ReDim result(x.Length - 1)
        For i = 0 To x.Length - 1
            result(i) = 1 / (System.Math.E ^ (-x(i)) + 1)
        Next
        Return result
    End Function

    Private Function IsOKForTrain() As Boolean
        If Data Is Nothing Then
            Throw New ArgumentNullException("NeuralNetwork.Data", "未装载训练集数据")
            Return False
        End If
        If Data.GetLength(0) <> (Layers(0).CellNumber + Layers(Layers.GetUpperBound(0)).CellNumber) Then
            Throw New ArgumentException("训练集数据项数与神经网络输入层项数不符")
            Return False
        End If

        Return True
    End Function

    Private Sub GetCurrentdEdW(ByRef dE_dW As Single(,), layerIndex As Integer)
        Dim sum_Of_WO As Single = 0
        For j = 0 To Layers(layerIndex).CellNumber - 1
            '计算Σ(W(i,j) * O(j))
            For i = 0 To Layers(layerIndex - 1).CellNumber - 1
                sum_Of_WO += Weight(layerIndex).Item(i, j) * Layers(layerIndex - 1).O(i)
            Next i
            '计算dEdW
            Dim sx As Single
            For i = 0 To Layers(layerIndex - 1).CellNumber - 1
                sx = Sigmoid(sum_Of_WO)
                dE_dW(i, j) = -Layers(layerIndex).E(j) * sx * (1 - sx) * Layers(layerIndex - 1).O(i)
            Next i
        Next j
    End Sub

    Private Sub UpdateCurrentWeight(ByRef dE_dW As Single(,), layerIndex As Integer)
        '更新weight
        For i = 0 To Weight(layerIndex).Item.GetUpperBound(0)
            For j = 0 To Weight(layerIndex).Item.GetUpperBound(1)
                Weight(layerIndex).Item(i, j) += -LearningRate * dE_dW(i, j)
            Next j
        Next i
    End Sub

    Private Sub SetCurrentNetwork(dataIndex As Integer)
        Dim outerLayerIndex As Integer = Layers.GetUpperBound(0)

        '输入数据
        For l = 0 To Layers(0).CellNumber - 1
            Layers(0).O.SetValue(Data(l, dataIndex), l)
        Next l

        '正向传播
        For l = 1 To outerLayerIndex
            Layers(l).O = Sigmoid(Weight(l).Multiply(Layers(l - 1).O))
        Next l

        '计算误差
        For c = 0 To Layers(outerLayerIndex).CellNumber - 1
            Layers(outerLayerIndex).E.SetValue(0.5 * (Data(Layers(0).CellNumber + c, dataIndex) - Layers(outerLayerIndex).O(c)) ^ 2, c)
        Next c

        '反向传播
        For l = outerLayerIndex - 1 To 1 Step -1
            Layers(l).E = Weight(l + 1).ToggleMultiply(Layers(l + 1).E)
        Next l
    End Sub

    Public Property Data As Single(,)  'Data的每一项由输入数据与理想输出数据拼接而成

    Private Property Layers As NeuralLayer()

    Private Property Weight As NeuralWeight()

    Public Property Generation As Integer
        Get
            Return _Generation
        End Get
        Set
            If Value <= 0 Then
                Throw New System.ArgumentOutOfRangeException
                Exit Property
            End If
            _Generation = Value
        End Set
    End Property

    Public Property LearningRate As Single
        Get
            Return _LearningRate
        End Get
        Set
            If Value > 1 Or Value < 0 Then
                Throw New System.ArgumentOutOfRangeException
                Exit Property
            End If
            _LearningRate = Value
        End Set
    End Property

    Private _LearningRate As Single
    Private _Generation As Integer

    Private Class NeuralLayer
        Sub New(cellNum As Integer)
            CellNumber = cellNum
            ReDim E(cellNum - 1)
            ReDim O(cellNum - 1)
        End Sub

        Public Property E As Single()

        Public Property O As Single()

        Public ReadOnly Property CellNumber As Integer
    End Class

    Private Class NeuralWeight
        Sub New(ByRef foreLayer As NeuralLayer, ByRef latterLayer As NeuralLayer)
            Dim v As Single(,)
            ReDim v(foreLayer.CellNumber - 1, latterLayer.CellNumber - 1)
            '对v赋随机值
            For i = 0 To v.GetUpperBound(0)
                For j = 0 To v.GetUpperBound(1)
                    v(i, j) = GetRd().NextDouble
                Next j
            Next i
            Count = v.Length
        End Sub
        Public Property Item As Single(,)

        Public ReadOnly Property Count As Integer

        Public Function Multiply(vector As Single()) As Single()
            '检查输入
            If Item.GetUpperBound(1) <> vector.GetUpperBound(0) Then
                Throw New ArgumentException("矩阵类型不同，无法相乘")
                Return Nothing
            End If

            '声明
            Dim i, j As Integer
            Dim result As Single()
            ReDim result(Item.GetUpperBound(0))

            '对矩阵的每个向量点乘
            For i = 0 To Item.GetUpperBound(0)
                For j = 0 To Item.GetUpperBound(1)
                    result(i) += Item(i, j) * vector(j)
                Next j
            Next i

            Return result

        End Function

        Public Function ToggleMultiply(vector As Single()) As Single()
            '检查输入
            If Item.GetUpperBound(0) <> vector.GetUpperBound(0) Then
                Throw New ArgumentException("矩阵类型不同，无法相乘")
                Return Nothing
            End If

            '声明
            Dim i, j As Integer
            Dim result As Single()
            ReDim result(Item.GetUpperBound(0))

            '对矩阵的每个向量点乘
            For j = 0 To Item.GetUpperBound(1)
                For i = 0 To Item.GetUpperBound(0)
                    result(j) += Item(j, i) * vector(i)
                Next i
            Next j

            Return result
        End Function

        Private Function GetRd() As Random
            If _rd Is Nothing Then
                _rd = New Random()
            End If
            Return _rd
        End Function

        Private Shared _rd As Random

    End Class

    Public Shared Operator =(left As NeuralNetwork, right As NeuralNetwork) As Boolean
        Return EqualityComparer(Of NeuralNetwork).Default.Equals(left, right)
    End Operator

    Public Shared Operator <>(left As NeuralNetwork, right As NeuralNetwork) As Boolean
        Return Not left = right
    End Operator

    Public Overrides Function Equals(obj As Object) As Boolean
        Dim network = TryCast(obj, NeuralNetwork)
        Return network IsNot Nothing AndAlso
               EqualityComparer(Of Single(,)).Default.Equals(Data, network.Data) AndAlso
               EqualityComparer(Of NeuralLayer()).Default.Equals(Layers, network.Layers) AndAlso
               EqualityComparer(Of NeuralWeight()).Default.Equals(Weight, network.Weight) AndAlso
               Generation = network.Generation AndAlso
               LearningRate = network.LearningRate AndAlso
               _LearningRate = network._LearningRate AndAlso
               _Generation = network._Generation
    End Function

    Public Overrides Function GetHashCode() As Integer
        Dim hashCode As Long = 163910645
        hashCode = (hashCode * -1521134295 + EqualityComparer(Of Single(,)).Default.GetHashCode(Data)).GetHashCode()
        hashCode = (hashCode * -1521134295 + EqualityComparer(Of NeuralLayer()).Default.GetHashCode(Layers)).GetHashCode()
        hashCode = (hashCode * -1521134295 + EqualityComparer(Of NeuralWeight()).Default.GetHashCode(Weight)).GetHashCode()
        hashCode = (hashCode * -1521134295 + Generation.GetHashCode()).GetHashCode()
        hashCode = (hashCode * -1521134295 + LearningRate.GetHashCode()).GetHashCode()
        Return hashCode
    End Function


    Public Class Test_NN
        Private nw As NeuralWeight
        Sub Test_NeuralWeight()
            nw = New NeuralWeight(New NeuralLayer(3), New NeuralLayer(4))

            Test_Neuralweight_Multiply()


        End Sub
        Sub Test_Neuralweight_Multiply()
            Debug.WriteLine("Test_Neuralweight_Multiply")
            Debug.WriteLine(nw.Item)


            '整数
            Debug.WriteLine("输入整数{1, 5, 4, 8}")
            Debug.WriteLine(nw.Multiply({1, 5, 4, 8}))

            '小数
            Debug.WriteLine("输入小数{1.2, 3.5, 6.6, 4.4}")
            Debug.WriteLine(nw.Multiply({1.2, 3.5, 6.6, 4.4}))


            '类型不符合
            Debug.WriteLine("输入{1.1}")
            Debug.WriteLine(nw.Multiply({1.1}))


            'null
            Debug.WriteLine("输入Nothing")
            Debug.WriteLine(nw.Multiply(Nothing))

        End Sub



    End Class

End Class