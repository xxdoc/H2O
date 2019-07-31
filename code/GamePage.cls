VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "GamePage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'======================================
'   页面绘制器
    Dim Page As GPage
    Dim Map() As Integer
    Dim PX As Single, PY As Single
    Dim XRemain As Single, YRemain As Single
    Dim Orders() As Integer
'======================================
Public Sub CreateBlockCave(Block As H2OType, Depth As Long, Count As Long, Base As H2OType)
    Dim SX As Long, SY As Long, SW As Long, SH As Long
    
    Randomize
    
    For I = 1 To Count
        SX = Int(Rnd * (UBound(Map, 1) - Depth))
        SY = Int(Rnd * (UBound(Map, 2) - Depth))
        
        SW = Int(Rnd * Depth)
        SH = Int(Rnd * Depth)
        
        'Fill
        For X = SX To SX + SW
            For Y = SY To SY + SH
                If Map(X, Y) = Base Then
                    Map(X, Y) = Block
                End If
            Next
        Next
    Next
End Sub
Public Sub CreateMap()
    ReDim Map(2000, 2000)
    
    'Create Blocks
    Call CreateBlockCave(H2ODirt, 4, 800000, 0)
    Call CreateBlockCave(H2OGrass, 4, 600000, H2ODirt)
    Call CreateBlockCave(H2OStone, 4, 100000, H2ODirt)
    Call CreateBlockCave(H2OTree, 4, 150000, H2OGrass)
    
    'Fill
    For X = 0 To UBound(Map, 1)
        For Y = 0 To UBound(Map, 2)
            If Map(X, Y) = 0 Then Map(X, Y) = H2OType.H2OWater
        Next
    Next
    
    PX = 1000: PY = 1000
End Sub
Public Sub Update()
    '游戏每一帧的过程（绘制过程请放在此处）
    
    Page.Clear '清空画布

    Dim DrawW As Long, DrawH As Long, DX As Long, DY As Long
    Dim MapW As Long, MapH As Long
    Dim X As Long, Y As Long
    
    MapW = UBound(Map, 1)
    MapH = UBound(Map, 2)
    
    DrawW = Round(GW / 32)
    DrawH = Round(GH / 32)
    
    For X = PX - DrawW / 2 - 1 To PX + DrawW / 2 + 1
        For Y = PY - DrawH / 2 - 1 To PY + DrawH / 2 + 1
            If X >= 0 And Y >= 0 And X <= MapW And Y <= MapH And Map(X, Y) <> H2OType.H2OWater Then
                DX = GW / 2 - (PX - X) * 32
                DY = GH / 2 - (PY - Y) * 32
                If Map(X, Y) <> 0 Then
                    Page.DrawImage Map(X, Y) * 2 + 1, DX, DY, pos:=posOnCenter
                End If
            End If
        Next
    Next
    
    If XRemain <> 0 Then
        PX = PX + IIf(XRemain > 0, 0.1, -0.1)
        XRemain = XRemain + IIf(XRemain > 0, -0.1, 0.1)
        If Abs(XRemain) < 0.001 Then XRemain = 0: PX = Round(PX)
    End If
    If YRemain <> 0 Then
        PY = PY + IIf(YRemain > 0, 0.1, -0.1)
        YRemain = YRemain + IIf(YRemain > 0, -0.1, 0.1)
        If Abs(YRemain) < 0.001 Then YRemain = 0: PY = Round(PY)
    End If
    
    If IsKeyPress(VK_LEFT) And XRemain = 0 Then XRemain = -1
    If IsKeyPress(VK_RIGHT) And XRemain = 0 Then XRemain = 1
    If IsKeyPress(VK_UP) And YRemain = 0 Then YRemain = -1
    If IsKeyPress(VK_DOWN) And YRemain = 0 Then YRemain = 1
    
    Page.Paint 0, GW / 2, GH / 2, 32, 32, argb(180, 255, 255, 255), pos:=posOnCenter
    Page.Paint 0, GW / 2, GH / 2, 32, 32, argb(100, 0, 0, 0), style:=1, pos:=posOnCenter
End Sub
Public Sub Wheel(Direction As Integer, Depth As Single)
    '鼠标滚轮事件
    'Direction:方向，Depth：深度
End Sub
Public Sub AnimationMsg(ID As String, msg As String)
    '动画消息接收
End Sub
Public Sub Enter()
    '页面进入事件
End Sub
Public Sub Leave()
    '页面离开事件
End Sub
Public Sub Gotfocus()
    '窗口取得焦点事件
End Sub
Public Sub Lostfocus()
    '窗口失去焦点事件
End Sub
Public Sub AnimationDone(ID As Integer)
    '动画播放结束事件
    'id：结束的动画ID
End Sub
Private Sub Class_Initialize()
    '创建页面绘制器
    Set Page = New GPage
    Page.Create Me
    '导入游戏资源
    Page.Res.NewImages App.Path & "\assets\block", 32, 32
    '创建页面
    ECore.Add Page, "GamePage"
    
    Call CreateMap
    
    ReDim Orders(4)
    Orders(0) = H2OType.H2OWater
    Orders(1) = H2OType.H2ODirt
    Orders(2) = H2OType.H2OStone
    Orders(3) = H2OType.H2OGrass
    Orders(4) = H2OType.H2OTree
    '===============================================
    '   如果需要添加动画，请放置在此处
    
    '===============================================
End Sub
