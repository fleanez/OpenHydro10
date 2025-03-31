Imports EnergyExemplar.PLEXOS.Energy
Imports EnergyExemplar.PLEXOS.Core
Imports EEUTILITY.Enums
'Imports EEUTILITY.Functions
Imports EnergyExemplar.PLEXOS.Utility.Enums
Imports EnergyExemplar.PLEXOS.Utility.Constants
Imports EnergyExemplar.PLEXOS.Utility.Functions
Imports EnergyExemplar.PLEXOS.Energy.EnergyModelGlobals
Imports PLEXOSCommon.Controls.CurrentUserButton

Class MyModel
    Implements IOpenModel


    'Constants:
    Private Const USE_ARCHIVE_FOR_MAULE As Boolean = False
    Private Const USE_ARCHIVE_FOR_LAJA_INFLOW As Boolean = False
    Private Const USE_ARCHIVE_FOR_LAJA_MAXVOLUME As Boolean = True

    Private Shared m_vol30NovPerSample As Volume30Nov 'Volumes at Nov 30 in 1000m3
    Private Shared m_stepPeriod30Nov As Integer = -1
    Private Shared m_horizonPeriod30Nov As Integer = -1
    Private Shared m_periodInitMaule As Integer = -1
    Private Shared m_isValidIrr As Boolean = False
    Private Shared m_isInitialized As Boolean = False

    'Laja Storages:
    Private Shared stoElToro As Storage
    Private Shared stoLaja_Gen As Storage
    Private Shared stoLaja_Riego As Storage
    Private Shared stoLaja_Mixto As Storage

    'Maule Storages
    Private Shared stoLMaule As Storage
    Private Shared stoCipreses As Storage
    Private Shared stoInvernada_Eco As Storage
    Private Shared stoMauleLow_Eco_Gen As Storage
    Private Shared stoMauleLow_Eco_Riego As Storage
    Private Shared stoMauleSup_Eco_Gen As Storage
    Private Shared stoMauleSup_Eco_Riego As Storage

    'Other Storages
    Private Shared stoRalco As Storage
    Private Shared stoColbun As Storage

    'Variables:
    Private Shared varQInvFree As Variable
    Private Shared varQFZ As Variable
    Private Shared varVol30Nov As Variable
    Private Shared varLAJAirrigators1
    Private Shared varLAJAirrigators2 As Variable
    Private Shared varLAJAsaltos As Variable
    Private Shared varReserveMinVolRalco As Variable
    Private Shared varReserveMinVolColbun As Variable
    Private Shared varReserveMinVolElToro As Variable
    Private Shared varRiegoTotalMaule As Variable

    Public Sub AfterInitialize() Implements IOpenModel.AfterInitialize

        If (CurrentEnergyModel.CurrentPhase.Index = SimulationPhaseEnum.STSchedule) Then
            InitializePhase()
        End If

    End Sub

    Public Sub BeforeProperties() Implements IOpenModel.BeforeProperties
        Dim strMessage As String = ""

        InitializeStep()
        m_isValidIrr = IsValidIrrigation(strMessage)

        If m_isValidIrr Then
            CurrentEnergyModel.Feedback.LogMessage("Loading Irrigation Agreement pre-calculation...")
        Else
            Return
        End If

        Dim dScalar As Double
        Dim dateBeginHorizon As Date = Date.FromOADate(CurrentEnergyModel.Horizon(HorizonAttributeEnum.DateFrom))
        Dim nDayBeginning As Integer = CInt(CurrentEnergyModel.Horizon(HorizonAttributeEnum.DayBeginning))
        Dim nPeriodsPerDayInLookAhead As Integer = CInt(CurrentEnergyModel.Horizon(HorizonAttributeEnum.LookaheadPeriodsperDay))
        Dim nPeriodsInHorizon As Integer = CurrentEnergyModel.Horizon.PeriodCount(PeriodEnum.Interval)
        Dim dateCur As Date
        Dim nHoursPerInterval As Double = CurrentEnergyModel.Horizon.HoursperInterval
        Dim nHoursPerIntervalInLookAhead As Double = CurrentEnergyModel.Horizon.HoursperInterval * CurrentEnergyModel.Horizon.LookAheadRatio
        Dim curLookAheadPeriod As Integer
        Dim dateBeginStep As Date = dateBeginHorizon.AddHours((CurrentEnergyModel.Steps.CurrentStep - 1) * CurrentEnergyModel.Horizon.ChronoSansLookaheadHoursperStep)
        Dim dateBeginLookAhead As Date = dateBeginStep.AddHours(CurrentEnergyModel.Horizon.ChronoSansLookaheadHoursperStep)
        Dim nFirstPeriod As Integer = CurrentEnergyModel.Steps.FirstPeriod
        Dim nLastPeriod As Integer = CurrentEnergyModel.Steps.StepPeriodCount
        Dim nBeginInterval As Integer = CurrentEnergyModel.Steps.FirstIntervalInPeriod(nFirstPeriod)
        Dim nLastInterval As Integer = CurrentEnergyModel.Steps.LastIntervalInPeriod(nLastPeriod)
        Dim nTotalPeriods As Integer = CurrentEnergyModel.Steps.StepPeriodCount
        Dim dLajaNov30Vol As Double 'Lajas volume Nov 30th in 1000m3

        'Initialize volumes on Nov 30th (this is done just in the first step):
        If CurrentEnergyModel.Steps.IsFirstStep Then
            m_vol30NovPerSample.Volume(Multivariate.CurrentSample) = varVol30Nov.SampleValue(CurrentEnergyModel.Steps.FirstPeriod, PeriodEnum.Interval) * CMD2M3 'We store as 1000m3
        End If

        For nCurPeriod As Integer = 1 To CurrentEnergyModel.Steps.StepPeriodCount

            If (nCurPeriod <= CurrentEnergyModel.Steps.StepPeriodCountSansLookahead) Then
                'dateCur = EEUTILITY.Functions.Period2Date(CurrentModel.Steps.PeriodTypeFrom(PeriodEnum.Interval) + nCurPeriod - 1, CurrentModel.Horizon.PeriodsperDay, nHoursPerInterval, dateBeginStep, nDayBeginning)
                dateCur = EnergyExemplar.PLEXOS.Utility.Functions.Period2Date(nCurPeriod, CurrentEnergyModel.Horizon.PeriodsperDay, nHoursPerInterval, dateBeginStep, nDayBeginning)
            Else
                curLookAheadPeriod = nCurPeriod - CurrentEnergyModel.Steps.StepPeriodCountSansLookahead
                dateCur = EnergyExemplar.PLEXOS.Utility.Functions.Period2Date(curLookAheadPeriod, nPeriodsPerDayInLookAhead, nHoursPerIntervalInLookAhead, dateBeginLookAhead, nDayBeginning)
            End If

            If Utils.IsLajaInitDate(dateCur) Then

                m_stepPeriod30Nov = nCurPeriod
                dLajaNov30Vol = m_vol30NovPerSample.Volume(Multivariate.CurrentSample)

                'Dim inflowGen(nTotalPeriods) As Double
                'Dim inflowRie(nTotalPeriods) As Double
                'Dim inflowMix(nTotalPeriods) As Double

                Dim inflowGen(nPeriodsInHorizon) As Double
                Dim inflowRie(nPeriodsInHorizon) As Double
                Dim inflowMix(nPeriodsInHorizon) As Double

                Dim Flow2Vol As Double = 3.6 * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod)

                If (dLajaNov30Vol > 1900000.0) Then 'Colchon Superior

                    inflowGen(m_horizonPeriod30Nov) = Math.Max(Math.Min((280500 + 0.65 * (dLajaNov30Vol - 1900000)) / Flow2Vol, Utils.LAJA_MAX_EXT_GEN), 0.0)
                    inflowRie(m_horizonPeriod30Nov) = Math.Max(880000 + 0.25 * (dLajaNov30Vol - 1900000) / Flow2Vol, 0.0)
                    inflowMix(m_horizonPeriod30Nov) = 0

                ElseIf (1370000 < dLajaNov30Vol AndAlso dLajaNov30Vol <= 1900000.0) Then 'Colchon Intermedio

                    inflowGen(m_horizonPeriod30Nov) = Math.Max(Math.Min((68500 + 0.04 * (dLajaNov30Vol - 1370000)) / Flow2Vol, Utils.LAJA_MAX_EXT_GEN), 0.0)
                    inflowRie(m_horizonPeriod30Nov) = Math.Max((668000 + 0.4 * (dLajaNov30Vol - 1370000)) / Flow2Vol, 0.0)
                    inflowMix(m_horizonPeriod30Nov) = 0

                ElseIf (1200000.0 < dLajaNov30Vol AndAlso dLajaNov30Vol <= 1370000) Then 'Colchon Transicion

                    inflowGen(m_horizonPeriod30Nov) = Math.Max(Math.Min((60000 + 0.05 * (dLajaNov30Vol - 1200000)) / Flow2Vol, Utils.LAJA_MAX_EXT_GEN), 0.0)
                    inflowRie(m_horizonPeriod30Nov) = Math.Max((600000 + 0.4 * (dLajaNov30Vol - 1200000)) / Flow2Vol, 0.0)
                    inflowMix(m_horizonPeriod30Nov) = 0

                ElseIf (dLajaNov30Vol <= 1200000.0) Then 'Colchon Inferior
                    Dim dVolLajaMixto As Double = 0.2 * dLajaNov30Vol
                    inflowGen(m_horizonPeriod30Nov) = Math.Max(Math.Min((0.05 * dLajaNov30Vol + 30000 - dVolLajaMixto) / Flow2Vol, Utils.LAJA_MAX_EXT_GEN), 0.0)
                    inflowRie(m_horizonPeriod30Nov) = Math.Max((570000 - dVolLajaMixto) / Flow2Vol, 0.0)
                    inflowMix(m_horizonPeriod30Nov) = dVolLajaMixto / Flow2Vol
                End If

#If USE_ARCHIVE_FOR_LAJA_INFLOW Then
                    Dim bHasNaturalInflows As Boolean = stoLaja_Gen.HasNaturalInflows And stoLaja_Riego.HasNaturalInflows And stoLaja_Mixto.HasNaturalInflows
                    If Not bHasNaturalInflows Then
                        CurrentModel.Feedback.LogMessage("Virtual Inflows without 'zero' inflows. Pre-calculation will exit")
                        m_isValidIrr = False
                        Return
                    End If
                    stoLaja_Gen.PutData(SystemStoragesEnum.NaturalInflow, inflowGen, PeriodEnum.Interval)
                    stoLaja_Riego.PutData(SystemStoragesEnum.NaturalInflow, inflowRie, PeriodEnum.Interval)
                    stoLaja_Mixto.PutData(SystemStoragesEnum.NaturalInflow, inflowMix, PeriodEnum.Interval)
#End If

                'Now we rebuild Vmax:
                Dim lg As New List(Of Double)
                Dim lr As New List(Of Double)
                Dim lm As New List(Of Double)
                For p As Integer = 1 To nPeriodsInHorizon
                    'For Each i As Integer In CurrentModel.Horizon.PeriodTypeIntervalTo(PeriodEnum.Interval)
                    If p < m_horizonPeriod30Nov Then
                        lg.Add(stoLaja_Gen.InitialVolume(nFirstPeriod) / Utils.CMD2M3) 'we need to convert to CMD
                        lr.Add(stoLaja_Riego.InitialVolume(nFirstPeriod) / Utils.CMD2M3)
                        lm.Add(stoLaja_Mixto.InitialVolume(nFirstPeriod) / Utils.CMD2M3)
                    Else
                        lg.Add(inflowGen(m_horizonPeriod30Nov) * Flow2Vol / Utils.CMD2M3) 'we need to convert to CMD
                        lr.Add(inflowRie(m_horizonPeriod30Nov) * Flow2Vol / Utils.CMD2M3) 'we need to convert to CMD
                        lm.Add(inflowMix(m_horizonPeriod30Nov) * Flow2Vol / Utils.CMD2M3) 'we need to convert to CMD
                    End If
                    'Next
                Next

#If USE_ARCHIVE_FOR_LAJA_MAXVOLUME Then
                stoLaja_Gen.PutData(SystemStoragesEnum.MaxVolume, lg.ToArray(), PeriodEnum.Interval)
                stoLaja_Riego.PutData(SystemStoragesEnum.MaxVolume, lr.ToArray(), PeriodEnum.Interval)
                stoLaja_Mixto.PutData(SystemStoragesEnum.MaxVolume, lm.ToArray(), PeriodEnum.Interval)
                stoLaja_Gen.MarkInputDirty(False)
                stoLaja_Riego.MarkInputDirty(False)
                stoLaja_Mixto.MarkInputDirty(False)
#End If

            End If

            If Utils.IsMauleInitDate(dateCur) Then
                m_periodInitMaule = nCurPeriod
            End If

#If USE_ARCHIVE_FOR_MAULE Then
            Dim dInitialVolumeLMAULE As Double
            If Utils.IsMauleInitDate(dateCur) Then
                m_periodInitMaule = nCurPeriod
                dScalar = stoLMaule.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod)
                dInitialVolumeLMAULE = stoLMaule.InitialVolume(nFirstPeriod)
                Dim inflowSupGen(nTotalPeriods) As Double
                Dim inflowSupRie(nTotalPeriods) As Double
                Dim inflowIntGen(nTotalPeriods) As Double
                Dim inflowIntRie(nTotalPeriods) As Double

                'Artificial inflows to fill up the superior porcion virtual storages:
                inflowSupGen(1) = 250000 / dScalar
                inflowSupRie(1) = 800000 / dScalar
                inflowIntGen(1) = 0.2 * (Math.Min(dInitialVolumeLMAULE, LMAULE_INTER_VOL) - 170000) / dScalar
                inflowIntRie(1) = 0.8 * (Math.Min(dInitialVolumeLMAULE, LMAULE_INTER_VOL) - 170000) / dScalar

                stoMauleSup_Eco_Gen.PutData(SystemStoragesEnum.NaturalInflow, inflowSupGen, PeriodEnum.Interval, 1, nCurPeriod, nCurPeriod)
                stoMauleSup_Eco_Riego.PutData(SystemStoragesEnum.NaturalInflow, inflowSupRie, PeriodEnum.Interval, 1, nCurPeriod, nCurPeriod)
                stoMauleLow_Eco_Gen.PutData(SystemStoragesEnum.NaturalInflow, inflowIntGen, PeriodEnum.Interval, 1, nCurPeriod, nCurPeriod)
                stoMauleLow_Eco_Riego.PutData(SystemStoragesEnum.NaturalInflow, inflowIntRie, PeriodEnum.Interval, 1, nCurPeriod, nCurPeriod)
                stoMauleSup_Eco_Gen.MarkInputDirty(False)
                stoMauleSup_Eco_Riego.MarkInputDirty(False)
                stoMauleLow_Eco_Gen.MarkInputDirty(False)
                stoMauleLow_Eco_Riego.MarkInputDirty(False)

                'Max Volume all virtual storages:
                Dim lsupg As New List(Of Double)
                Dim lsupr As New List(Of Double)
                Dim lintg As New List(Of Double)
                Dim lintr As New List(Of Double)
                Dim linv As New List(Of Double)
                Dim dateP As Date
                For p As Integer = 1 To CurrentModel.Horizon.PeriodCount(PeriodEnum.Interval)
                    dateP = EEUTILITY.Functions.Period2Date(p, CurrentModel.Horizon.PeriodsperDay, nHoursPerInterval, dateBeginHorizon, nDayBeginning)
                    lsupg.Add(350000 / Utils.CMD2M3) 'we need to convert to CMD
                    lsupr.Add(800000 / Utils.CMD2M3) 'we need to convert to CMD
                    If dateP.CompareTo(dateCur) = 0 Then
                        lintg.Add(inflowIntGen(1) * dScalar / Utils.CMD2M3) 'we need to convert to CMD
                        lintr.Add(inflowIntRie(1) * dScalar / Utils.CMD2M3) 'we need to convert to CMD
                        linv.Add(0.0) 'Reset invernada economies
                    Else
                        lintg.Add(stoMauleLow_Eco_Gen.MaxVolume(nFirstPeriod) / Utils.CMD2M3) 'we need to convert to CMD
                        lintr.Add(stoMauleLow_Eco_Riego.MaxVolume(nFirstPeriod) / Utils.CMD2M3) 'we need to convert to CMD
                        linv.Add(stoInvernada_Eco.MaxVolume(nFirstPeriod) / Utils.CMD2M3) 'we need to convert to CMD
                    End If
                Next
                stoMauleSup_Eco_Gen.PutData(SystemStoragesEnum.MaxVolume, lsupg.ToArray(), PeriodEnum.Interval)
                stoMauleSup_Eco_Riego.PutData(SystemStoragesEnum.MaxVolume, lsupr.ToArray(), PeriodEnum.Interval)
                stoMauleLow_Eco_Gen.PutData(SystemStoragesEnum.MaxVolume, lintg.ToArray(), PeriodEnum.Interval)
                stoMauleLow_Eco_Riego.PutData(SystemStoragesEnum.MaxVolume, lintr.ToArray(), PeriodEnum.Interval)
                stoInvernada_Eco.PutData(SystemStoragesEnum.MaxVolume, linv.ToArray(), PeriodEnum.Interval)

                stoMauleSup_Eco_Gen.MarkInputDirty(False)
                stoMauleSup_Eco_Riego.MarkInputDirty(False)
                stoMauleLow_Eco_Gen.MarkInputDirty(False)
                stoMauleLow_Eco_Riego.MarkInputDirty(False)
                stoInvernada_Eco.MarkInputDirty(False)
            End If
#End If

        Next

    End Sub

    Public Sub AfterProperties() Implements IOpenModel.AfterProperties
        If Not m_isValidIrr Then
            Return
        End If

        Dim dInitialVolumeELTORO As Double
        Dim dInitialVolumeLaja_Gen As Double
        Dim dInitialVolumeLaja_Riego As Double
        Dim dInitialVolumeLaja_Mixto As Double
        Dim dInitialVolumeLMAULE As Double
        Dim dInitialVolumeCIPRESES As Double
        Dim dInitialVolumeCOLBUN As Double
        Dim dInitialVolumeRALCO As Double
        Dim dInitialVolumenMauleLow_Riego As Double
        Dim dInitialVolumenMauleSup_Riego As Double

        Dim nInitPeriod As Double = CurrentEnergyModel.Steps.FirstPeriod
        Dim nTotalPeriods As Double = CurrentEnergyModel.Steps.StepPeriodCount
        Dim nTotalHours As Double = CurrentEnergyModel.Steps.HoursInStep

        'Initial Step (Volumes):
        For Each s In StoragesIN
            If s.Name.Equals("ELTORO", StringComparison.OrdinalIgnoreCase) Then
                dInitialVolumeELTORO = s.CurrentStorageStepBridge.PeriodZeroVolume
            ElseIf s.Name.Equals("LMAULE", StringComparison.OrdinalIgnoreCase) Then
                dInitialVolumeLMAULE = s.CurrentStorageStepBridge.PeriodZeroVolume
            ElseIf s.Name.Equals("CIPRESES", StringComparison.OrdinalIgnoreCase) Then
                dInitialVolumeCIPRESES = s.CurrentStorageStepBridge.PeriodZeroVolume
            ElseIf s.Name.Equals("COLBUN", StringComparison.OrdinalIgnoreCase) Then
                dInitialVolumeCOLBUN = s.CurrentStorageStepBridge.PeriodZeroVolume
            ElseIf s.Name.Equals("RALCO", StringComparison.OrdinalIgnoreCase) Then
                dInitialVolumeRALCO = s.CurrentStorageStepBridge.PeriodZeroVolume
            End If
        Next

        dInitialVolumenMauleLow_Riego = stoMauleLow_Eco_Riego.CurrentStorageStepBridge.PeriodZeroVolume
        dInitialVolumenMauleSup_Riego = stoMauleSup_Eco_Riego.CurrentStorageStepBridge.PeriodZeroVolume
        dInitialVolumeLaja_Gen = stoLaja_Gen.CurrentStorageStepBridge.PeriodZeroVolume
        dInitialVolumeLaja_Mixto = stoLaja_Mixto.CurrentStorageStepBridge.PeriodZeroVolume
        dInitialVolumeLaja_Riego = stoLaja_Riego.CurrentStorageStepBridge.PeriodZeroVolume

        Dim dRiego(nTotalPeriods) As Double
        Dim dHoyaInt(nTotalPeriods) As Double
        Dim dHoyaIntLaja(nTotalPeriods) As Double
        Dim varQInvFree As Variable

        For Each v As Variable In ModelGlobals.VariablesIN.InServiceObjects
            'Total Irrigation:
            If Utils.isRiegoVariable(v) Then
                For nCurPeriod As Integer = 1 To CurrentEnergyModel.Steps.StepPeriodCount
                    dRiego(nCurPeriod) += v(SystemVariablesEnum.Profile, nCurPeriod)
                Next
            End If
            'HoyaIntermedia:
            If Utils.IsHoyaIntermedia(v) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    dHoyaInt(nCurPeriod) += v(SystemVariablesEnum.Profile, nCurPeriod)
                Next
            End If
            'Cipreses min inflow for free operation:
            If v.Name = "QInvFree" Then
                varQInvFree = v
            End If
            If Utils.IsHoyaIntermediaLaja(v) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    dHoyaIntLaja(nCurPeriod) += v(SystemVariablesEnum.Profile, nCurPeriod)
                Next
            End If
        Next

        Dim varInflowInvEco As DecisionVariable
        Dim varInflowMauleIEcoGen As DecisionVariable
        Dim varInflowMauleIEcoRiego As DecisionVariable
        Dim varInflowMauleSEcoGen As DecisionVariable
        Dim varInflowMauleSEcoRiego As DecisionVariable
        For Each v As DecisionVariable In ModelGlobals.DecisionVariablesIN.InServiceObjects
            If v.Name.Equals("Inflow_Invernada_Eco", StringComparison.OrdinalIgnoreCase) Then
                varInflowInvEco = v
            ElseIf v.Name.Equals("Inflow_MauleLow_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then
                varInflowMauleIEcoGen = v
            ElseIf v.Name.Equals("Inflow_MauleLow_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then
                varInflowMauleIEcoRiego = v
            ElseIf v.Name.Equals("Inflow_MauleSup_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then
                varInflowMauleSEcoGen = v
            ElseIf v.Name.Equals("Inflow_MauleSup_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then
                varInflowMauleSEcoRiego = v
            End If
        Next

        If IsNothing(varInflowInvEco) OrElse IsNothing(varInflowMauleIEcoGen) OrElse IsNothing(varInflowMauleIEcoRiego) _
            OrElse IsNothing(varInflowMauleSEcoGen) OrElse IsNothing(varInflowMauleSEcoRiego) Then
            CurrentEnergyModel.Feedback.LogMessage("Couldn't find all required decision variables for Maule...") 'TODO: Move this to validation
            Return
        End If

        'Total Deficit:
        Dim dDeficit(nTotalPeriods) As Double
        For nCurPeriod As Integer = 1 To CurrentEnergyModel.Steps.StepPeriodCount
            dDeficit(nCurPeriod) = dRiego(nCurPeriod) - dHoyaInt(nCurPeriod) ' stoLMaule(SystemStoragesEnum.NaturalInflow, nCurPeriod)
        Next

        'Filtration constraints:
        For Each c As Constraint In ModelGlobals.ConstraintsIN.InServiceObjects
            If (Utils.IsFiltration(c)) Then
                Dim s As Storage
                Dim dFiltRHS As Double
                Dim dFiltCoeff As Double
                s = Utils.GetStorage(c)
                dFiltCoeff = Utils.GetFiltrationLinearCoefficient(c.Name, s.InitialVolume(nInitPeriod))
                dFiltRHS = Utils.GetFiltrationLinearRHS(c.Name, s.InitialVolume(nInitPeriod))
                For nCurPeriod As Integer = 1 To CurrentEnergyModel.Steps.StepPeriodCount
                    s.EndVolumeCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, dFiltCoeff)
                    c.ConstraintRow.RHS(nCurPeriod) = dFiltRHS
                Next
            End If
        Next

        'Adjust FCF constraints to the end of the lookahead:
        For Each c In ModelGlobals.ConstraintsIN.InServiceObjects
            If c.Name.StartsWith("FCF_", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To CurrentEnergyModel.Steps.PeriodTypeCount(c.PeriodType) - 1
                    c.ConstraintRow.RHS(nCurPeriod) = Utils.FCF_MIN_VALUE
                Next
            End If
        Next

        'Maule Irrigation Constraints:
        'CurrentEnergyModel.MathProgram.EnterSafeAijMode()
        Dim dScalar As Double
        For Each c As Constraint In ModelGlobals.ConstraintsIN.InServiceObjects

            If c.Name.Equals("ExtractDef_Maule", StringComparison.OrdinalIgnoreCase) Then
                'c.Initialize(0)
                For nCurPeriod As Integer = 1 To CurrentEnergyModel.Steps.StepPeriodCount
                    dScalar = stoLMaule.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod) 'This is to convert to flow
                    'stoLMaule.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod) = 100.0 / dScalar
                    If Utils.IsSuperiorPorcionMaule(dInitialVolumeLMAULE) Then
                        stoMauleSup_Eco_Gen.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, -100.0 / dScalar)
                        stoMauleSup_Eco_Riego.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, -100.0 / dScalar)
                        stoInvernada_Eco.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, -100.0 / dScalar)
                        stoMauleLow_Eco_Gen.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, 0.0)
                        stoMauleLow_Eco_Riego.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, 0.0)
                    Else
                        stoMauleSup_Eco_Gen.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, 0.0)
                        stoMauleSup_Eco_Riego.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, 0.0)
                        stoInvernada_Eco.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, 0.0)
                        stoMauleLow_Eco_Gen.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, -100.0 / dScalar)
                        stoMauleLow_Eco_Riego.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, -100.0 / dScalar)
                    End If
                Next

            ElseIf c.Name.Equals("Maule_Irrigation_Deficit", StringComparison.OrdinalIgnoreCase) Then

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If dDeficit(nCurPeriod) > 0 Then
                        c.ConstraintRow(nCurPeriod).RHS(nCurPeriod) = dDeficit(nCurPeriod) 'OJO!!!
                    End If
                Next

            ElseIf c.Name.Equals("Cipreses_IfDeficitMaule", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    dScalar = stoCipreses.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod) 'This is to convert N.I. into volume
                    varInflowInvEco.Coefficient(stoInvernada_Eco.BalanceConstraint(nCurPeriod), nCurPeriod, 1.0) 'This goes for sure
                    If dDeficit(nCurPeriod) > 0 AndAlso Utils.IsSuperiorPorcionMaule(dInitialVolumeLMAULE) Then 'Only in the superior portion Inv can accumulate economies
                        stoCipreses.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, -1.0)
                        c.ConstraintRow(nCurPeriod).RHS(nCurPeriod) = -stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) * dScalar
                    Else
                        stoCipreses.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, 0.0)
                    End If
                Next

            ElseIf c.Name.Equals("Cipreses_IfDeficitMaule_NoDeficitInv_2", StringComparison.OrdinalIgnoreCase) Then

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If dDeficit(nCurPeriod) > 0 AndAlso stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) > dDeficit(nCurPeriod) Then
                        c.ConstraintRow(nCurPeriod).RHS(nCurPeriod) = dDeficit(nCurPeriod)
                    End If
                Next

            ElseIf c.Name.Equals("Cipreses_IfDeficitMaule_IfDeficitInv_1", StringComparison.OrdinalIgnoreCase) Then

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If dDeficit(nCurPeriod) > 0 AndAlso stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) <= dDeficit(nCurPeriod) Then
                        c.ConstraintRow(nCurPeriod).RHS(nCurPeriod) = dDeficit(nCurPeriod)
                    End If
                Next

            ElseIf c.Name.Equals("DeficitMaulexEcoRiego", StringComparison.OrdinalIgnoreCase) Then

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    dScalar = stoLMaule.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod)
                    If dDeficit(nCurPeriod) > 0 AndAlso stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) <= dDeficit(nCurPeriod) Then
                        stoLMaule.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, SafeDivision(-1.0, dScalar))
                        If (Utils.IsLowePorcionMaule(dInitialVolumeLMAULE)) Then
                            stoInvernada_Eco.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, SafeDivision(-1.0, dScalar))
                        Else
                            stoInvernada_Eco.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, 0.0)
                        End If
                    Else
                        stoLMaule.ReleaseCoefficient(c.ConstraintRow(nCurPeriod), nCurPeriod, 0.0)
                    End If
                Next

            ElseIf c.Name.Equals("Inv_CotaFija_ub", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) + dHoyaInt(nCurPeriod) < varQInvFree(SystemVariablesEnum.Profile, nCurPeriod) Then
                        'dScalar = stoCipreses.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod) 'This is to convert to flow
                        c.ConstraintRow(nCurPeriod).RHS(nCurPeriod) = stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod)
                    End If
                Next

            ElseIf c.Name.Equals("Inv_CotaFija_lb", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod) + dHoyaInt(nCurPeriod) < varQInvFree(SystemVariablesEnum.Profile, nCurPeriod) Then
                        'dScalar = stoCipreses.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentModel.Steps.HoursinPeriod(nCurPeriod) 'This is to convert to flow
                        c.ConstraintRow(nCurPeriod).RHS(nCurPeriod) = stoCipreses(SystemStoragesEnum.NaturalInflow, nCurPeriod)
                    End If
                Next

            ElseIf c.Name.Equals("HoyaIntermedia", StringComparison.OrdinalIgnoreCase) Then

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    c.ConstraintRow(nCurPeriod).RHS(nCurPeriod) = dHoyaInt(nCurPeriod)
                Next

            ElseIf c.Name.Equals("Maule_Irrigation_Sup", StringComparison.OrdinalIgnoreCase) OrElse c.Name.Equals("Maule_Generation_Sup", StringComparison.OrdinalIgnoreCase) Then
                If Not (Utils.IsSuperiorPorcionMaule(dInitialVolumeLMAULE)) Then
                    For nCurPeriod As Integer = 1 To nTotalPeriods
                        c.ConstraintRow.RHS(nCurPeriod) = 0.0
                    Next
                End If

            ElseIf c.Name.Equals("Maule_Irrigation_Low", StringComparison.OrdinalIgnoreCase) OrElse c.Name.Equals("Maule_Generation_Low", StringComparison.OrdinalIgnoreCase) Then
                If Utils.IsSuperiorPorcionMaule(dInitialVolumeLMAULE) Then
                    For nCurPeriod As Integer = 1 To nTotalPeriods
                        c.ConstraintRow.RHS(nCurPeriod) = 0.0
                    Next
                End If

            ElseIf c.Name.Equals("InflowDef_Invernada_Eco", StringComparison.OrdinalIgnoreCase) Then
                If Utils.IsSuperiorPorcionMaule(dInitialVolumeLMAULE) Then
                    For nCurPeriod As Integer = 1 To nTotalPeriods
                        c.ConstraintRow.RHS(nCurPeriod) = Utils.FREE_FLOW
                    Next
                End If

            ElseIf c.Name.Equals("InflowDef_MauleLow_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    Dim dInflow As Double = 0.0
                    dScalar = stoMauleLow_Eco_Gen.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod)
                    varInflowMauleIEcoGen.Coefficient(stoMauleLow_Eco_Gen.BalanceConstraint(nCurPeriod), nCurPeriod, 1.0)
                    If Utils.IsLowePorcionMaule(dInitialVolumeLMAULE) Then
                        dInflow += 0.2 * stoLMaule(SystemStoragesEnum.NaturalInflow, nCurPeriod) * dScalar
                    End If
                    If nCurPeriod = m_periodInitMaule Then 'If it is initialize period we also need to add the 20% of the reserve
                        If Utils.IsLowePorcionMaule(dInitialVolumeLMAULE) Then
                            dInflow += 0.2 * Math.Max(dInitialVolumeLMAULE - Utils.LMAULE_EXTRAO_VOL, 0.0) 'Check negative should be temporarily
                        Else
                            dInflow += 0.2 * Utils.GetLowerPorcionVolume()
                        End If
                    End If
                    c.ConstraintRow.RHS(nCurPeriod) = dInflow
                Next

            ElseIf c.Name.Equals("InflowDef_MauleLow_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    Dim dInflow As Double = 0.0
                    dScalar = stoMauleLow_Eco_Riego.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod)
                    varInflowMauleIEcoRiego.Coefficient(stoMauleLow_Eco_Riego.BalanceConstraint(nCurPeriod), nCurPeriod, 1.0)
                    If Utils.IsLowePorcionMaule(dInitialVolumeLMAULE) Then
                        dInflow += 0.8 * stoLMaule(SystemStoragesEnum.NaturalInflow, nCurPeriod) * dScalar
                    End If
                    If nCurPeriod = m_periodInitMaule Then 'If it is initialize period we also need to add the 80% of the reserve
                        If Utils.IsLowePorcionMaule(dInitialVolumeLMAULE) Then
                            dInflow += 0.8 * Math.Max(dInitialVolumeLMAULE - Utils.LMAULE_EXTRAO_VOL, 0.0) 'Check negative should be temporarily
                        Else
                            dInflow += 0.8 * Utils.GetLowerPorcionVolume()
                        End If
                    End If
                    c.ConstraintRow.RHS(nCurPeriod) = dInflow
                Next

            ElseIf c.Name.Equals("InflowDef_MauleSup_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then

                If m_periodInitMaule > 0 Then
                    dScalar = stoLMaule.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentEnergyModel.Steps.HoursinPeriod(m_periodInitMaule)
                    varInflowMauleSEcoGen.Coefficient(stoMauleSup_Eco_Gen.BalanceConstraint(m_periodInitMaule), m_periodInitMaule, 1.0)
                    c.ConstraintRow.RHS(m_periodInitMaule) = stoMauleSup_Eco_Gen.MaxVolume(nInitPeriod) 'Previously 250000
                End If

            ElseIf c.Name.Equals("InflowDef_MauleSup_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then

                If m_periodInitMaule > 0 Then
                    dScalar = stoLMaule.Multiplier(SystemStoragesEnum.MaxRelease) * CurrentEnergyModel.Steps.HoursinPeriod(m_periodInitMaule)
                    varInflowMauleSEcoRiego.Coefficient(stoMauleSup_Eco_Riego.BalanceConstraint(m_periodInitMaule), m_periodInitMaule, 1.0)
                    c.ConstraintRow.RHS(m_periodInitMaule) = stoMauleSup_Eco_Riego.MaxVolume(nInitPeriod) ' Previously 800000
                End If

            ElseIf c.Name.Equals("ShortageDef_CuencaMaule", StringComparison.OrdinalIgnoreCase) Then

                Dim dQMauleIrrReqStep As Double = 0.0 'Average flow requirement in step
                Dim dQMauleIrrMaxStep As Double 'Max average irrigation flow rights in step

                For nCurPeriod As Integer = 1 To nTotalPeriods
                    dQMauleIrrReqStep += varRiegoTotalMaule(SystemVariablesEnum.Profile, nCurPeriod) * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod)
                Next
                dQMauleIrrReqStep = SafeDivision(dQMauleIrrReqStep, nTotalHours)

                If Utils.IsLowePorcionMaule(dInitialVolumeLMAULE) Then
                    dQMauleIrrMaxStep = dInitialVolumenMauleLow_Riego * stoLMaule.VolumeScalar
                    For nCurPeriod As Integer = 1 To nTotalPeriods
                        dQMauleIrrMaxStep += 0.8 * stoLMaule(SystemStoragesEnum.NaturalInflow, nCurPeriod) * SecondsInHour * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod)
                    Next
                Else
                    dQMauleIrrMaxStep = dInitialVolumenMauleSup_Riego * stoLMaule.VolumeScalar
                End If
                dQMauleIrrMaxStep = SafeDivision(dQMauleIrrMaxStep, nTotalHours * SecondsInHour)

                Dim dShortageStepMaule = dQMauleIrrReqStep - dQMauleIrrMaxStep
                If (dShortageStepMaule > 0) Then

                    For nCurPeriod As Integer = 1 To nTotalPeriods
                        Dim dShortageMaule = Math.Min(dShortageStepMaule, varRiegoTotalMaule(SystemVariablesEnum.Profile, nCurPeriod))
                        c.ConstraintRow.RHS(nCurPeriod) = dShortageMaule
                    Next

                End If

            End If

        Next
        'CurrentEnergyModel.MathProgram.ExitSafeAijMode()

        'Laja Irrigation constraints:
        Dim dQMaxRiegoStep As Double 'Max average irrigation flow rights in step
        Dim dQLajaIrr1ReqStep As Double = 0.0 'Average flow requirement for 1st irrigators in step
        Dim dQLajaIrr1MaxStep As Double 'Maximum average flow possible for 1st irrigators in step
        Dim dQLajaIrr2ReqStep As Double = 0.0
        Dim dQLajaIrr2MaxStep As Double
        Dim dQLajasaltosReqStep As Double = 0.0
        Dim dQLajasaltosMaxStep As Double
        Dim dFiltLajaMedio As Double = Utils.GetFiltration("ELTORO", dInitialVolumeELTORO)
        Dim dHoyaIntLajaMedio As Double = dHoyaIntLaja.Average()


        dQMaxRiegoStep = SafeDivision(dInitialVolumeLaja_Mixto * stoLaja_Mixto.VolumeScalar + dInitialVolumeLaja_Riego * stoLaja_Riego.VolumeScalar, (nTotalHours * SecondsInHour))

        For nCurPeriod As Integer = 1 To nTotalPeriods
            dQLajaIrr1ReqStep += varLAJAirrigators1(SystemVariablesEnum.Profile, nCurPeriod) * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod)
        Next
        dQLajaIrr1ReqStep = SafeDivision(dQLajaIrr1ReqStep, nTotalHours)
        dQLajaIrr1MaxStep = Math.Min(Math.Max(dQMaxRiegoStep + dFiltLajaMedio + dHoyaIntLajaMedio, 0.0), dQLajaIrr1ReqStep)

        For nCurPeriod As Integer = 1 To nTotalPeriods
            dQLajaIrr2ReqStep += varLAJAirrigators2(SystemVariablesEnum.Profile, nCurPeriod) * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod)
        Next
        dQLajaIrr2ReqStep = SafeDivision(dQLajaIrr2ReqStep, nTotalHours)
        dQLajaIrr2MaxStep = Math.Min(Math.Max(dQMaxRiegoStep + dFiltLajaMedio + dHoyaIntLajaMedio - dQLajaIrr1MaxStep, 0.0), dQLajaIrr2ReqStep)

        For nCurPeriod As Integer = 1 To nTotalPeriods
            dQLajasaltosReqStep += varLAJAsaltos(SystemVariablesEnum.Profile, nCurPeriod) * CurrentEnergyModel.Steps.HoursinPeriod(nCurPeriod)
        Next
        dQLajasaltosReqStep = SafeDivision(dQLajasaltosReqStep, nTotalHours)
        dQLajasaltosMaxStep = Math.Min(Math.Max(dQMaxRiegoStep + dFiltLajaMedio + dHoyaIntLajaMedio - dQLajaIrr1MaxStep - dQLajaIrr2MaxStep, 0.0), dQLajasaltosReqStep)

        Dim bUseMovAvg As Boolean = True

        For Each c As Constraint In ModelGlobals.ConstraintsIN.InServiceObjects

            If c.Name.Equals("Qmin_Saltos", StringComparison.OrdinalIgnoreCase) Then
                If Not c.IsDefinedNonZero(SystemConstraintsEnum.RHS) Then
                    For nCurPeriod As Integer = 1 To nTotalPeriods
                        c.ConstraintRow.RHS(nCurPeriod) = 1.0 * dQLajasaltosMaxStep + 0.0 * dQLajaIrr1MaxStep + 0.0 * dQLajaIrr2MaxStep 'TODO: Move coefficients to Variables
                    Next
                End If

            ElseIf c.Name.Equals("Qmin_Tucapel", StringComparison.OrdinalIgnoreCase) Then
                If Not c.IsDefinedNonZero(SystemConstraintsEnum.RHS) Then
                    For nCurPeriod As Integer = 1 To nTotalPeriods
                        c.ConstraintRow.RHS(nCurPeriod) = 0.0 * dQLajasaltosMaxStep + 0.628 * dQLajaIrr1MaxStep + 1.0 * dQLajaIrr2MaxStep
                    Next
                End If

            ElseIf c.Name.Equals("Qmin_Zanartu_Collao", StringComparison.OrdinalIgnoreCase) Then
                If Not c.IsDefinedNonZero(SystemConstraintsEnum.RHS) Then
                    For nCurPeriod As Integer = 1 To nTotalPeriods
                        c.ConstraintRow.RHS(nCurPeriod) = 0.0 * dQLajasaltosMaxStep + 0.372 * dQLajaIrr1MaxStep + 0.0 * dQLajaIrr2MaxStep
                    Next
                End If

            End If

        Next

        'Ralco constraints:
        Dim dMaxVolume As Double
        For Each c In ModelGlobals.ConstraintsIN.InServiceObjects
            dMaxVolume = stoRalco(SystemStoragesEnum.MaxVolume, nInitPeriod) * CMD2M3
            If c.Name.Equals("Extraction_Ralco_Sup", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To CurrentEnergyModel.Steps.PeriodTypeCount(c.PeriodType) - 1
                    If Utils.IsSuperiorPorcionRalco(dInitialVolumeRALCO) Then
                        c.ConstraintRow.RHS(nCurPeriod) = 3337 + 0.00808552 * dInitialVolumeRALCO
                    Else
                        c.ConstraintRow.RHS(nCurPeriod) = Utils.FREE_RALCO
                    End If
                Next
            ElseIf c.Name.Equals("Extraction_Ralco_Inf", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To CurrentEnergyModel.Steps.PeriodTypeCount(c.PeriodType) - 1
                    If Utils.IsLowerPorcionRalco(dInitialVolumeRALCO) Then
                        c.ConstraintRow.RHS(nCurPeriod) = 1375 + 0.00602351 * dInitialVolumeRALCO
                    Else
                        c.ConstraintRow.RHS(nCurPeriod) = Utils.FREE_RALCO
                    End If
                Next
            End If
        Next

        'Reserve volume constraints:
        For Each c In ModelGlobals.ConstraintsIN.InServiceObjects
            If c.Name.Equals("ReserveMinVol_Ralco", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If (dInitialVolumeRALCO <= varReserveMinVolRalco(SystemVariablesEnum.Profile, nInitPeriod) * CMD2M3) Then
                        c.ConstraintRow.RHS(nCurPeriod) = 0.0
                    Else
                        c.ConstraintRow.RHS(nCurPeriod) = Utils.FREE_FLOW
                    End If
                Next
            ElseIf c.Name.Equals("ReserveMinVol_ElToro", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If (dInitialVolumeELTORO <= varReserveMinVolElToro(SystemVariablesEnum.Profile, nInitPeriod) * CMD2M3) Then
                        c.ConstraintRow.RHS(nCurPeriod) = 0.0
                    Else
                        c.ConstraintRow.RHS(nCurPeriod) = Utils.FREE_FLOW
                    End If
                Next
            ElseIf c.Name.Equals("ReserveMinVol_Colbun", StringComparison.OrdinalIgnoreCase) Then
                For nCurPeriod As Integer = 1 To nTotalPeriods
                    If (dInitialVolumeCOLBUN <= varReserveMinVolColbun(SystemVariablesEnum.Profile, nInitPeriod) * CMD2M3) Then
                        c.ConstraintRow.RHS(nCurPeriod) = 0.0
                    Else
                        c.ConstraintRow.RHS(nCurPeriod) = Utils.FREE_FLOW
                    End If
                Next
            End If
        Next

    End Sub

    Public Sub BeforeOptimize() Implements IOpenModel.BeforeOptimize
    End Sub

    Public Sub AfterOptimize() Implements IOpenModel.AfterOptimize
    End Sub

    Public Sub BeforeRecordSolution() Implements IOpenModel.BeforeRecordSolution
        'We register the new 30 Nov Volume
        For Each s In StoragesIN
            If s.Name.Equals("ELTORO", StringComparison.OrdinalIgnoreCase) Then
                If (m_stepPeriod30Nov > 0) Then
                    'Dim d(CurrentModel.Steps.StepPeriodCount) As Double
                    'm_vol30Nov = s.EndVolume.DirtyValue(m_period30Nov)
                    m_vol30NovPerSample.Volume(Multivariate.CurrentSample) = s.EndVolume(m_stepPeriod30Nov)
                    'm_vol30Nov = s.EndVolume(m_stepPeriod30Nov)
                    's.GetEndVolumePrimalList(d)
                    'm_vol30Nov = s.EndVolumeOUT(m_period30Nov)
                End If
            End If
            If s.Name.Equals("Laja_Gen", StringComparison.OrdinalIgnoreCase) Then
                'Dim d(CurrentModel.Steps.StepPeriodCount) As Double
                's.GetEndVolumePrimalList(d)
                'Dim endVLajaGen As Double = s.EndVolume(CurrentModel.Steps.StepPeriodCountSansLookahead)
            End If
        Next
    End Sub

    Public Sub AfterRecordSolution() Implements IOpenModel.AfterRecordSolution
    End Sub

    Public Sub TerminatePhase() Implements IOpenModel.TerminatePhase
    End Sub

    Public Function OnWarning(Message As String) As Boolean Implements IOpenModel.OnWarning
        Return False
    End Function

    Public Function EnforceMyConstraints() As Integer Implements IOpenModel.EnforceMyConstraints
        Return 0
    End Function

    Public Function HasDynamicTransmissionConstraints() As Boolean Implements IOpenModel.HasDynamicTransmissionConstraints
        Return 0
    End Function

    Private Shared Sub InitializePhase()
        Dim dateBeginHorizon As Date = Date.FromOADate(CurrentEnergyModel.Horizon(HorizonAttributeEnum.DateFrom))
        Dim nDayBeginning As Integer = CInt(CurrentEnergyModel.Horizon(HorizonAttributeEnum.DayBeginning))
        Dim dateCur As Date

        m_vol30NovPerSample = New Volume30Nov(Multivariate.SampleCount)

        'Initilize the period of the horizon for Nov 30th:
        If (m_horizonPeriod30Nov < 0) Then
            For nPeriod As Integer = 1 To CurrentEnergyModel.Horizon.PeriodCount(PeriodEnum.Interval)
                dateCur = Period2Date(nPeriod, CurrentEnergyModel.Horizon.PeriodsperDay, CurrentEnergyModel.Horizon.HoursperInterval, dateBeginHorizon, nDayBeginning)
                If (Utils.IsLajaInitDate(dateCur)) Then
                    m_horizonPeriod30Nov = nPeriod
                    Return
                End If
            Next
        End If
    End Sub

    Private Sub InitializeStep()

        Dim nInitPeriod As Integer = CurrentEnergyModel.Steps.FirstPeriod
        Dim bIsFirstStep As Boolean = CurrentEnergyModel.Steps.IsFirstStep
        m_periodInitMaule = -1
        m_stepPeriod30Nov = -1

        For Each s In StoragesIN

            If s.Name.Equals("ELTORO", StringComparison.OrdinalIgnoreCase) Then
                stoElToro = s
            ElseIf s.Name.Equals("Laja_Gen", StringComparison.OrdinalIgnoreCase) Then
                stoLaja_Gen = s
            ElseIf s.Name.Equals("Laja_Riego", StringComparison.OrdinalIgnoreCase) Then
                stoLaja_Riego = s
            ElseIf s.Name.Equals("Laja_Mixto", StringComparison.OrdinalIgnoreCase) Then
                stoLaja_Mixto = s
            ElseIf s.Name.Equals("LMAULE", StringComparison.OrdinalIgnoreCase) Then
                stoLMaule = s
            ElseIf s.Name.Equals("CIPRESES", StringComparison.OrdinalIgnoreCase) Then
                stoCipreses = s
            ElseIf s.Name.Equals("Invernada_Eco", StringComparison.OrdinalIgnoreCase) Then
                stoInvernada_Eco = s
            ElseIf s.Name.Equals("MauleLow_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then
                stoMauleLow_Eco_Gen = s
            ElseIf s.Name.Equals("MauleLow_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then
                stoMauleLow_Eco_Riego = s
            ElseIf s.Name.Equals("MauleSup_Eco_Gen", StringComparison.OrdinalIgnoreCase) Then
                stoMauleSup_Eco_Gen = s
            ElseIf s.Name.Equals("MauleSup_Eco_Riego", StringComparison.OrdinalIgnoreCase) Then
                stoMauleSup_Eco_Riego = s
            ElseIf s.Name.Equals("RALCO", StringComparison.OrdinalIgnoreCase) Then
                stoRalco = s
            ElseIf s.Name.Equals("COLBUN", StringComparison.OrdinalIgnoreCase) Then
                stoColbun = s
            End If
        Next

        For Each v As Variable In ModelGlobals.VariablesIN.InServiceObjects
            If (v.Name.Equals("LAJAVol30Nov")) Then
                varVol30Nov = v
                'If bIsFirstStep Then
                '    m_vol30NovPerSample.Volume(Multivariate.CurrentSample) = v.SampleValue(CurrentModel.Steps.FirstPeriod, PeriodEnum.Interval) * CMD2M3 'We store as 1000m3
                'End If
                'If m_vol30Nov = 0 Then
                '    m_vol30Nov = v.ArcValue(SystemVariablesEnum.Profile, 1)
                'End If
            ElseIf (v.Name.Equals("LAJAirrigators1", StringComparison.OrdinalIgnoreCase)) Then
                varLAJAirrigators1 = v
            ElseIf (v.Name.Equals("LAJAirrigators2", StringComparison.OrdinalIgnoreCase)) Then
                varLAJAirrigators2 = v
            ElseIf (v.Name.Equals("LAJAsaltos", StringComparison.OrdinalIgnoreCase)) Then
                varLAJAsaltos = v
            ElseIf v.Name.Equals("QFZ", StringComparison.OrdinalIgnoreCase) Then
                varQFZ = v
            ElseIf v.Name.Equals("QInvFree", StringComparison.OrdinalIgnoreCase) Then
                varQInvFree = v
            ElseIf v.Name.Equals("ReserveMinVolRalco", StringComparison.OrdinalIgnoreCase) Then
                varReserveMinVolRalco = v
            ElseIf v.Name.Equals("ReserveMinVolElToro", StringComparison.OrdinalIgnoreCase) Then
                varReserveMinVolElToro = v
            ElseIf v.Name.Equals("ReserveMinVolColbun", StringComparison.OrdinalIgnoreCase) Then
                varReserveMinVolColbun = v
            ElseIf v.Name.Equals("RiegoTotal_Maule", StringComparison.OrdinalIgnoreCase) Then
                varRiegoTotalMaule = v
            End If
        Next

    End Sub

    Private Shared Function IsValidIrrigation(ByRef strMessage As String) As Boolean
        Dim bIsNotValidLaja As Boolean
        Dim bIsNotValidMaule As Boolean
        Dim bIsNotValidReserveMin As Boolean
        Dim bIsNotValidRalco As Boolean
        Dim bIsNotValidColbun As Boolean

        bIsNotValidLaja = IsNothing(stoElToro) Or IsNothing(stoLaja_Gen) Or IsNothing(stoLaja_Gen) Or IsNothing(stoLaja_Mixto) Or IsNothing(stoLaja_Riego) _
                    Or IsNothing(varQFZ)
        bIsNotValidMaule = IsNothing(stoLMaule) Or IsNothing(stoCipreses) Or IsNothing(stoInvernada_Eco) Or IsNothing(stoMauleLow_Eco_Gen) Or IsNothing(stoMauleLow_Eco_Riego) _
                    Or IsNothing(stoMauleSup_Eco_Gen) Or IsNothing(stoMauleSup_Eco_Riego) Or IsNothing(varQInvFree)
        bIsNotValidReserveMin = IsNothing(varReserveMinVolColbun) Or IsNothing(varReserveMinVolRalco) Or IsNothing(varReserveMinVolElToro)
        bIsNotValidRalco = IsNothing(stoRalco)
        bIsNotValidColbun = IsNothing(stoColbun)
        Return Not bIsNotValidLaja And Not bIsNotValidMaule And Not bIsNotValidReserveMin And Not bIsNotValidRalco And Not bIsNotValidColbun And CurrentEnergyModel.CurrentPhase.Index = SimulationPhaseEnum.STSchedule
    End Function

    Public Sub BeginInitialize() Implements IOpenModel.BeginInitialize
        'Throw New NotImplementedException()
    End Sub
End Class


Class Volume30Nov

    Private m_vol30Nov As List(Of Double)

    Public Sub New(Samples As Integer)
        m_vol30Nov = New List(Of Double)
        For i As Integer = 1 To Samples
            m_vol30Nov.Add(0.0)
        Next
    End Sub

    Public Property Volume(nSample As Integer)
        Get
            Return m_vol30Nov(nSample - 1)
        End Get
        Set
            m_vol30Nov(nSample - 1) = Value
        End Set
    End Property

End Class