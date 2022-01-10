/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.wifi;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.rdkb.utils.BroadBandMeshUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;

public class BroadBandMeshWifiTest extends AutomaticsTestBase {

    /**
     * Verify Mesh Enable does not change the 5ghz channel to DFS range
     * <ol>
     * <li>Disable Mesh using webpa and wiat for 90 seconds for the changes to get affected</li>
     * <li>Verify whether the current channel set for 5ghz is in the non overlapping range</li>
     * <li>Enable Mesh using webpa and wiat for 90 seconds for the changes to get affected</li>
     * <li>Verify whether the current channel set for 5ghz has not changed or is in the non overlapping range</li>
     * <li>Disable Mesh using webpa and wiat for 90 seconds for the changes to get affected</li>
     * <li>Verify whether the current channel set for 5ghz has not changed or is in the non overlapping range after
     * disabling mesh</li>
     * </ol>
     * 
     * @param {@link
     *            device}
     * 
     * @author anandam.s
     * @Refactor Athira
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-MESH-1000")
    public void verifyChannelNumberOnMeshEnabledState(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WIFI-MESH-100";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String initialMeshState = null;
	String initialAutoChannelState = null;
	// Variable Declaration Ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-MESH-1000");
	LOGGER.info("TEST DESCRIPTION: Verify Mesh Enable does not  change the 5ghz channel  to DFS  range ");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Disable Mesh using webpa  and wait for 90 seconds for the changes to get affected ");
	LOGGER.info("2. Set the 5ghz Channel selection as Auto  ");
	LOGGER.info("3. Verify whether the current channel set for 5ghz is in the non overlapping range ");
	LOGGER.info("4. Enable Mesh using webpa  and wait for 90 seconds for the changes to get affected ");
	LOGGER.info(
		"5. Verify whether the current channel set for 5ghz has not changed or is in the non overlapping range ");
	LOGGER.info("6. Disable Mesh using webpa  and wait for 90 seconds for the changes to get affected ");
	LOGGER.info(
		"7. Verify whether the current channel set for 5ghz has not changed or is in the non overlapping range  after disabling mesh");

	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Get the existing state of Mesh and Auto channel enable");
	    LOGGER.info(
		    "PRE-CONDITION : ACTION : Get the existing state of Mesh and Auto channel enable using webpa parameters");
	    LOGGER.info("PRE-CONDITION : EXPECTED : A non null state is obtained");

	    initialMeshState = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
	    initialAutoChannelState = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ);

	    if (CommonMethods.isNotNull(initialAutoChannelState) && CommonMethods.isNotNull(initialMeshState)) {
		LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
	    } else {
		LOGGER.error("PRE-CONDITION : ACTUAL : Pre condition failed");
	    }

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s1";
	    errorMessage = "Mesh is not in disabled state";
	    status = false;
	    List<WebPaParameter> webPaParameters = new ArrayList<WebPaParameter>();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Disable Mesh using webpa  and wait for 90 seconds for the changes to get affected ");
	    LOGGER.info(
		    "STEP 1: ACTION : Set the webpa Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable with Value  falseEg: dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable bool falseCR component name is: eRT.com.cisco.spvtg.ccsp.CRsubsystem_prefix eRT.setv from/to component(eRT.com.cisco.spvtg.ccsp.meshagent): Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.EnableExecution succeed.");
	    LOGGER.info("STEP 1: EXPECTED : Mesh should be in disabled state");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.isNotNull(initialMeshState) && initialMeshState.equals(BroadBandTestConstants.FALSE);
	    if (!status) {
		status = BroadBandMeshUtils.enableOrDisableMesh(device, tapEnv, false);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL :Disabled Mesh via webpa "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("Waiting for 90 seconds");
	    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);

	    LOGGER.info("**********************************************************************************");
	    stepNum = "s2";
	    errorMessage = "AutoChannel is not enabled";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Set the 5ghz Channel selection as Auto  ");
	    LOGGER.info("STEP 2: ACTION : Set the webpa Device.WiFi.Radio.10101.AutoChannelEnable with Value  true");
	    LOGGER.info("STEP 2: EXPECTED : 5ghz Channel selection should be set as Auto");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(initialAutoChannelState)
		    && initialAutoChannelState.equals(BroadBandTestConstants.TRUE);
	    if (!status) {
		webPaParameters.add(BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ,
			BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue()));
		status = BroadBandWebPaUtils.setMultipleParametersUsingWebPaOrDmcli(device, tapEnv, webPaParameters);
	    }

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Channel selection is set as Auto using webpa "
			+ BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ);
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");
	    stepNum = "s3";
	    errorMessage = "5ghz operating channel is in overlapping range or DFS range";
	    status = false;
	    String[] possibleChannels = new String[15];
	    String initialRadioChannel = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify whether the current channel set for 5ghz is in the non overlapping range ");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute webpa  Device.WiFi.Radio.10101.Channel and verify the channel number obtained is  any of the below channels  36,40,44,48,149,153,157,161,165");
	    LOGGER.info("STEP 3: EXPECTED : Current  5ghz operating channel is in non-overlapping range");
	    LOGGER.info("**********************************************************************************");
	    String possible5ghzChannels = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_POSSIBLECHANNELS_IN_5GHZ);
	    if (CommonMethods.isNotNull(possible5ghzChannels)) {
		possibleChannels = possible5ghzChannels.split(",");
		initialRadioChannel = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
		LOGGER.info("Current Radio channel :" + initialRadioChannel);
		if (possibleChannels.length > 0 && CommonMethods.isNotNull(initialRadioChannel)) {
		    status = Arrays.asList(possibleChannels).contains(initialRadioChannel);
		    if (!status) {
			LOGGER.error(
				"5ghz operating channel is in overlapping range or DFS range .Current radio channel is "
					+ initialRadioChannel);
			LOGGER.info("Going to set to one of the non overlapping channel range");
			status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
				WebPaDataTypes.STRING.getValue(), possibleChannels[0]);
		    }
		} else {
		    errorMessage = "Current radio channel obtained is null";
		}
	    } else {
		status = false;
		errorMessage = "Possible 5ghz radio channel list obtained using webpa "
			+ BroadBandWebPaConstants.WEBPA_PARAM_FOR_POSSIBLECHANNELS_IN_5GHZ + " is null";
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL :  Current 5ghz operating channel is in non-overlapping range");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");
	    stepNum = "s4";
	    errorMessage = "Failed to enable mesh using "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE;
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Enable Mesh using webpa  and wiat for 90 seconds for the changes to get affected ");
	    LOGGER.info(
		    "STEP 4: ACTION : Set the webpa Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable with Value  trueEg: dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable bool falseCR component name is: eRT.com.cisco.spvtg.ccsp.CRsubsystem_prefix eRT.setv from/to component(eRT.com.cisco.spvtg.ccsp.meshagent): Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.EnableExecution succeed.");
	    LOGGER.info("STEP 4: EXPECTED : Mesh should be in enabled state");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandMeshUtils.enableOrDisableMesh(device, tapEnv, true);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Enabled mesh using webpa "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("Waiting for 90 seconds");
	    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);

	    LOGGER.info("**********************************************************************************");
	    stepNum = "s5";
	    errorMessage = "5ghz operating channel is in overlapping  or DFS range  after mesh enable ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify whether the current channel set for 5ghz has not changed or is in the non overlapping range ");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute webpa  Device.WiFi.Radio.10101.Channeland verify the channel number obtained is  same as before enabling mesh or is in any of the below channels  36,40,44,48,149,153,157,161,165");
	    LOGGER.info(
		    "STEP 5: EXPECTED :  5ghz operating channel has not changed after enabling Mesh/ 5ghz operating channel is still in non overlapping range after mesh enable");
	    LOGGER.info("**********************************************************************************");

	    BroadBandResultObject result = BroadBandMeshUtils.verifyRadioChannelAfterMeshChange(device, tapEnv,
		    initialRadioChannel);
	    if (result.isStatus()) {
		LOGGER.info(
			"STEP 5: ACTUAL :  5ghz operating channel has not changed after enabling Mesh/ 5ghz operating channel is still in non overlapping range after mesh enable");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + result.getErrorMessage());
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, result.isStatus(), result.getErrorMessage(),
		    false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Failed to disable mesh using "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE;
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Disable Mesh using webpa  and wait for 90 seconds for the changes to get affected ");
	    LOGGER.info(
		    "STEP 6: ACTION : Set the webpa Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable with Value  false"
			    + "Eg: dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable bool falseCR "
			    + "component name is: eRT.com.cisco.spvtg.ccsp.CRsubsystem_prefix eRT.setv"
			    + " from/to component(eRT.com.cisco.spvtg.ccsp.meshagent): Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.EnableExecution succeed.");
	    LOGGER.info("STEP 6: EXPECTED : Mesh should be in disabled state");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandMeshUtils.enableOrDisableMesh(device, tapEnv, false);
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL :Disabled Mesh using webpa");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("Waiting for 90 seconds");
	    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "5ghz operating channel is in overlapping  or DFS range  after mesh disable ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify whether the current channel set for 5ghz has not changed or is in the non overlapping range  after disabling mesh");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute webpa  Device.WiFi.Radio.10101.Channeland verify the channel number obtained is  same as before disabling mesh or is in any of the below channels  36,40,44,48,149,153,157,161,165");
	    LOGGER.info(
		    "STEP 7: EXPECTED :  5ghz operating channel has not changed after disabling Mesh/ 5ghz operating channel is still in non overlapping range after mesh disable");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandMeshUtils.verifyRadioChannelAfterMeshChange(device, tapEnv, initialRadioChannel);
	    if (result.isStatus()) {
		LOGGER.info(
			"STEP 7: ACTUAL :  5ghz operating channel has not changed after disabling Mesh/ 5ghz operating channel is still in non overlapping range after mesh disable");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + result.getErrorMessage());
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, result.isStatus(), result.getErrorMessage(),
		    false);

	    LOGGER.info("**********************************************************************************");
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
    }

    /**
     * 
     * <ol>
     * <li>Pre condition: Disable bridge mode and enable mesh wifi using webpa</li>
     * <li>Step 1: Enable Bridge Mode using webpa request</li>
     * <li>Step 2: Check for LanMode value set to bridge mode log message</li>
     * <li>Step 3: Check for Setting Mesh to disabled when bridge mode enabled log message</li>
     * <li>Step 4: Poll for Mesh wifi has been disabled log message</li>
     * <li>Step 5: Verify Mesh Enable parameter has been set to false</li>
     * <li>Post condition: Disable bridge mode and mesh wifi using webpa</li>
     * </ol>
     * 
     * @author Ashwin sankara
     * @Refactor Athira
     * 
     * @param device
     *            Device to be used for execution
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-MESH-WIFI-1001")

    public void testVerifyBridgeModeEnabledUsingWebpaWhenMeshEnabled(Dut device) {

	// Variable Declaration begins
	boolean result = false;
	String testId = "TC-RDKB-MESH-WIFI-001";
	String errorMessage = null;
	String step = "s1";
	// Variable Declation Ends

	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-MESH-WIFI-1001");
	    LOGGER.info("TEST DESCRIPTION: Verify enabling Bridge Mode using WebPA when the MESH is enabled");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. Enable Bridge Mode using webpa request");
	    LOGGER.info("2. Check for LanMode value set to bridge mode log message");
	    LOGGER.info("3. Check for Setting Mesh to disabled when bridge mode enabled log message");
	    LOGGER.info("4. Poll for Mesh wifi has been disabled log message");
	    LOGGER.info("5. Verify Mesh Enable parameter has been set to false");
	    LOGGER.info("#######################################################################################");

	    executeBridgeMeshPrecondition(device, false, true);

	    LOGGER.info("**********************************************************************************");

	    step = "s1";
	    errorMessage = "Failed to enable bridge mode using webpa when mesh is enabled";
	    result = false;

	    LOGGER.info("STEP 1: DESCRIPTION : Enable Bridge Mode using webpa request");
	    LOGGER.info("STEP 1: ACTION : Execute webpa command to set LanManagement parameter to bridge-static");
	    LOGGER.info("STEP 1: EXPECTED : WebPA request to enable bridge mode successful");

	    result = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.CONSTANT_BRIDGE_STATIC, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

	    if (result) {
		LOGGER.info("STEP 1: ACTUAL : WebPA request to enable bridge mode successful");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    verifyMeshDisabledWhenBridgeEnabled(device, testId, BroadBandTestConstants.CONSTANT_1);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured while validating enabling bridge mode using webpa when mesh enabled: "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, result, errorMessage, true);

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-MESH-WIFI-1001");
	LOGGER.info("#######################################################################################");
    }

    /**
     * Verify enabling Bridge Mode using SNMP when the MESH is enabled.
     * <ol>
     * <li>Pre condition: Disable bridge mode and enable mesh wifi using webpa</li>
     * <li>Step 1: Enable Bridge Mode using snmpset command</li>
     * <li>Step 2: Check for LanMode value set to bridge mode log message</li>
     * <li>Step 3: Check for Setting Mesh to disabled when bridge mode enabled log message</li>
     * <li>Step 4: Poll for Mesh wifi has been disabled log message</li>
     * <li>Step 5: Verify Mesh Enable parameter has been set to false</li>
     * <li>Post condition: Disable bridge mode and mesh wifi using webpa</li>
     * </ol>
     * 
     * @author Ashwin Sankara
     * @Refactor Athira
     * 
     * @param device
     *            Device to be used for execution
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-MESH-WIFI-1002")
    public void testVerifyBridgeModeEnabledUsingSnmpWhenMeshEnabled(Dut device) {
	// Variable Declaration begins
	boolean result = false;
	String testId = "TC-RDKB-MESH-WIFI-002";
	String errorMessage = null;
	String step = "s1";
	String response = null;
	// Variable Declation Ends
	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-MESH-WIFI-1002");
	    LOGGER.info("TEST DESCRIPTION: Verify enabling Bridge Mode using SNMP when the MESH is enabled");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. Enable Bridge Mode using snmpset command");
	    LOGGER.info("2. Check for LanMode value set to bridge mode log message");
	    LOGGER.info("3. Check for Setting Mesh to disabled when bridge mode enabled log message");
	    LOGGER.info("4. Poll for Mesh wifi has been disabled log message");
	    LOGGER.info("5. Verify Mesh Enable parameter has been set to false");
	    LOGGER.info("#######################################################################################");

	    executeBridgeMeshPrecondition(device, false, true);

	    LOGGER.info("**********************************************************************************");

	    step = "s1";
	    errorMessage = "Snmp command did not set bridge mode successfully";
	    result = false;

	    LOGGER.info("STEP 1: DESCRIPTION : Enable Bridge Mode using snmpset command");
	    LOGGER.info("STEP 1: ACTION : Execute snmp command to set bridge mode oid to value 1");
	    LOGGER.info("STEP 1: EXPECTED : Snmpset command to enable bridge mode successful");

	    response = BroadBandSnmpUtils.executeSnmpSetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getOid(), SnmpDataType.INTEGER,
		    BroadBandTestConstants.STRING_VALUE_ONE,
		    BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getTableIndex());
	    result = CommonMethods.isNotNull(response)
		    && response.trim().equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);

	    if (result) {
		LOGGER.info("STEP 1: ACTUAL : Snmpset command to enable bridge mode successful");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    verifyMeshDisabledWhenBridgeEnabled(device, testId, BroadBandTestConstants.CONSTANT_1);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured while validating enabling bridge mode using snmp when mesh enabled: "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, result, errorMessage, true);

	}

    }

    /**
     * Method to execute pre condition for tests to set mesh and bridge mode
     * 
     * @param device
     *            {@link Dut}
     * @param bridge
     *            Boolean value containing true/false for enable/disable bridge mode
     * @param mesh
     *            Boolean value containing value of mesh enable to be set
     * 
     */
    public static void executeBridgeMeshPrecondition(Dut device, boolean bridge, boolean mesh) {
	LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	LOGGER.info("PRE-CONDITION STEPS");
	LOGGER.info("PRE-CONDITION : DESCRIPTION : Disable bridge mode and enable mesh wifi using webpa");
	LOGGER.info("PRE-CONDITION : ACTION : Execute webpa commands to set LanManagement parameter to router"
		+ " and MeshEnable parameter to true");
	LOGGER.info("PRE-CONDITION : EXPECTED : Pre condition executed successfully");

	boolean result = BroadBandWebPaUtils.setAndVerifyBridgeModeAndMesh(device, tapEnv, bridge, mesh);

	if (result) {
	    LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
	} else {
	    LOGGER.error("PRE-CONDITION : ACTUAL : Pre condition failed");
	    throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR);
	}
	LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
    }

    /**
     * Method to execute steps to validate mesh gets disabled when we enable bridge mode
     * 
     * @param device
     *            {@link Dut}
     * @param testId
     *            String containing test case id to update step execution status
     * @param stepNum
     *            Integer containing previous step executed after which these steps are validated
     * 
     * @author Ashwin Sankara
     * @Refactor Athira
     * 
     */
    public static void verifyMeshDisabledWhenBridgeEnabled(Dut device, String testId, int stepNum) {

	String step = "s" + ++stepNum;
	String errorMessage = "Unable to find bridge mode set log message in PAMlog";
	boolean result = false;

	try {
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Check for LanMode value set to bridge mode log message");
	    LOGGER.info("STEP " + stepNum
		    + ": ACTION : Execute grep command for Lan mode set to (2) log message in PAMlog");
	    LOGGER.info("STEP " + stepNum + ": EXPECTED : Log message for bridge mode set is present");
	    LOGGER.info("**********************************************************************************");
	    result = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_BRIDGE_MODE_ENABLED,
		    BroadBandTestConstants.COMMAND_NTP_LOG_FILE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
	    if (!result) {
		try {
		    int index = BroadBandTraceConstants.LOG_MESSAGE_BRIDGE_MODE_ENABLED.indexOf("(");
		    result = CommonUtils.validateTraceLog(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_BRIDGE_MODE_ENABLED.substring(0, index),
			    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, true);
		    LOGGER.error("As validation in  " + BroadBandTestConstants.COMMAND_NTP_LOG_FILE
			    + " fails, status of validation done on device trace :" + result);
		} catch (Exception e) {
		    LOGGER.error("Exception occured while validating device trace " + e.getMessage());
		}
	    }

	    if (result) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Log message for bridge mode set is present");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);
	    step = "s" + ++stepNum;
	    errorMessage = "Unable to find setting mesh to disabled log message in PAMlog";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNum
		    + ": DESCRIPTION : Check for Setting Mesh to disabled when bridge mode enabled log message");
	    LOGGER.info("STEP " + stepNum
		    + ": ACTION : Execute grep command for setting mesh to disabled log message in PAMlog");
	    LOGGER.info("STEP " + stepNum + ": EXPECTED : Log message for setting mesh to disabled is present");
	    LOGGER.info("**********************************************************************************");
	    result = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_SETTING_MESH_DISABLED,
		    BroadBandTestConstants.COMMAND_NTP_LOG_FILE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

	    if (!result) {
		try {
		    result = CommonUtils.validateTraceLog(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_SETTING_MESH_DISABLED,
			    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, true);
		    LOGGER.error("As validation in  " + BroadBandTestConstants.COMMAND_NTP_LOG_FILE
			    + " fails, status of validation done on device trace :" + result);

		} catch (Exception e) {
		    LOGGER.error("Exception occured while validating device trace " + e.getMessage());
		}
	    }
	    if (result) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Log message for setting mesh to disabled is present");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    step = "s" + ++stepNum;
	    errorMessage = "Unable to find Meshwifi disabled log message in MeshAgentLog";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Poll for Mesh wifi has been disabled log message");
	    LOGGER.info("STEP " + stepNum
		    + ": ACTION : Execute grep command for Meshwifi disabled log message in MeshAgentLog");
	    LOGGER.info("STEP " + stepNum + ": EXPECTED : Log message for Meshwifi disabled is present");
	    LOGGER.info("**********************************************************************************");
	    long startTime = System.currentTimeMillis();

	    do {
		result = CommonMethods.isNotNull(BroadBandCommonUtils.searchArmOrAtomLogFile(tapEnv, device,
			BroadBandTestConstants.MESHWIFI_DISABLED_LOG, BroadBandCommandConstants.LOG_FILE_MESHAGENT));
	    } while ((System.currentTimeMillis() - startTime < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS) && !result
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (!result) {
		try {
		    result = CommonUtils.validateTraceLog(tapEnv, device, BroadBandTestConstants.MESHWIFI_DISABLED_LOG,
			    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, true);
		    LOGGER.error("As validation in  " + BroadBandCommandConstants.LOG_FILE_MESHAGENT
			    + " fails, status of validation done on device trace :" + result);

		} catch (Exception e) {
		    LOGGER.error("Exception occured while validating device trace " + e.getMessage());
		}
	    }
	    if (result) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Log message for Meshwifi disabled is present");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);
	    step = "s" + ++stepNum;
	    errorMessage = "Value of MeshEnable parameter is not false";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify Mesh Enable parameter has been set to false");
	    LOGGER.info("STEP " + stepNum + ": ACTION : Execute webpa command to get value of MeshEnable parameter");
	    LOGGER.info("STEP " + stepNum + ": EXPECTED : Value of MeshEnable parameter is false");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    if (result) {
		LOGGER.info("STEP " + stepNum + ": ACTUAL : Value of MeshEnable parameter is false");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);
	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);
	}
    }

    /**
     * 
     * This method tests the behavior of enabling mesh when bridge mode is enabled. It is expected that mesh cannot be
     * enabled when bridge mode is enabled.
     * 
     * <ol>
     * <li>Pre condition: Enable bridge mode and disable mesh wifi using webpa</li>
     * <li>Step 1: Enable mesh wifi using webpa request</li>
     * <li>Step 2: Check for Mesh enable error message in MeshAgentlog file</li>
     * <li>Post condition: Disable bridge mode and mesh wifi using webpa</li>
     * </ol>
     * 
     * @author Ashwin sankara
     * @refactor Govardhan
     * 
     * @param device
     *            Dut to be used for execution
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-MESH-WIFI-1004")
    public void testVerifyMeshNotEnableWhenBridgeModeEnabled(Dut device) {

	// Variable Declaration begins
	boolean result = false;
	String testId = "TC-RDKB-MESH-WIFI-004";
	String errorMessage = null;
	String step = "s1";
	// Variable Declation Ends

	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-MESH-WIFI-1004");
	    LOGGER.info("TEST DESCRIPTION: Verify mesh not getting enabled when bridge mode is enabled");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. Enable mesh wifi using webpa request");
	    LOGGER.info("2. Check for Mesh enable error message in MeshAgentlog file");
	    LOGGER.info("#######################################################################################");

	    executeBridgeMeshPrecondition(device, true, false);

	    step = "s1";
	    errorMessage = "WebPA request to enable mesh successful";
	    result = false;

	    LOGGER.info("STEP 1: DESCRIPTION : Enable mesh wifi using WebPA");
	    LOGGER.info("STEP 1: ACTION : Execute webpa commands to set MeshEnable parameter to true");
	    LOGGER.info("STEP 1: EXPECTED : WebPA request to enable mesh wifi should not be successful");

	    result = !BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_0);

	    if (result) {
		LOGGER.info("STEP 1: ACTUAL : WebPA request to enable mesh wifi not successful");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    step = "s2";
	    errorMessage = "Unable to find mesh enable error message in MeshAgentlog file";
	    result = false;

	    LOGGER.info("STEP 2: DESCRIPTION : Check for Mesh enable error message in MeshAgentlog file");
	    LOGGER.info("STEP 2: ACTION : Execute grep command for Mesh enable error message in MeshAgentLog");
	    LOGGER.info("STEP 2: EXPECTED : Mesh enable error message present in MeshAgentlog file");

	    result = CommonMethods.isNotNull(BroadBandCommonUtils.searchArmOrAtomLogFile(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_MESH_ERROR_BRIDGE_ENABLED,
		    BroadBandCommandConstants.LOG_FILE_MESHAGENT));

	    if (result) {
		LOGGER.info("STEP 2: ACTUAL : Mesh enable error message present in MeshAgentlog file");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured while validating mesh not getting enabled when bridge mode is enabled: "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, result, errorMessage, true);

	} finally {
	    if (!errorMessage.equalsIgnoreCase(BroadBandTestConstants.PRE_CONDITION_ERROR)) {
		executeBridgeMeshPostCondition(device);
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-MESH-WIFI-1004");
	LOGGER.info("#######################################################################################");
    }
    
    /**
     * Method to execute posts condition for tests to disable mesh and bridge mode
     * 
     * @param device
     *            {@link Dut}
     */
    public static void executeBridgeMeshPostCondition(Dut device) {
	LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	LOGGER.info("POST-CONDITION STEPS");
	LOGGER.info("POST-CONDITION : DESCRIPTION : Disable bridge mode and mesh wifi using webpa");
	LOGGER.info("POST-CONDITION : ACTION : Execute webpa commands to set LanManagement parameter"
		+ " to router and MeshEnable parameter to false");
	LOGGER.info("POST-CONDITION : EXPECTED : Post condition executed successfully");

	boolean result = BroadBandWebPaUtils.setAndVerifyBridgeModeAndMesh(device, tapEnv, false, false);

	if (result) {
	    LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	} else {
	    LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	}
	LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + result);
	LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
    }
}
