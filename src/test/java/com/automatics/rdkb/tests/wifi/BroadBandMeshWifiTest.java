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
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.webpa.WebPaServerResponse;
import com.automatics.rdkb.utils.BroadBandMeshUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;

public class BroadBandMeshWifiTest extends AutomaticsTestBase {
    
    // String which represents the type of device.we are using it temporarily as
    // this test case is applicable only to
    private static String deviceType = "XB";

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
    
    /**
     * 
     * This method tests the behaviour of wifi mesh service in enabled state. service is expected to retain it's enabled
     * state even after reboot.
     * 
     * <ol>
     * <li>step 1 :Enable the wifi mesh service using TR181 parameter
     * <li>EXPECTED : The wifi mesh service should be enabled successfully
     * 
     * <li>step 2 : Check the wifi mesh enable status using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Enable
     * <li>EXPECTED : parameter should return true
     * 
     * <li>step 3 : Verify mesh status using tr-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Status
     * <li>EXPECTED: Value of this parameter must be Full
     * 
     * <li>step 4 : Verify mesh status using tr-181 parameter Device.X_RDKCENTRAL-COM_Mesh.State
     * <li>EXPECTED: Value of this parameter must be Full
     * 
     * <li>step 5 : verify the plume back end url using TR-181 paramter
     * <li>EXPECTED : parameter should have a value :ssl:wildfire.plume.tech:443
     * 
     * <li>step 6 : Check the status of wifimesh service through Atom console by using command systemctl status
     * meshwifi.service
     * <li>EXPECTED : meshwifi service status should be active
     * 
     * <li>step 7 : Verify the mesh wifi enable status from /rdklogs/logs/MeshAgentLog.txt.0
     * <li>EXPECTED : Logs must contain information about enabling of wifi mesh service
     * 
     * <li>step 8 : reboot the device and then observe the behaviour of wifi mesh service agent
     * <li>EXPECTED : box should be reboot and come up with a proper Ip
     * 
     * <li>Step 9: Verify the mesh enable logs in /rdklogs/logs/MeshAgentLog.txt.0
     * <li>EXPECTED: since the mesh service active before reboot after reboot also wifi agent must be enabled
     * 
     * <li>step 10 : verify the mesh enable status using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Enable
     * <li>EXPECTED :Mesh enable status must be true after reboot
     * 
     * <li>step 11 :Check the wifi mesh service status through Atom console using systemctl command
     * <li>EXPECTED : wifimesh service status must be active after reboot
     * 
     * <li>step 12 : Disable meshwifi service using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Enable
     * <li>EXPECTED : wifi mesh service should be successfully disabled
     * 
     * <li>step 13 : Verify whether the wifimesh service is inactive through Atom console using systemctl
     * <li>EXPECTED :wifimesh service should be inactive since it is disabled in previous step
     * 
     * <li>step 14 : verify the enable status of wifi mesh service using TR-181 paramter
     * <li>EXPECTED: parameter should return value as FALSE
     * 
     * <li>step 15 : verify the wifi mesh service status using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Status
     * <li>EXPECTED : wifi mesh service status parameter should return value as OFF
     * 
     * <li>step 16 : verify the wifi mesh service status using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.State
     * <li>EXPECTED : wifi mesh service status parameter should return value as Full
     * 
     * <li>step 17 : verify plume back end url using TR-181 paramenter Device.X_RDKCENTRAL-COM_Mesh.BackhaulURL
     * <li>EXPECTED: parameter should return value ssl:wildfire.plume.tech:443
     * 
     * <li>step 18 : Check the wifi mesh service disabled logs from /rdklogs/logs/MeshAgentLog.txt.0
     * <li>EXPECTED : Logs related to disabling of wifi mesh service must be present in /rdklogs/logs/MeshAgentLog.txt.0
     * 
     * @author Rahul Raveendran
     * @refactor Govardhan
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-WIFI-1001")
    public void testVerifyEnabledMeshAgent(Dut device) {

	// stores the execution status
	boolean status = false;
	// String to store the testid
	String testId = "TC-RDKB-WIFI-001";
	// string to store the errormessage
	String errorMessage = null;
	// String to store the step Number
	String stepNumber = "s1";
	// String to store the response
	String response = null;	
	//  String to store Plume back haul url for wifi mesh service
	String PLUME_BACK_HAUL_URL = null;
	
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-1001");
	LOGGER.info("TEST DESCRIPTION: verify the status of wifi mesh service on enabling and disabling it");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Enable the wifi mesh service using TR181 parameter");
	LOGGER.info("2. Check the wifi mesh enable status using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Enable");
	LOGGER.info("3. Verify mesh status using tr-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Status");
	LOGGER.info("4. Verify mesh status using tr-181 parameter Device.X_RDKCENTRAL-COM_Mesh.State");
	LOGGER.info("5. verify the plume back end url using TR-181 paramter");
	LOGGER.info(
		"6. Check the status of wifimesh service through Atom console by using command systemctl status meshwifi.service");
	LOGGER.info("7. Verify the mesh wifi enable status from /rdklogs/logs/MeshAgentLog.txt.0");
	LOGGER.info("8. reboot the device and then observe the behaviour of wifi mesh service agent");
	LOGGER.info("9. Verify the mesh enable logs in /rdklogs/logs/MeshAgentLog.txt.0");
	LOGGER.info("10. verify the mesh enable status using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Enable");
	LOGGER.info("11. Check the wifi mesh service status through Atom console using systemctl command");
	LOGGER.info("12. Disable meshwifi service using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Enable");
	LOGGER.info("13. Verify whether the wifimesh service is inactive through Atom console using systemctl");
	LOGGER.info("14. verify the enable status of wifi mesh service using TR-181 paramter");
	LOGGER.info(
		"15. verify the wifi mesh service ststus using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Status");
	LOGGER.info(
		"16. verify the wifi mesh service ststus using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.State");
	LOGGER.info("17. verify plume back end url using TR-181 paramenter Device.X_RDKCENTRAL-COM_Mesh.BackhaulURL ");
	LOGGER.info("18. Check the wifi mesh service disabled logs from /rdklogs/logs/MeshAgentLog.txt.0 ");
	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    // Since Mesh is by default enabled hence we are disabling it via XPC instead of
	    // WEBPA Param as part of
	    try {
		BroadBandMeshUtils.enableOrDisableMesh(device, tapEnv, false);
	    } catch (Exception e) {
		LOGGER.error("Exception Occured While disabling Mesh via XPC : " + e.getMessage());
	    }
	    LOGGER.info("############################################################################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : DISABLE MESH WIFI SERVICE");
	    LOGGER.info(
		    "PRE-CONDITION : ACTION :  EXECUTE PARAMS: the Band Steering(Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Enable), Dynamic Channel Selection(Device.WiFi.Radio.{i}.X_COMCAST-COM_DCSEnable, Bridge  mode(Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode with false using WebPA command");
	    LOGGER.info("PRE-CONDITION : EXPECTED : WEBPA EXECUTION SHOULD BE SUCCESSFUL");
	    LOGGER.info("############################################################################");
	    String[] parameters = new String[] { BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLE_DYNAMIC_CHANNEL_SELECTION_FOR_2GHZ,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLE_DYNAMIC_CHANNEL_SELECTION_FOR_5GHZ };
	    tapEnv.executeMultipleWebPaGetCommands(device, parameters);
	    // setting mesh enable param to false using dmcli irrespective of
	    // any state from previous test
	    long startTime = System.currentTimeMillis();
	    do {
		status = disableBridgeModeBandSteeringDynamicChannelSelectionUsingWebPaCommand(device);
	    } while (!status
		    && ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS)
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info(
			"PRE-CONDITION : ACTUAL : Successfully set Band Steering, Dynamic Channel,Bridge mode selection to false using WebPA command.");
	    } else {
		LOGGER.error(
			"PRE-CONDITION : ACTUAL : Failed to Band Steering, Dynamic Channel,Bridge mode selection to false using WebPA command");
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "");
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	    /**
	     * step 1 :Enable the wifi mesh service using TR181 parameter EXPECTED : The wifi mesh service should be
	     * enabled successfully
	     */
	    status = false;
	    errorMessage = "Failed to enable WiFi mesh service using WebPA";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION: Enable the wifimesh service using TR-181 paramter");
	    LOGGER.info("STEP 1: ACTION : EXECUTE  WEBPA SET  PARAM:"
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE + " Value : True");
	    LOGGER.info("STEP 1: EXPECTED : The command should return Execution succeed");
	    LOGGER.info("**************************************************************************");

	    // setting mesh enable paarm to true using webpa
	    status = BroadBandMeshUtils.enableOrDisableMesh(device, tapEnv, true);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully set mesh enable using webpa");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);
	    LOGGER.info("**************************************************************************");
	    status = false;
	    LOGGER.info("CAPTURING WIFI STATUS :");
	    /*
	     * To capture the status of mesh change capturing the mesh status in arrayList meshStatusList breaking the
	     * loop if status are captured and if mesh is not enabled with status off
	     */
	    ArrayList<String> meshStatusList = new ArrayList<String>();
	    startTime = System.currentTimeMillis();
	    do {
		response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATUS);
		LOGGER.info("WifiMesh Service status :" + response);
		if (!meshStatusList.contains(response)) {
		    meshStatusList.add(response);
		}
		status = CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.MESHWIFI_STATE_FULL);
	    } while (System.currentTimeMillis() - startTime < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS && !status);

	    /*
	     * step 2 : Check the wifi mesh enable status using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Status
	     * EXPECTED : parameter should return true
	     */
	    stepNumber = "s2";
	    status = false;
	    errorMessage = "Failed to verify the value of "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE + " as true";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info("STEP 2 : DESCRIPTION : Verify wifi mesh enable status after enabling");
	    LOGGER.info("STEP 2 : ACTION : EXECUTE WEBPA SET PARAM: "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE + " VALUE :True");
	    LOGGER.info(
		    "STEP 2 : EXCPECTED : paramater Device.X_RDKCENTRAL-COM_Mesh.Enable should return boolean value true");
	    LOGGER.info("**************************************************************************");
	    // A polling logic for a wait time of 3 minutes to check whether wifi mesh is
	    // enabled
	    startTime = System.currentTimeMillis();
	    try {
		do {
		    status = BroadBandWiFiUtils.verifyMeshWifiParamValuesUsingWebPa(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE, RDKBTestConstants.TRUE);
		} while (System.currentTimeMillis() - startTime < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
			&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 2 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 2 : ACTUAL : Successfully verified "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE + "as true using webpa");
	    } else {
		LOGGER.error("STEP 2 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 3 : Verify mesh status using tr-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Status EXPECTED: Value of
	     * this parameter must be init
	     */
	    stepNumber = "s3";
	    status = false;
	    errorMessage = "The List of values of mesh status -" + meshStatusList.toString()
		    + " do not contain the expected value -" + BroadBandTestConstants.WIFIMESH_STATUS_INIT;
	    LOGGER.info("**************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION  : Verify mesh status using tr-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Status");
	    LOGGER.info("STEP 3: ACTION : EXECUTE WEBPA GET PARAM :"
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATUS);
	    LOGGER.info("STEP 3: EXCPECTED : value of this parameter must be Init indicationg that it is active");
	    LOGGER.info("**************************************************************************");
	    status = meshStatusList.contains(BroadBandTestConstants.WIFIMESH_STATUS_INIT);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Successfully verified "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATUS + " status as Init");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : The List of values of mesh status -" + meshStatusList.toString()
			+ " do not contain the expected value -" + BroadBandTestConstants.WIFIMESH_STATUS_INIT);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 4 : Verify mesh status using tr-181 parameter Device.X_RDKCENTRAL-COM_Mesh.State EXPECTED: Value of
	     * this parameter must be init
	     */
	    stepNumber = "s4";
	    status = false;
	    errorMessage = "wifimesh service state is not returned as Full";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info(
		    "STEP 4 : DESCRIPTION  : Verify mesh status using tr-181 parameter Device.X_RDKCENTRAL-COM_Mesh.State");
	    LOGGER.info("STEP 4 : ACTION : EXECUTE WEBPA GET PARAM: "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATE);
	    LOGGER.info("STEP 4 : EXCPECTED  : value of this parameter must be Full indicationg that it is active");
	    LOGGER.info("**************************************************************************");
	    try {
		status = BroadBandWiFiUtils.verifyMeshWifiParamValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATE,
			BroadBandTestConstants.MESHWIFI_STATE_FULL);
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 4 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully verified "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATE + " values as :"
			+ BroadBandTestConstants.MESHWIFI_STATE_FULL);
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 5 : verify the plume back end url using TR-181 paramter EXPECTED : parameter should have a value :
	     * ssl:wildfire.plume.tech:443
	     */
	    PLUME_BACK_HAUL_URL = BroadbandPropertyFileHandler.getPlumeBackHaulURL();
	    
	    stepNumber = "s5";
	    status = false;
	    errorMessage = "Plume back url value do not contain the expected url "
		    + PLUME_BACK_HAUL_URL;
	    LOGGER.info("**************************************************************************");
	    LOGGER.info("STEP 5 : DESCRIPTION : Verify the plume back end url using TR-181 parameter ");
	    LOGGER.info("STEP 5 : ACTION : EXCUTE WEBPA GET PARAM:"
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_BACKHAULURL);
	    LOGGER.info("STEP 5 : EXPECTED  : parameter should have a value :  ssl:wildfire.plume.tech:443");
	    LOGGER.info("**************************************************************************");
	    try {
		status = BroadBandWiFiUtils.verifyMeshWifiParamValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_BACKHAULURL,
			PLUME_BACK_HAUL_URL);
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 5 - " + exception.getMessage();

	    }
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully verified "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_BACKHAULURL + " value as :"
			+ PLUME_BACK_HAUL_URL);
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 6 : Check the status of wifimesh service through Atom console by using command systemctl status
	     * meshwifi.service EXPECTED : meshwifi service status should be active
	     */
	    stepNumber = "s6";
	    status = false;
	    errorMessage = "Failed to verify that the mesh service is active using systemctl comamnd";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info("STEP 6 : DESCRIPTION : VERIFY THE STATUS OF WIFI MESH SERVICE USING SYSTEMCTL COMMAND");
	    LOGGER.info(
		    "STEP 6 : ACTION : EXECUTE 'systemctl status meshwifi.service' IN ATOM FOR ATOM DEVICE ELSE IN ARM CONSOLE");
	    LOGGER.info("STEP 6 : EXPECTED  : STATUS OF WIFI MESH SERVICE SHOULD BE ACTIVE");
	    LOGGER.info("**************************************************************************");
	    try {
		status = BroadBandWiFiUtils.verifyMeshSerivceStatusUsingSystemctlFromAtomConsole(device, tapEnv, true,
			deviceType);
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 6 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully verified Mesh status using systemctl command");
	    } else {
		LOGGER.error("STEP 6 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 7 : Verify the mesh wifi enable status from /rdklogs/logs/MeshAgentLog.txt.0 EXPECTED : Logs must
	     * contain information about enabling of wifi mesh service
	     */
	    stepNumber = "s7";
	    status = false;
	    errorMessage = "Wifi mesh service enabled logs are absent";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info("STEP 7 : DESCRIPTION  : VERIFY THE MESH ENABLE STATUS FROM /rdklogs/logs/MeshAgentLog.txt.0");
	    LOGGER.info("STEP 7 : ACTION : EXECUTE cat /rdklogs/logs/MeshAgentLog.txt.0 AND CHECK");
	    LOGGER.info("STEP 7 : EXPECTED  : ENABLING OF WIFI MESH SERVICE MUST BE LOGGED");
	    LOGGER.info("**************************************************************************");
	    try {
		status = BroadBandWiFiUtils.verifyWifiMeshStatusFromLogs(device, tapEnv, true, deviceType);
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 7 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 7 : ACTUAL : Successfully verified Mesh service in MeshAgentLog.txt.0");
	    } else {
		LOGGER.error("STEP 7 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 8 : reboot the device and then observe the behaviour of wifi mesh service agent EXPECTED : box
	     * should be reboot and come up with a proper Ip
	     */
	    stepNumber = "s8";
	    status = false;
	    errorMessage = "Device is not accessible after reboot!!";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info("STEP 8 : DESCRIPTION  : REBOOT THE DEVICE");
	    LOGGER.info("STEP 8 : ACTION : EXECUTE reboot COMMAND");
	    LOGGER.info("STEP 8 : EXPECTED: THE BOX SHOULD REBOOT AND COME UP WITH AV");
	    LOGGER.info("**************************************************************************");
	    // rebooting the box and verifying Ip acquistion
	    status = CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device)
		    && BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Successfully rebooted device and verified webpa status");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

	    /*
	     * Step 9: Verify the mesh enable logs in /rdklogs/logs/MeshAgentLog.txt.0 EXPECTED: since the mesh service
	     * active before reboot after reboot also wifi agent must be enabled .
	     */
	    stepNumber = "s9";
	    status = false;
	    errorMessage = "Wifi mesh service enabled logs are absent after reboot";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info(
		    "STEP 9 : DESCRIPTION  : VERIFY THE MESH ENABLE STATUS FROM /rdklogs/logs/MeshAgentLog.txt.0 AFTER REBOOT");
	    LOGGER.info("STEP 9 : ACTION : EXECUTE cat /rdklogs/logs/MeshAgentLog.txt.0 AND CHECK");
	    LOGGER.info("STEP 9 : EXPECTED  : ENABLING OF WIFI MESH SERVICE MUST BE LOGGED");
	    LOGGER.info("**************************************************************************");
	    try {
		status = BroadBandWiFiUtils.verifyWifiMeshStatusFromLogs(device, tapEnv, true, deviceType);
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 9 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 9 : ACTUAL : Successfully verified Wifi mesh status in MeshAgentLog log file");
	    } else {
		LOGGER.error("STEP 9 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 10 : verify the mesh enable status using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Enable
	     * EXPECTED : Mesh enable status must be true after reboot
	     */
	    stepNumber = "s10";
	    status = false;
	    errorMessage = "Failed to verify the value of "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE + " as true";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION  : VERIFY THE MESH ENABLE STATUS USING TR-181 PARAMETER Device.X_RDKCENTRAL-COM_Mesh.Enable");
	    LOGGER.info("STEP 10 : ACTION : EXECUTE WEBPA GET PARAM : "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
	    LOGGER.info("STEP 10 : EXPECTED  : MESH ENABLE STATUS MUST BE TRUE AFTER REBOOT");
	    LOGGER.info("**************************************************************************");
	    try {
		status = BroadBandWiFiUtils.verifyMeshWifiParamValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE, RDKBTestConstants.TRUE);
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 10 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 10 : ACTUAL : Successfully verified "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE + " value as true");
	    } else {
		LOGGER.error("STEP 10 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 11 :Check the wifi mesh service status through Atom console using systemctl command EXPECTED :
	     * wifimesh service status must be active after reboot
	     */
	    stepNumber = "s11";
	    status = false;
	    errorMessage = "Failed to verify that the mesh service is active using systemctl comamnd";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info(
		    "STEP 11 : DESCRIPTION  : CHECK THE WIFI MESH SERVICE STATUS THROUGH ATOM CONSOLE USING systemctl");
	    LOGGER.info(
		    "STEP 11 : ACTION : EXECUTE COMMAND 'systemctl status meshwifi.service' IN ATOM FOR ATOM ELSE IN ARM CONSOLE");
	    LOGGER.info("STEP 11 : EXPECTED  : MESHWIFI SERVICE STATUS MUST BE ACTIVE ");
	    LOGGER.info("**************************************************************************");
	    try {
		status = BroadBandWiFiUtils.verifyMeshSerivceStatusUsingSystemctlFromAtomConsole(device, tapEnv, true,
			deviceType);
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 11 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Successfully verified wifi mesh service in atom console ");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 12 : Disable meshwifi service using TR-181 parameter Device.X_RDKCENTRAL-COM_Mesh.Enable EXPECTED :
	     * wifi mesh service should be successfully disabled
	     */
	    stepNumber = "s12";
	    status = false;
	    errorMessage = "Failed to disable WiFi mesh service using Xpc Apis";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info("STEP 12 : DESCRIPTION  : DISABLE MESHWIFI SERVICE USING XPC APIs");
	    LOGGER.info("STEP 12 : ACTION : EXECUTE WEBPA SET PARAM :"
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE + " VALUE : False");
	    LOGGER.info("STEP 12 : EXPECTED  : COMMAND SHOULD RETURN EXECUTION SUCCEED");
	    LOGGER.info("**************************************************************************");
	    status = BroadBandMeshUtils.enableOrDisableMesh(device, tapEnv, false);
	    if (status) {
		LOGGER.info("STEP 12 : ACTUAL : Successfully verified Mesh wifi service ");
		LOGGER.info("Waiting for 60 seconds after disabling mesh via XPC... ");
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    } else {
		LOGGER.error("STEP 12 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

	    /*
	     * step 13 : Verify whether the wifimesh service is inactive through Atom console using systemctl EXPECTED :
	     * wifimesh service should be inactive since it is disabled in previous step
	     */
	    stepNumber = "s13";
	    status = false;
	    errorMessage = "Failed to verify that the mesh service is inactive using systemctl comamnd";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info(
		    "STEP 13 : DESCRIPTION  : VERFIY WHETHER THE WIFI MESH SERVICE IS INACTIVE THROUGH ATOM CONSOLE USING systemctl");
	    LOGGER.info(
		    "STEP 13 : ACTION : EXECUTE COMMAND 'systemctl status meshwifi.service' IN ATOM FOR ATOM ELSE IN ARM CONSOLE");
	    LOGGER.info("STEP 13 : EXPECTED  : WIFIMESH SERVICE STATUS SHOULD BE INACTIVE");
	    LOGGER.info("**************************************************************************");
	    startTime = System.currentTimeMillis();
	    try {
		do {
		    status = BroadBandWiFiUtils.verifyMeshSerivceStatusUsingSystemctlFromAtomConsole(device, tapEnv,
			    false, deviceType);
		} while (System.currentTimeMillis() - startTime < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
			&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 13 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Successfully verified mesh serivce as inactive");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 14 : verify the enable status of wifi mesh service using TR-181 paramter EXPECTED: parameter should
	     * return value as FALSE
	     */
	    stepNumber = "s14";
	    status = false;
	    errorMessage = "Failed to verify the value of "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE + " as false";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info("STEP 14 : DESCRIPTION  : VERIFY THE ENABLE STATUS OF WIFI MESH SERVICE USING TR-181 PARAMTER");
	    LOGGER.info("STEP 14 : ACTION : EXECUTE WEBPA GET PARAM :"
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
	    LOGGER.info("STEP 14 : EXPECTED  : PARAMETER SHOULD RETURN VALUE AS FALSE");
	    LOGGER.info("**************************************************************************");
	    try {
		do {
		    status = BroadBandWiFiUtils.verifyMeshWifiParamValuesUsingWebPa(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE, RDKBTestConstants.FALSE);
		} while (System.currentTimeMillis() - startTime < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
			&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 14 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 14 : ACTUAL : Successfully verified "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE + "status as false");
	    } else {
		LOGGER.error("STEP 14 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 15 : verify the wifi mesh service ststus using TR-181 parameter EXPECTED : wifi mesh service status
	     * parameter should return value as OFF
	     */

	    stepNumber = "s15";
	    status = false;
	    errorMessage = "Failed to verify the value of " + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATUS
		    + " as Off";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info("STEP 15 : DESCRIPTION  : VERIFY THE MESH SERVICE STATUS USING TR-181 PARAMETER");
	    LOGGER.info("STEP 15 : ACTION : EXECUTE WEBPA GET PARAM "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATUS);
	    LOGGER.info("STEP 15 : EXPECTED  : WIFI MESH SERVICE STATUS PARAMETER SHOULD RETURN VALUE AS OFF");
	    LOGGER.info("**************************************************************************");
	    try {
		do {
		    status = BroadBandWiFiUtils.verifyMeshWifiParamValuesUsingWebPa(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATUS,
			    BroadBandTestConstants.MESHWIFI_STATE_OFF);
		} while (System.currentTimeMillis() - startTime < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
			&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 15 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 15 : ACTUAL : Successfully verified "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATUS + " using webpa parameter as Off");
	    } else {
		LOGGER.error("STEP 15 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 16 : verify the wifi mesh service state using TR-181 parameter EXPECTED : wifi mesh service state
	     * parameter should return value as OFF
	     */

	    stepNumber = "s16";
	    status = false;
	    errorMessage = "Failed to verify the value of " + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATE
		    + " as Full";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info("STEP 16 : DESCRIPTION  : VERIFY THE MESH SERVICE STATUS USING TR-181 PARAMETER");
	    LOGGER.info("STEP 16 : ACTION : EXECUTE WEBPA GET PARAM "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATE);
	    LOGGER.info("STEP 16 : EXPECTED  : WIFI MESH SERVICE STATUS PARAMETER SHOULD RETURN VALUE AS FULL");
	    LOGGER.info("**************************************************************************");
	    try {
		do {
		    status = BroadBandWiFiUtils.verifyMeshWifiParamValuesUsingWebPa(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATE,
			    BroadBandTestConstants.MESHWIFI_STATE_FULL);
		} while (System.currentTimeMillis() - startTime < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
			&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 16 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 16 : ACTUAL : Successfully verified "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_STATE + " value as "
			+ BroadBandTestConstants.MESHWIFI_STATE_FULL);
	    } else {
		LOGGER.error("STEP 16 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 17 : verify plume back end url using TR-181 paramenter Device.X_RDKCENTRAL-COM_Mesh.BackhaulURL
	     * EXPECTED : parameter should return value ssl:wildfire.plume.tech:443
	     */
	    stepNumber = "s17";
	    status = false;
	    errorMessage = "Plume back url value do not contain the expected url "
		    + PLUME_BACK_HAUL_URL;
	    LOGGER.info("**************************************************************************");
	    LOGGER.info(
		    "STEP 17 : DESCRIPTION  : VERIFY PLUME BACK END URL USING TR-181 PARAMENTER Device.X_RDKCENTRAL-COM_Mesh.BackhaulURL");
	    LOGGER.info("STEP 17 : ACTION : EXCUTE WEBPA GET PARAM "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_BACKHAULURL);
	    LOGGER.info("STEP 17 : EXPECTED  : PARAMETER SHOULD RETURN VALUE  ssl:wildfire.plume.tech:443");
	    LOGGER.info("**************************************************************************");
	    try {
		status = BroadBandWiFiUtils.verifyMeshWifiParamValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_BACKHAULURL,
			PLUME_BACK_HAUL_URL);
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 17 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 17 : ACTUAL : Successfully verified "
			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MESH_BACKHAULURL
			+ " value as BroadBandTestConstants.PLUME_BACK_HAUL_URL");
	    } else {
		LOGGER.error("STEP 17 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * step 18 : Check the wifi mesh service disabled logs from /rdklogs/logs/MeshAgentLog.txt.0 EXPECTED : Logs
	     * related to disabling of wifi mesh service must be present in /rdklogs/logs/MeshAgentLog.txt.0
	     */
	    stepNumber = "s18";
	    status = false;
	    errorMessage = "Wifi mesh service disabled logs are absent after reboot";
	    LOGGER.info("**************************************************************************");
	    LOGGER.info(
		    "STEP 18 : DESCRIPTION : CHECK THE WIFI MESH SERVICE DISABLED LOGS FROM /rdklogs/logs/MeshAgentLog.txt.0 THROUGH ATOM CONSOLE");
	    LOGGER.info("STEP 18 : ACTION : EXECUTE cat /rdklogs/logs/MeshAgentLog.txt.0 AND CHECK VALUE");
	    LOGGER.info(
		    "STEP 18 : EXPECTED  : LOGS RELATING TO DISABLING OF WIFI MESH SERVICE MUST BE PRESENT IN /rdklogs/logs/MeshAgentLog.txt.0");
	    LOGGER.info("**************************************************************************");
	    try {
		status = BroadBandWiFiUtils.verifyWifiMeshStatusFromLogs(device, tapEnv, false, deviceType);
	    } catch (TestException exception) {
		errorMessage = "Exception occured during step 18 - " + exception.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP 18 : ACTUAL : Successfully verified MeshAgentLog for mesh status");
	    } else {
		LOGGER.error("STEP 18  : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = "Exception occured during Mesh Wifi Verification - " + exception.getMessage();
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, errorMessage, true);

	}
    }

    /**
     * Utility method to disable the Band Steering(Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Enable), Dynamic Channel
     * Selection(Device.WiFi.Radio.{i}.X_COMCAST-COM_DCSEnable, Bridge
     * mode(Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode) using WebPA command.
     * 
     * @param device
     *            The Dut to be validated.
     * @return True if set operation succeeded.
     */
    private boolean disableBridgeModeBandSteeringDynamicChannelSelectionUsingWebPaCommand(Dut device) {
	List<WebPaParameter> webPaParameters = new ArrayList<WebPaParameter>();

	/*
	 * Disabling the bridge mode.
	 */
	WebPaParameter bridgeModeDisabled = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
		BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS,
		BroadBandTestConstants.LAN_MANAGEMENT_MODE_ROUTER, WebPaDataTypes.STRING.getValue());
	webPaParameters.add(bridgeModeDisabled);
	/*
	 * Disabling the band steering.
	 */
	WebPaParameter bandStreeringDisabled = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
		BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE, BroadBandTestConstants.FALSE,
		WebPaDataTypes.BOOLEAN.getValue());
	webPaParameters.add(bandStreeringDisabled);
	/*
	 * Disabling the dynamic channel selection for 2.4 GHz private SSID.
	 */
	WebPaParameter dcsOn2GhzSsidDisabled = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
		BroadBandWebPaConstants.WEBPA_PARAM_ENABLE_DYNAMIC_CHANNEL_SELECTION_FOR_2GHZ,
		BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
	webPaParameters.add(dcsOn2GhzSsidDisabled);

	/*
	 * Disabling the dynamic channel selection for 5 GHz private SSID.
	 */
	WebPaParameter dcsOn5GhzSsidDisabled = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
		BroadBandWebPaConstants.WEBPA_PARAM_ENABLE_DYNAMIC_CHANNEL_SELECTION_FOR_5GHZ,
		BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
	webPaParameters.add(dcsOn5GhzSsidDisabled);
	/*
	 * Disabling the mesh if it is already enabled.
	 */
	WebPaParameter meshDisabled = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE, BroadBandTestConstants.FALSE,
		WebPaDataTypes.BOOLEAN.getValue());
	webPaParameters.add(meshDisabled);
	WebPaServerResponse webPaSetResponse = tapEnv.setWebPaParameterValues(device, webPaParameters);
	return webPaSetResponse.getMessage().trim().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
    }

}
