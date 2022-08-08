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
package com.automatics.rdkb.tests.webpa;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.http.HttpStatus;
import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.enums.ProcessRestartOption;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTelemetryConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants.DeviceManagementServerParams;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.server.WhiteListServer;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandMeshUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandSystemUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.CodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.parentalcontrol.BroadBandParentalControlUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWifiWhixUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.webpa.WebPaServerResponse;
import com.automatics.utils.AutomaticsPropertyUtility;

public class BroadBandWebPaTests extends AutomaticsTestBase {
    /**
     * 
     * This method verifies that webpa request to get value of Webpa.version parameter gives the value of WebPA version
     * 
     * <ol>
     * <li>Step 1 : Verify retrieval of WebPA version in TR181 parameter</li>
     * </ol>
     * 
     * @param device
     *            Dut to be used for execution
     * 
     * @author Ashwin Sankarasubramanian, Ashwin Sankara
     * @refactor Govardhan
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.WEBPA })
    @TestDetails(testUID = "TC-RDKB-WEBPA-1003")
    public void testVerifyWebPAVersion(Dut device) {
	// Variables declaration starts
	boolean status = false;
	String testId = "TC-RDKB-WEBPA-003";
	String errorMessage = null;
	String response = null;
	String stepNum = null;
	String webpaVersion = null;
	ArrayList<String> patternMatchedStringList = new ArrayList<>();
	// Variables declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1003");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the retrieval of webpa version from tr181 parameter ");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify WebPA version obtained using WebPA request.");
	LOGGER.info("#######################################################################################");
	try {
	    stepNum = "S1";
	    errorMessage = "Unable to get the webpa version.";
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify WebPA version obtained using WebPA request.");
	    LOGGER.info("STEP 1: ACTION : ACTION: Execute the TR-181 parameter-Device.X_RDKCENTRAL-COM_Webpa.Version.");
	    LOGGER.info("STEP 1: EXPECTED : WebPA request response contains WebPA version.");
	    LOGGER.info("*****************************************************************************************");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WEBPA_VERSION);
	    if (CommonMethods.isNotNull(response)
		    && response.contains(BroadBandTestConstants.PROCESS_NAME_WEBPA.toUpperCase())) {
		patternMatchedStringList = CommonMethods.patternFinderToReturnAllMatchedString(response,
			BroadBandTestConstants.WEBPA_VERSION_PATTERN_MATCHER);
		if (patternMatchedStringList.size() == BroadBandTestConstants.CONSTANT_2) {
		    webpaVersion = patternMatchedStringList.get(BroadBandTestConstants.CONSTANT_0) + "."
			    + patternMatchedStringList.get(BroadBandTestConstants.CONSTANT_1);
		}
	    }
	    status = CommonMethods.isNotNull(webpaVersion);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : WebPA request response contains WebPA version: " + webpaVersion);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

	} catch (Exception exception) {
	    LOGGER.info("Inside catch");
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured while validating webpa version using webpa request: " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNum, status, errorMessage, true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-1003");
    }

    /**
     * Validating SSID of private 5 GHz network is advertised
     * <ol>
     * <li>PRE-CONDITION :Check whether WebPA is Up and Running</li>
     * <li>STEP 1: Validating SSID of private 5 GHz network is advertised</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @Refactor Athira
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-6001")
    public void webpaValidate5GHznetworkAdvertised(Dut device) {

	boolean status = false;
	String testId = "TC-RDKB-WEBPA-601";
	String testStep = null;
	String errorMessage = null;
	String testStepNumber = "s1";
	boolean private5GhzSsidAdvertized = false;
	try {

	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WEBPA-6001 ####################");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("TEST DESCRIPTION: Validating SSID of private 5 GHz network is advertised");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("PRE-CONDITION : Check whether Webpa is Up and Running");
	    LOGGER.info("STEP 1: Validating SSID of private 5 GHz network is advertised");

	    LOGGER.info("##########################  STARTING PRE-CONFIGURATIONS ##########################");
	    LOGGER.info("PRECONDITION: DESCRIPTION: VERIFY WHETHER WEBPA IS UP AND RUNNING");
	    LOGGER.info(
		    "PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
	    LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    errorMessage = "WEBPA PROCESS IS NOT UP AND RUNNING";
	    if (!status) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    } else {
		LOGGER.info("WEBPA PROCESS IS UP AND RUNNING, PROCEEDING FOR TEST!");
	    }
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("#########################  COMPLETED PRE-CONFIGURATIONS #########################");
	    LOGGER.info("**********************************************************************************");

	    testStep = "s1";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: Validating SSID of private 5 GHz network is advertised");
	    LOGGER.info(
		    "STEP 1: ACTION :Set a new value for SSID and get the same value and Validating SSID of private 5.0 GHz network is advertised");
	    LOGGER.info("STEP 1: EXPECTED: Set a new value for SSID and get the same value and SSID is advertised");

	    errorMessage = "Failed to set 5 Ghz SSID name using WebPA parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME;
	    // Set Current 5Ghz SSID name, other than default value
	    LOGGER.info("Modify Current SSID name of 5 Ghz as " + BroadBandTestConstants.STRING_TEST_1);

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_TEST_1);
	    LOGGER.info("Is Current SSID for 5 Ghz modified - " + status);

	    if (status) {
		String response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED);

		if (CommonMethods.isNotNull(response)) {
		    private5GhzSsidAdvertized = Boolean.parseBoolean(response);
		    if (!private5GhzSsidAdvertized) {
			errorMessage = errorMessage
				+ (CommonMethods.isNotNull(errorMessage) ? BroadBandTestConstants.CHAR_NEW_LINE
					: BroadBandTestConstants.EMPTY_STRING)
				+ "Private 5 GHz SSID advertisement (Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled) is in disabled state";
		    }
		} else {
		    errorMessage = errorMessage
			    + (CommonMethods.isNotNull(errorMessage) ? BroadBandTestConstants.CHAR_NEW_LINE
				    : BroadBandTestConstants.EMPTY_STRING)
			    + "Unable to retrieve the Private 5 GHz SSID advertisement enabled status (Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled) via WebPA/DMCLI";
		}

	    } else {
		errorMessage = ("Unable to retrieve the Private 5 GHz SSID name (Device.WiFi.SSID.10101.SSID) via WebPA/DMCLI");
	    }

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Set new value for private 5.0 GHz SSID and Validated SSID of private 5.0 GHz network is advertised");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, false);
	}

    }

    /**
     * Validation of Wi-Fi bridge mode or LAN mode
     * <ol>
     * <li>PRE-CONDITION :Check whether Webpa is Up and Running</li>
     * <li>STEP 1: Validation of Wi-Fi - bridge mode or LAN mode</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @Refactor Athira
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-6013")
    public void webpaBridgemodeValidation(Dut device) {

	boolean status = false;
	String testId = "TC-RDKB-WEBPA-613";
	String testStep = null;
	String errorMessage = null;

	try {

	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WEBPA-6013 ####################");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("TEST DESCRIPTION: Validation of Wi-Fi operation mode - bridge mode or LAN mode");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("PRE-CONDITION : Check whether Webpa is Up and Running");
	    LOGGER.info("STEP 1: Validating Wi-Fi operation mode bridge mode or LAN mode");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("##########################  STARTING PRE-CONFIGURATIONS ##########################");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("PRECONDITION: DESCRIPTION: VERIFY WHETHER WEBPA IS UP AND RUNNING");
	    LOGGER.info(
		    "PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
	    LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("*********************************************************************************");
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    errorMessage = "WEBPA PROCESS IS NOT UP AND RUNNING";
	    if (!status) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    } else {
		LOGGER.info("WEBPA PROCESS IS UP AND RUNNING, PROCEEDING FOR TEST!");
	    }
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("#########################  COMPLETED PRE-CONFIGURATIONS #########################");
	    ;
	    LOGGER.info("**********************************************************************************");

	    testStep = "s1";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: Validation of  Wi-Fi bridge mode or LAN mode");
	    LOGGER.info("STEP 1: ACTION : Change the device to bridge / LAN mode");
	    LOGGER.info("STEP 1: EXPECTED: Device should persist in the same mode after being set");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Device failed to persist in the same mode after being set ";

	    status = BroadBandCommonUtils.setDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv, device);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Device persist in the Bridge mode after being set");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, false);
	} finally {
	    int postCondition = 0;
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");

	    /**
	     * POST-CONDITION 1: Revert the device back to Router mode.
	     */
	    postCondition++;

	    LOGGER.info("#####################################################################################");
	    LOGGER.info("POST-CONDITION " + postCondition + ":  DESCRIPTION :Revert the device back to Router mode.");
	    LOGGER.info("POST-CONDITION " + postCondition
		    + ": ACTION : Execute webpa command : Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode with value as router");
	    LOGGER.info("POST-CONDITION " + postCondition + ": EXPECTED: Should be able to set lanMode as router");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandCommonUtils.setDeviceInRouterModeStatusUsingWebPaCommand(tapEnv, device);

	    if (status) {
		LOGGER.info("Reverted the device back to Router mode.");
	    }
	    if (status) {
		LOGGER.info("POST-CONDITION " + postCondition + ": ACTUAL : Reverted the device back to Router mode.");
	    } else {
		LOGGER.error("POST-CONDITION " + postCondition + ": ACTUAL : Post condition failed ");
	    }
	}
	LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-WEBPA-6013 ####################");
    }

    /**
     * Validating SSID of public 2.4 GHz SSID is advertised
     * <ol>
     * <li>PRE-CONDITION :Check whether WebPA is Up and Running</li>
     * <li>STEP 1: Validating SSID of Public 2.4 GHz network is advertised</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @Refactor Athira
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-6022")
    public void webpaValidatePublic2_4GHznetworkAdvertised(Dut device) {

	boolean status = false;
	String testId = "TC-RDKB-WEBPA-622";
	String testStep = null;
	String errorMessage = null;
	String testStepNumber = "s1";
	boolean public2_4GhzSsidAdvertized = false;
	try {

	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WEBPA-6022 ####################");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("TEST DESCRIPTION: Validating SSID of public 2.4 GHz network is advertised");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("PRE-CONDITION : Check whether Webpa is Up and Running");
	    LOGGER.info("STEP 1: Validating SSID of public 2.4 GHz network is advertised");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("##########################  STARTING PRE-CONFIGURATIONS ##########################");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("PRECONDITION: DESCRIPTION: VERIFY WHETHER WEBPA IS UP AND RUNNING");
	    LOGGER.info(
		    "PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
	    LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    errorMessage = "WEBPA PROCESS IS NOT UP AND RUNNING";
	    if (!status) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    } else {
		LOGGER.info("WEBPA PROCESS IS UP AND RUNNING, PROCEEDING FOR TEST!");
	    }
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("#########################  COMPLETED PRE-CONFIGURATIONS #########################");

	    LOGGER.info("**********************************************************************************");

	    testStep = "s1";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: Validating SSID of public 2.4 GHz network is advertised");
	    LOGGER.info(
		    "STEP 1: ACTION :Set a new value for SSID and get the same value and Validating SSID of public 2.4 GHz network is advertised");
	    LOGGER.info("STEP 1: EXPECTED: Set a new value for SSID and get the same value and SSID is advertised");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to set public 2.4 Ghz SSID name using WebPA parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_NAME;
	    // Set Current public 2.4Ghz SSID name, other than default value SSID
	    LOGGER.info("Modify Current SSID name of 2.4 Ghz as " + BroadBandTestConstants.STRING_TEST_1);

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_NAME,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_TEST_1);
	    LOGGER.info("Is Current SSID for public 2.4 Ghz modified - " + status);

	    if (status) {
		String response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_ADV_ENABLED);

		if (CommonMethods.isNotNull(response)) {
		    public2_4GhzSsidAdvertized = Boolean.parseBoolean(response);
		    if (!public2_4GhzSsidAdvertized) {
			errorMessage = errorMessage
				+ (CommonMethods.isNotNull(errorMessage) ? BroadBandTestConstants.CHAR_NEW_LINE
					: BroadBandTestConstants.EMPTY_STRING)
				+ "Public 2.4 GHz SSID advertisement (Device.WiFi.AccessPoint.10105.SSIDAdvertisementEnabled) is in disabled state";
		    }
		} else {
		    errorMessage = errorMessage
			    + (CommonMethods.isNotNull(errorMessage) ? BroadBandTestConstants.CHAR_NEW_LINE
				    : BroadBandTestConstants.EMPTY_STRING)
			    + "Unable to retrieve the Public 2.4 GHz SSID advertisement enabled status (Device.WiFi.AccessPoint.10105.SSIDAdvertisementEnabled) via WebPA/DMCLI";
		}

	    } else {
		errorMessage = ("Unable to retrieve the Public 2.4 GHz SSID name (Device.WiFi.SSID.10105.SSID) via WebPA/DMCLI");
	    }

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Set new value for public 2.4 GHz SSID and Validated SSID of public 2.4 GHz network is advertised");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, false);
	}

    }

    /**
     * Validating SSID of public 5 GHz SSID is advertised
     * <ol>
     * <li>PRE-CONDITION :Check whether WebPA is Up and Running</li>
     * <li>STEP 1: Validating SSID of Public 5 GHz network is advertised</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @Refactor Athira
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-6024")
    public void webpaValidatePublic5GHznetworkAdvertised(Dut device) {

	boolean status = false;
	String testId = "TC-RDKB-WEBPA-624";
	String testStep = null;
	String errorMessage = null;
	String testStepNumber = "s1";
	boolean public5GhzSsidAdvertized = false;
	try {

	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WEBPA-6024 ####################");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("TEST DESCRIPTION: Validating SSID of public 5 GHz network is advertised");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("PRE-CONDITION : Check whether Webpa is Up and Running");
	    LOGGER.info("STEP 1: Validating SSID of public 5 GHz network is advertised");
	    LOGGER.info("##########################  STARTING PRE-CONFIGURATIONS ##########################");
	    LOGGER.info("PRECONDITION: DESCRIPTION: VERIFY WHETHER WEBPA IS UP AND RUNNING");
	    LOGGER.info(
		    "PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
	    LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    errorMessage = "WEBPA PROCESS IS NOT UP AND RUNNING";
	    if (!status) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    } else {
		LOGGER.info("WEBPA PROCESS IS UP AND RUNNING, PROCEEDING FOR TEST!");
	    }
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("#########################  COMPLETED PRE-CONFIGURATIONS #########################");

	    LOGGER.info("**********************************************************************************");

	    testStep = "s1";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: Validating SSID of public 5 GHz network is advertised");
	    LOGGER.info(
		    "STEP 1: ACTION :Set a new value for SSID and get the same value and Validating SSID of public 5.0 GHz network is advertised");
	    LOGGER.info("STEP 1: EXPECTED: Set a new value for SSID and get the same value and SSID is advertised");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to set public 5 Ghz SSID name using WebPA parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_NAME;
	    // Set Current public 5Ghz SSID name, other than default value
	    LOGGER.info("Modify Current SSID name of 5 Ghz as " + BroadBandTestConstants.STRING_TEST_1);

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_NAME,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_TEST_1);
	    LOGGER.info("Is Current SSID for public 5 Ghz modified - " + status);

	    if (status) {
		String response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_ADV_ENABLED);

		if (CommonMethods.isNotNull(response)) {
		    public5GhzSsidAdvertized = Boolean.parseBoolean(response);
		    if (!public5GhzSsidAdvertized) {
			errorMessage = errorMessage
				+ (CommonMethods.isNotNull(errorMessage) ? BroadBandTestConstants.CHAR_NEW_LINE
					: BroadBandTestConstants.EMPTY_STRING)
				+ "Public 5 GHz SSID advertisement (Device.WiFi.AccessPoint.10105.SSIDAdvertisementEnabled) is in disabled state";
		    }
		} else {
		    errorMessage = errorMessage
			    + (CommonMethods.isNotNull(errorMessage) ? BroadBandTestConstants.CHAR_NEW_LINE
				    : BroadBandTestConstants.EMPTY_STRING)
			    + "Unable to retrieve the Public 5 GHz SSID advertisement enabled status (Device.WiFi.AccessPoint.10105.SSIDAdvertisementEnabled) via WebPA/DMCLI";
		}

	    } else {
		errorMessage = ("Unable to retrieve the Public 5 GHz SSID name (Device.WiFi.SSID.10105.SSID) via WebPA/DMCLI");
	    }

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Set new value for public 5.0 GHz SSID and Validated SSID of public 5.0 GHz network is advertised");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, false);
	}

    }

    /**
     * Verify Webpa requests (GET/SET/PUT/POST/DELETE) are working fine and Webpa notifications should be sent
     * successfully to server
     * <ol>
     * <li>Verify the WebPA GET command can be executed successfully</li>
     * <li>Verify WEBPA GET request log message is present in WEBPAlog.txt.0</li>
     * <li>Verify the WebPA SET command can be executed successfully</li>
     * <li>Verify WEBPA SET request log message is present in WEBPAlog.txt.0</li>
     * <li>Verify the WEBPA POST command can be executed successfully</li>
     * <li>Verify WEBPA POST request log message is present in WEBPAlog.txt.0</li>
     * <li>Verify the WEBPA PUT command can be executed successfully</li>
     * <li>Verify WEBPA PUT request log message is present in WEBPAlog.txt.0</li>
     * <li>Verify the WEBPA DELETE command can be executed successfully</li>
     * <li>Verify WEBPA DELETE request log message is present in WEBPAlog.txt.0</li>
     * </ol>
     * 
     * @author Joseph Maduram
     * @param device
     * @Refactor Sruthi Santhosh
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-1013")
    public void verifyWebpaNotificationToServer(Dut device) {
	// String to store the test case ID
	String testId = "TC-RDKB-WEBPA-113";
	// String to store the test step number
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// stores the test status
	boolean status = false;
	// stores the webPaServer Response
	WebPaServerResponse webPaServerResponse = null;
	// stores the current device timestamp
	String currentDeviceTimeStamp = null;
	try {
	    LOGGER.info("STARTING TESTCASE :verifyWebpaNotificationToServer()");
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION-1: DESCRIPTION : Set and verify the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	    LOGGER.info(
		    "PRE-CONDITION-1: ACTION : Set the Global DNS IPv4 value  using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	    LOGGER.info(
		    "PRE-CONDITION-1: EXPECTED : Global DNS IPv4 value should be set to using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	    LOGGER.info("#######################################################################################");
	    status = false;
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, WebPaDataTypes.STRING.getValue(),
		    AutomaticsPropertyUtility
			.getProperty(BroadBandPropertyKeyConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE), BroadBandTestConstants.THREE_MINUTES,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    // Error message
	    errorMessage = "Failed to set Global DNS IPv4 value to using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'";
	    LOGGER.info(
		    "PRE-CONDITION-1: ACTUAL: " + (status ? "Global DNS IPv4 value sucessfully set " : errorMessage));

	    if (!status) {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-1 FAILED : " + errorMessage);
	    }
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION-2: DESCRIPTION : Set and verify the Global DNS IPv6 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	    LOGGER.info(
		    "PRE-CONDITION-2: ACTION : Set the Global DNS IPv6 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	    LOGGER.info(
		    "PRE-CONDITION-2: EXPECTED : Global DNS IPv6 value should be set to  using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	    LOGGER.info("#######################################################################################");
	    status = false;
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6, WebPaDataTypes.STRING.getValue(),
		    BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV6_VALUE, BroadBandTestConstants.THREE_MINUTES,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    // Error message
	    errorMessage = "Failed to set Global DNS IPv6 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'";
	    LOGGER.info(
		    "PRE-CONDITION-2: ACTUAL: " + (status ? "Global DNS IPv6 value sucessfully set " : errorMessage));
	    if (!status) {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-2 FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1013");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify Webpa requests (GET/SET/PUT/POST/DELETE) are working fine and Webpa notifications should be sent successfully to server");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1:Verify the WebPA GET command can be executed successfully
	     * 
	     */
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 1:DESCRIPTION: Verify the WebPA GET command can be executed successfully");
	    LOGGER.info(
		    "STEP 1:ACTION: Execute the TR-181 parameter-Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS to retrieve status of XDNS by WebPA");
	    LOGGER.info("STEP 1:EXPECTED: WEBPA request should respond with success message and status code 200");
	    LOGGER.info("#####################################################################################");
	    currentDeviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    errorMessage = "Unable to retrieve the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' using WebPA command.";
	    String xDNSStatus = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS);
	    LOGGER.info("Xdns status retrieved using WebPa = " + xDNSStatus);
	    status = CommonMethods.isNotNull(xDNSStatus);
	    if (status) {
		LOGGER.info(
			"S1 ACTUAL: Successfully retrieved  the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' using WebPA command.");
	    } else {
		LOGGER.error("S1 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 2: Verify WEBPA GET request log message is present in WEBPAlog.txt.0
	     *
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify WEBPA GET request log message is present in WEBPAlog.txt.0 ");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute command: 1. grep -i \"WDMP-C: Request\" /rdklogs/logs/Consolelog.txt.0");
	    LOGGER.info("STEP 2: EXPECTED : Webpa Log messages should present in WEBPAlog.txt.0");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Failed to get the  log message under /rdklogs/logs/WEBPAlog.txt.0 ";
	    status = BroadBandSystemUtils.verifyWebpaNotificationForPollingTime(tapEnv, device,
		    BroadBandTraceConstants.WEBPA_GET_NOTIFICATION, currentDeviceTimeStamp);
	    if (status) {
		LOGGER.info("S2 ACTUAL: WEBPA GET request log message is present in WEBPAlog.txt.0");
	    } else {
		LOGGER.error("S2 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * ` STEP 3:Verify the WebPA SET command can be executed successfully
	     * 
	     */
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 3:DESCRIPTION: Verify the WebPA SET command can be executed successfully");
	    LOGGER.info(
		    "STEP 3:ACTION: Execute the TR-181 parameter-Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS to set the  status of XDNS as 'true' by WebPA");
	    LOGGER.info("STEP 3:EXPECTED: WEBPA request should respond with success message and status code 200");
	    LOGGER.info("#####################################################################################");
	    errorMessage = "Unable to set the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' as 'true'using WebPA command..";
	    currentDeviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    webPaServerResponse = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.CONSTANT_3);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info(
			"S3 ACTUAL: Successfully able to set the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' as 'true' using WebPA command.");
	    } else {
		LOGGER.error("S3 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 4: Verify WEBPA SET request log message is present in WEBPAlog.txt.0
	     *
	     */
	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify WEBPA SET request log message is present in WEBPAlog.txt.0 ");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute command: 1. grep -i \"WDMP-C: SET Request\" /rdklogs/logs/Consolelog.txt.0");
	    LOGGER.info("STEP 4: EXPECTED : Log message should present in WEBPAlog.txt.0");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Failed to get the  log message under /rdklogs/logs/WEBPAlog.txt.0 ";
	    status = BroadBandSystemUtils.verifyWebpaNotificationForPollingTime(tapEnv, device,
		    BroadBandTraceConstants.WEBPA_SET_NOTIFICATION, currentDeviceTimeStamp);
	    if (status) {
		LOGGER.info("S4 ACTUAL: WEBPA SET request log message is present in WEBPAlog.txt.0");
	    } else {
		LOGGER.error("S4 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 5:Verify the WEBPA PUT command can be executed successfully
	     * 
	     * 
	     */
	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 5:DESCRIPTION: Verify the WEBPA PUT command can be executed successfully");
	    LOGGER.info(
		    "STEP 5:ACTION: Execute the TR-181 parameter-Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS to put the table entry in XDNS by WebPA");
	    LOGGER.info("STEP 5:EXPECTED: should be able to put the table entry in XDNS table by WebPA");
	    LOGGER.info("#####################################################################################");
	    // Instance to store webPaServerResponse
	    currentDeviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    webPaServerResponse = BroadBandWebPaUtils.invokeRestCallToXDNS(tapEnv, device,
		    BroadBandTestConstants.STRING_PUT);
	    if (webPaServerResponse != null) {
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    }
	    errorMessage = "Unable to put the  table entry using the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'  using WebPA command.";
	    if (status) {
		LOGGER.info(
			"S5 ACTUAL: Successfully able to put the table entry using the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'' using WebPA command.");
	    } else {
		LOGGER.error("S5 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 6: Verify WEBPA PUT request log message is present in WEBPAlog.txt.0
	     *
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify WEBPA PUT request log message is present in WEBPAlog.txt.0 ");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute command: 1. grep -i \"WDMP-C: REPLACE_ROWS Request\" /rdklogs/logs/Consolelog.txt.0");
	    LOGGER.info("STEP 6: EXPECTED : Log message should present in WEBPAlog.txt.0");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Failed to get the  log message under /rdklogs/logs/WEBPAlog.txt.0 ";
	    status = BroadBandSystemUtils.verifyWebpaNotificationForPollingTime(tapEnv, device,
		    BroadBandTraceConstants.WEBPA_PUT_NOTIFICATION, currentDeviceTimeStamp);
	    if (status) {
		LOGGER.info("S6 ACTUAL: WEBPA PUT Log message should present in WEBPAlog.txt.0");
	    } else {
		LOGGER.error("S6 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * STEP 7:Verify the WEBPA POST command can be executed successfully
	     * 
	     * 
	     */
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 7:DESCRIPTION :Verify the WEBPA POST command can be executed successfully");
	    LOGGER.info(
		    "STEP 7:ACTION: Execute the TR-181 parameter-Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS to post the table entry in XDNS by WebPA");
	    LOGGER.info("STEP 7:EXPECTED: should be able to post the table entry in XDNS table by WebPA");
	    LOGGER.info("#####################################################################################");
	    currentDeviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    webPaServerResponse = BroadBandWebPaUtils.invokeRestCallToXDNS(tapEnv, device,
		    BroadBandTestConstants.STRING_POST);
	    LOGGER.info("webPaServerResponse is" + webPaServerResponse.getMessage());
	    String tableRowNumber = webPaServerResponse.getRow();
	    LOGGER.info("tableRowNumber is" + tableRowNumber);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    errorMessage = "Unable to post the  the table entry using the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'' using WebPA command.";
	    if (status) {
		LOGGER.info(
			"S7 ACTUAL: Successfully able to post the table entry using the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'' using WebPA command.");
	    } else {
		LOGGER.error("S7 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 8: Verify WEBPA POST request log message is present in WEBPAlog.txt.0
	     *
	     */
	    testStepNumber = "s8";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify WEBPA POST request log message is present in WEBPAlog.txt.0 ");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute command: 1. grep -i \"WDMP-C: ADD_ROW Request\" /rdklogs/logs/Consolelog.txt.0");
	    LOGGER.info("STEP 8: EXPECTED : Log message should present in WEBPAlog.txt.0");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Failed to get the  log message under /rdklogs/logs/WEBPAlog.txt.0 ";
	    status = BroadBandSystemUtils.verifyWebpaNotificationForPollingTime(tapEnv, device,
		    BroadBandTraceConstants.WEBPA_POST_NOTIFICATION, currentDeviceTimeStamp);
	    if (status) {
		LOGGER.info("S8 ACTUAL: WEBPA POST Log message should present in WEBPAlog.txt.0");
	    } else {
		LOGGER.error("S8 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 9: Verify the WEBPA DELETE command can be executed successfully
	     *
	     */
	    testStepNumber = "s9";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 9: DESCRIPTION: Verify the WEBPA DELETE command can be executed successfully");
	    LOGGER.info(
		    "STEP 9:ACTION: Execute the TR-181 parameter-Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS to delete the table entry in XDNS by WebPA");
	    LOGGER.info("STEP 9: EXPECTED: should be able to delete the table entry in XDNS table by WebPA");
	    LOGGER.info("#####################################################################################");
	    currentDeviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    errorMessage = "Null response obtained for using delete by webpa";
	    WebPaServerResponse deleteResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRowNumber);
	    if (CommonMethods.isNotNull(deleteResponse.getMessage())) {
		status = deleteResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		errorMessage = "Unable to delete the table row using webpa";
	    }
	    if (status) {
		LOGGER.info("S9 ACTUAL: table row deleted succesfully using webpa");
	    } else {
		LOGGER.error("S9 ACTUAL: " + errorMessage + " ACTUAL RESPONSE: " + deleteResponse.getMessage());
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 10: Verify WEBPA DELETE request log message is present in WEBPAlog.txt.0
	     *
	     */
	    testStepNumber = "s10";
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify WEBPA DELETE request log message is present in WEBPAlog.txt.0 ");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute command: 1. grep -i \"WDMP-C: DELETE_ROW Request\" /rdklogs/logs/Consolelog.txt.0");
	    LOGGER.info("STEP 10: EXPECTED : Log message should present in WEBPAlog.txt.0");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Failed to get the  log message under /rdklogs/logs/WEBPAlog.txt.0 ";
	    status = BroadBandSystemUtils.verifyWebpaNotificationForPollingTime(tapEnv, device,
		    BroadBandTraceConstants.WEBPA_DELETE_NOTIFICATION, currentDeviceTimeStamp);
	    if (status) {
		LOGGER.info("S10 ACTUAL: WEBPA DELETE Log message should present in WEBPAlog.txt.0");
	    } else {
		LOGGER.error("S10 ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured during execution !!!!" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("POST-CONDITION 1:DESCRIPTION :Verify the WEBPA Set command is executed for default value.");
	    LOGGER.info("POST-CONDITION 1:ACTION : Verify the WEBPA Set command is executed for default value");
	    LOGGER.info("POST-CONDITION 1:EXPECTED: Should be able to WEBPA Set the default value");
	    LOGGER.info("#####################################################################################");
	    webPaServerResponse = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.CONSTANT_3);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("WEBPA Set to the default value as 'false'");
	    } else {
		LOGGER.error("WEBPA Set to the default value as 'false' Failed");
	    }
	}
	LOGGER.info("ENDING TESTCASE :verifyWebpaNotificationToServer()");
    }

    /**
     * Test to Validate traceroute from the device with Ipv4 address
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>1:Retrieve IPv4 address with nslookup and save</li>
     * <li>2:Set Host address as IPv4 address using Webpa</li>
     * <li>3:Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Complete' webpa
     * response</li>
     * <li>4:Get Traceroute hops using Webpa command and validate hostaddress as IPv4 address</li>
     * <li>POST-CONDITION 1: Set Traceroute Host Address To Null</li>
     * </ol>
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-TRACE-ROUTE-1001", testDecription = "Test to Validate traceroute from the device with Ipv4 address")
    public void testToVerifyTraceRouteFromDeviceIpv4(Dut device) {
	boolean status = false;// String to store the test case status
	String testId = "TC-RDKB-TRACE-ROUTE-101";// Test case id
	String testStep = null;// Test step number
	String errorMessage = null;// String to store the error message
	String nslookupIPv4Addr = null;// String to store IPv4
	BroadBandResultObject bandResultObject = new BroadBandResultObject();
	try {
	    LOGGER.info("#################### STARTING TEST CASE:TC-RDKB-TRACE-ROUTE-1001 #####################");
	    LOGGER.info("TEST DESCRIPTION: Test to Validate traceroute from the device with Ipv4 address");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("1.Retrieve IPv4 address with nslookup and save");
	    LOGGER.info("2.Set Host address as IPv4 address using Webpa");
	    LOGGER.info(
		    "3.Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Complete' webpa response");
	    LOGGER.info("4.Get Traceroute hops using Webpa command and validate hostaddress as IPv4 address");
	    LOGGER.info("POST-CONDITION 1: Set Traceroute Host Address To Null");
	    LOGGER.info("#####################################################################################");

	    /**
	     * STEP 1:Retrieve IPv4 address with nslookup and save
	     */
	    status = false;
	    String response = null;
	    testStep = "s1";
	    LOGGER.info(
		    "*********************************************************************************************");
	    LOGGER.info("STEP 1 : DESCRIPTION :Retrieve IPv4 address with nslookup and save");
	    LOGGER.info("STEP 1 : ACTION :Execute command nslookup to  facebook.com");
	    LOGGER.info("STEP 1 : EXPECTED:IPv4 address should be retrieved successfully and saved");
	    LOGGER.info(
		    "*********************************************************************************************");
	    errorMessage = "Unable to retrieve IPv4 address from nslookup response";
	    try {
		response = tapEnv.executeCommandUsingSshConnection(
			WhiteListServer.getInstance(tapEnv, BroadbandPropertyFileHandler.getReverseSshJumpServer()),
			BroadBandCommonUtils.concatStringUsingStringBuffer(
				BroadBandCommandConstants.CMD_NSLOOKUP_WITH_PATH_FOR_IPV4_ADDRESS,
				BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK));
		if (CommonMethods.isNotNull(response)) {
		    nslookupIPv4Addr = BroadBandCommonUtils.patternFinderForMultipleMatches(response,
			    BroadBandTestConstants.PATTERN_TO_RETRIEVE_IPV4_ADDRESS_FROM_NSLOOKUP_FACEBOOK,
			    BroadBandTestConstants.CONSTANT_1).get(0);
		    status = CommonMethods.isNotNull(nslookupIPv4Addr) && CommonMethods.isIpv4Address(nslookupIPv4Addr);
		}
	    } catch (Exception e) {
		LOGGER.error("EXCEPTION OCCURED WHILE RETRIEVING IPV4 ADDRESS FROM NS LOOKUP :\n" + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 1:ACTUAL :IPv4 Address are retrieved successfully from nslookup response");
	    } else {
		LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("********************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

	    /**
	     * STEP 2:Set Host address as IPv4 address using Webpa
	     */
	    status = false;
	    testStep = "s2";
	    LOGGER.info("********************************************************************************************");
	    LOGGER.info("STEP 2 : DESCRIPTION :Set Host address as IPv4 address retrieved from nslookup using Webpa");
	    LOGGER.info(
		    "STEP 2 : ACTION :Execute Webpa SET command Device.IP.Diagnostics.TraceRoute.Host IPv4 address as value");
	    LOGGER.info("STEP 2 : EXPECTED:Webpa should be success with Success 200 status code");
	    LOGGER.info(
		    "*********************************************************************************************");
	    errorMessage = "Unable to set IP address to traceroute host using webpa";
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_HOST, BroadBandTestConstants.CONSTANT_0,
		    nslookupIPv4Addr);
	    if (status) {
		LOGGER.info("STEP 2:ACTUAL :Webpa Set for Host address is successful");
	    } else {
		LOGGER.error("STEP 2:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("********************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

	    /**
	     * STEP 3:Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Complete' webpa
	     * response
	     */
	    status = false;
	    testStep = "s3";
	    LOGGER.info(
		    "*********************************************************************************************");
	    LOGGER.info(
		    "STEP 3 : DESCRIPTION :Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Complete' webpa response");
	    LOGGER.info(
		    "STEP 3 : ACTION :Execute Webpa get command Device.IP.Diagnostics.TraceRoute.DiagnosticsState Value:Requested");
	    LOGGER.info("STEP 3 : EXPECTED:Webpa should be success with Success 200 status code");
	    LOGGER.info(
		    "*********************************************************************************************");
	    errorMessage = "Unable to set and Validate webpa request for traceroute diagnosticState";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDurationForPassedValue(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_DIAGNOSTIC_STATE, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.STRING_REQUESTED, BroadBandTestConstants.STRING_COMPLETE,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP 3:ACTUAL :Diagnostic state set sucessfully and verified with 'Complete' response");
	    } else {
		LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info(
		    "**********************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	    /**
	     * STEP 4:Get Trace route hops using Webpa command and validate host address as IPv4 address
	     */
	    status = false;
	    testStep = "s4";
	    LOGGER.info(
		    "**********************************************************************************************");
	    LOGGER.info(
		    "STEP 4 : DESCRIPTION :Get Traceroute hops using Webpa command and validate hostaddress as IPv4 address");
	    LOGGER.info("STEP 4 : ACTION :Execute Webpa GET command:Device.IP.Diagnostics.TraceRoute.RouteHops.");
	    LOGGER.info("STEP 4 : EXPECTED:Webpa get request should be successful");
	    LOGGER.info(
		    "*********************************************************************************************");
	    errorMessage = "Unable to Validate traceroute from webpa request";
	    bandResultObject = BroadBandWebPaUtils.validateTraceRouteOutput(device, tapEnv, false);
	    errorMessage += bandResultObject.getErrorMessage();
	    status = bandResultObject.isStatus();
	    if (status) {
		LOGGER.info("STEP 4:ACTUAL :Traceroute hops validated successfully with Ipv4 address");
	    } else {
		LOGGER.error("STEP 4:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info(
		    "*********************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Execution error in verifying traceroute " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
	} finally {
	    errorMessage = "UNABLE TO RESTORE THE TRACEROUTE HOST ADDRESS";
	    status = false;
	    LOGGER.info("###############################STARTING POST-CONFIGURATIONS###############################");
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION:SET TRACEROUTE HOST ADDRESS TO NULL");
	    LOGGER.info("POST CONDITION 1: ACTION:EXECUTE WEBPA COMMAND PARAMETER:"
		    + BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_HOST + " with Value '"
		    + BroadBandTestConstants.STRING_NULL + "'");
	    LOGGER.info("POST-CONDITION 1: EXPECTED:TRACEROUTE HOST ADDRESS SHOULD BE RESTORED SUCCESSFULLY");
	    LOGGER.info("##########################################################################");
	    try {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_HOST, BroadBandTestConstants.CONSTANT_0,
			BroadBandTestConstants.STRING_NULL);
		if (status) {
		    LOGGER.info("POST-CONDITION 1 : ACTUAL : SUCCESSFULLY RESTORED THE TRACEROUTE HOST ADDRESS");
		} else {
		    LOGGER.error("POST-CONDITION 1 : ACTUAL :" + errorMessage);
		}
	    } catch (Exception e) {
		LOGGER.error("EXCEPTION OCCURED WHILE CONFIGURING POST CONDITION 1:\n" + e.getMessage());
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-TRACE-ROUTE-1001");
    }

    /**
     * 
     * <ol>
     * <li>1:Retrieve IPv6 address with nslookup and save</li>
     * <li>2:Set Host address as IPv6 address using Webpa</li>
     * <li>3:Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Completed' webpa
     * response</li>
     * <li>4:Get Traceroute hops using Webpa command and validate hostaddress as IPv6 address</li>
     * </ol>
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-TRACE-ROUTE-1002", testDecription = "Test to Validate traceroute from the device with IPv6 address")
    public void testToVerifyTraceRouteFromDeviceForIpv6(Dut device) {
	boolean status = false;// String to store the test case status
	String testId = "TC-RDKB-TRACE-ROUTE-102";// Test case id
	String testStep = null;// Test step number
	String errorMessage = null;// String to store the error message
	String nslookupIPv6Addr = null;// String to store IPv6
	BroadBandResultObject bandResultObject = new BroadBandResultObject();
	try {
	    LOGGER.info("#################### STARTING TEST CASE:TC-RDKB-TRACE-ROUTE-1002 #####################");
	    LOGGER.info("TEST DESCRIPTION: Test to Validate traceroute from the device with IPv6 address");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("1.Retrieve IPv6 address with nslookup and save");
	    LOGGER.info("2.Set Host address as IPv6 address using Webpa");
	    LOGGER.info(
		    "3.Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Completed' webpa response");
	    LOGGER.info("4.Get Traceroute hops using Webpa command and validate hostaddress as IPv6 address");
	    LOGGER.info("#####################################################################################");

	    /**
	     * STEP 1:Retrieve IPv4 and IPv6 address with nslookup and save
	     */
	    status = false;
	    testStep = "s1";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 1 : DESCRIPTION :Retrieve IPv6 address with nslookup and save");
	    LOGGER.info("STEP 1 : ACTION :Execute command nslookup to  facebook.com");
	    LOGGER.info("STEP 1 : EXPECTED:IPv6 address should be retrieved successfully and saved");
	    errorMessage = "Unable to retrieve IPv6 address from nslookup response";
	    nslookupIPv6Addr = BroadBandCommonUtils.executeCommandOnJumpServerAndRetrievIPAddressWithPatternSearch(
		    tapEnv,
		    CommonUtils.concatStringUsingStringBuffer(
			    BroadBandCommandConstants.CMD_NSLOOKUP_WITH_PATH_FOR_IPV6_ADDRESS,
			    BroadBandTestConstants.SSID_NAME_WITH_WHITSPACE_ONLY,
			    BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK),
		    true, BroadBandTestConstants.PATTERN_TO_RETRIVE_IPV6_ADDRESS_FROM_NSLOOKUP_FACEBOOK,
		    BroadBandTestConstants.CONSTANT_1);

	    if (CommonMethods.isNotNull(nslookupIPv6Addr) && CommonMethods.isIpv6Address(nslookupIPv6Addr)) {
		status = true;
		LOGGER.info("STEP 1:ACTUAL :IPv6 Address is retrieved successfully from nslookup response");
	    } else {
		LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

	    /**
	     * STEP 2:Set Host address as IPv6 address using Webpa
	     */
	    status = false;
	    testStep = "s2";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 2 : DESCRIPTION :Set Host address as IPv6 address retrieved from nslookup using Webpa");
	    LOGGER.info(
		    "STEP 2 : ACTION :Execute webpa command Device.IP.Diagnostics.TraceRoute.DiagnosticsState value Ipv6 address");
	    LOGGER.info("STEP 2 : EXPECTED:Webpa should be success with Success 200 status code");
	    errorMessage = "Unable to set IP address to traceroute host using webpa";
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_HOST, BroadBandTestConstants.CONSTANT_0,
		    nslookupIPv6Addr);
	    if (status) {
		LOGGER.info("STEP 2:ACTUAL :Webpa set IPv6 address to Host address is successful");
	    } else {
		LOGGER.error("STEP 2:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	    /**
	     * STEP 3:Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Completed' webpa
	     * response
	     */
	    status = false;
	    testStep = "s3";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 3 : DESCRIPTION :Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Completed' webpa response");
	    LOGGER.info(
		    "STEP 3 : ACTION :Execute Webpa SET and GET:Device.IP.Diagnostics.TraceRoute.DiagnosticsState with value:Requested");
	    LOGGER.info(
		    "STEP 3 : EXPECTED:Webpa should be success with Success 200 status code and Get request should be validated successfully");
	    errorMessage = "Unable to set and Validate webpa request for traceroute diagnosticState";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDurationForPassedValue(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_DIAGNOSTIC_STATE, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.STRING_REQUESTED, BroadBandTestConstants.STRING_COMPLETE,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP 3:ACTUAL :Diagnostic state is set and validated successfully");
	    } else {
		LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	    /**
	     * STEP 4:Get Traceroute hops using Webpa command and validate hostaddress as IPv6 address
	     */
	    status = false;
	    testStep = "s4";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 4 : DESCRIPTION :Get Traceroute hops using Webpa command and validate hostaddress as IPv6 address");
	    LOGGER.info("STEP 4 : ACTION :Execute Webpa get command:evice.IP.Diagnostics.TraceRoute.RouteHops.");
	    LOGGER.info("STEP 4 : EXPECTED:Webpa set request should be successful");
	    errorMessage = "Unable to Validate traceroute from webpa request";
	    bandResultObject = BroadBandWebPaUtils.validateTraceRouteOutput(device, tapEnv, true);
	    if (bandResultObject.isStatus()) {
		status = true;
		LOGGER.info("STEP 4:ACTUAL :Traceroute hops is validated with IPv6 address");
	    } else {
		LOGGER.error("STEP 4:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Execution error in verifying traceroute " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
	} finally {
	    LOGGER.info("###############################STARTING POST-CONFIGURATIONS###############################");
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION:SET TRACEROUTE HOST ADDRESS TO NULL");
	    LOGGER.info(
		    "POST CONDITION 1: ACTION:EXECUTE WEBPA COMMAND PARAMETER:Device.IP.Diagnostics.TraceRoute.Host with Value 'null'");
	    LOGGER.info("POST-CONDITION 1: EXPECTED:TRACEROUTE HOST ADDRESS SHOULD BE RESTORED SUCCESSFULLY");
	    LOGGER.info("##########################################################################");
	    status = false;
	    try {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_HOST, BroadBandTestConstants.CONSTANT_0,
			BroadBandTestConstants.STRING_NULL);
		LOGGER.info("POST CONDITION 1:ACTUAL:TRACEROUTE RESTORATION STATUS: " + status);
	    } catch (Exception e) {
		LOGGER.error("EXCEPTION OCCURED WHILE CONFIGURING POST CONDITION 1:\n" + e.getMessage());
	    }

	}

    }

    /**
     * Verify all reports for Harvester and LMLite are enabled and Verify CcspWebpaadapter responds to WEBPA GET, SET
     * requests.
     * <ol>
     * <li>PRE CONDITION 1:Reboot the device</li>
     * <li>PRE CONDITION 2:Verify whether WebPA is Up and Running in the Device.</li>
     * <li>Verify WEBPAlog.txt.0 Logs for "Component caching is completed" message in the Atom Console.</li>
     * <li>Verify parodus process is running on Arm side.</li>
     * <li>Verify the Current Value for Network Device Status via WEB Pa Commands.</li>
     * <li>Verify the Current Value for Network Device Traffic via WEB Pa Commands.</li>
     * <li>Verify retrieving current value for the Interface Devices Wifi Via WEBPA Command.</li>
     * <li>Verify Enabling the Network Device Status to True Via WEBPA Command.</li>
     * <li>Verify Enabling the Network Device Traffic to True Via WEBPA Command.</li>
     * <li>Verify enabling the "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled" to true using webpa command</li>
     * <li>Verify the Current Value for "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled" via WEB Pa Commands.</li>
     * <li>Set value to parameter "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod" using WEBpa</li>
     * <li>Get value to parameter "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod" using WEBpa</li>
     * <li>Set value to parameter "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod" using WEBpa</li>
     * <li>Get value to parameter "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod" using WEBpa</li>
     * <li>Verify the Process harvestor.txt is running in atom and arm console.</li>
     * <li>Verify Harvester.txt log contains the required string.</li>
     * <li>Verify Enabling the Interface Devices Wifi to True Via WEBPA Command.</li>
     * <li>Verify the Process lmlite.txt is running in atom and arm console.</li>
     * <li>POST CONDITION 1: Revert the Network Device Status,Network Device Traffic and Interface Device WiFi to its
     * default Values.</li>
     * </ol>
     * 
     * @author Prashant Mishra
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-WEBPA-1008")
    public void ToVerifyHarvesterLMLiteAndCcspWebpaAdapter(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBPA-108";
	String stepNum = "s1";
	String errorMessage = "";
	String response = "";
	boolean status = false;
	boolean isAtomSyncAvailable = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1008");
	LOGGER.info(
		"TEST DESCRIPTION: Verify all reports for Harvester and LMLite are enabled and Verify CcspWebpaadapter responds to WEBPA GET, SET requests.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE CONDITION 1. Reboot the device");
	LOGGER.info("PRE CONDITION 2. Verify whether WebPA is Up and Running in the Device.");
	LOGGER.info(
		"1. Verify WEBPAlog.txt.0 Logs for \"Component caching is completed\" message in the Atom and Arm Console.");
	LOGGER.info("2. Verify parodus process is running on ARM side.");
	LOGGER.info("3. Verify the Current Value for Network Device Status via WEB Pa Commands.");
	LOGGER.info("4. Verify the Current Value for Netwrok Device Traffic via WEB Pa Commands.");
	LOGGER.info("5. Verify retrieving current value for  the Interface Devices Wifi Via WEBPA Command.");
	LOGGER.info("6. Verify Enabling the Network Device Status to True Via WEBPA Command.");
	LOGGER.info("7. Verify Enabling the Network Device Traffic to True Via WEBPA Command.");
	LOGGER.info(
		"8. Verify enabling the \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" to true using webpa command");
	LOGGER.info(
		"9. Verify the Current Value for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" via WEB Pa Commands.");
	LOGGER.info(
		"10. Set value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\" using WEBpa");
	LOGGER.info(
		"11. Get value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\" using WEBpa");
	LOGGER.info(
		"12. Set value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\" using WEBpa");
	LOGGER.info(
		"13. Get value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\" using WEBpa");
	LOGGER.info("14. Verify the Process harvestor.txt is running in atom and arm console.");
	LOGGER.info("15. Verify Harvester.txt log conatins the required string.");
	LOGGER.info("16. Verify Enabling the Interface Devices Wifi to True Via WEBPA Command");
	LOGGER.info("17. Verify the Process lmlite.txt is running in atom and arm console.");
	LOGGER.info(
		"POST CONDITION 1. Revert the Network Device Status,Network Device Traffic,NeighboringAP and Interface Device WiFi to its default Values.");

	LOGGER.info("#######################################################################################");
	;
	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");

	    LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Reboot the device.");
	    LOGGER.info("PRE-CONDITION 1 : ACTION : Reboot the device using the reboot command");
	    LOGGER.info("PRE-CONDITION 1 : EXPECTED : Reboot of the device should be successful.");
	    errorMessage = "Unable to Reboot Device.";
	    try {
		status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 1 : ACTUAL : Device Rebooted successfully.");
	    } else {
		LOGGER.error("PRE-CONDITION 1: ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : Verify whether WebPA is Up and Running in the Device.");
	    LOGGER.info(
		    "PRE-CONDITION 2 : ACTION : Verifying Successful webpa Get response ,in case of failure rechecking for 8 minutes.");
	    LOGGER.info("PRE-CONDITION 2 : EXPECTED : WebPA should be Up and Running in the Device.");
	    errorMessage = "WebPA is not Up and not Running in Device.";
	    try {
		status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 2 : ACTUAL :  WebPA is Up and Running in the Device.");
	    } else {
		LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "WEBPAlog.txt.0 log file doesnï¿½t contain the required String : \"Component caching is completed\"";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION :Verify WEBPAlog.txt.0 Logs for \"Component caching is completed\" message in the Atom/Arm Console.");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute command: ssh <atom ip> and then executetail -f rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info(
		    "STEP 1: EXPECTED : Webpa should wait for system ready signal to process component caching.  webpa should proceed with caching within 7 mins after getting system ready signal.  \"Checked CR - System is ready, proceed with component caching\" & \"Component caching is completed. Hence setting cachingStatus to active\"should be present in the WEBPAlog.txt.0 log file");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.LOG_MESSAGE_COMP_CACHING, BroadBandCommandConstants.LOG_FILE_WEBPA,
		    BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS);
	    status = CommonMethods.isNotNull(response);
	    if (!status) {
		try {
		    status = CommonUtils.validateTraceLog(tapEnv, device,
			    BroadBandTraceConstants.WEBPA_LOG_MESSAGE_COMP_CACHING_TRACE,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, true);
		} catch (Exception e) {
		    LOGGER.error("Exception occured while validating device trace " + e.getMessage());
		}
	    }
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : WEBPAlog.txt.0 log file contains the required String: \"Component caching is completed\"");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S2";
	    errorMessage = "Parodus Process details is not displayed as (/usr/bin/parodus). ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify parodus process is running on ARM console.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the below command in ARM console: Command: root@Docsis-Gateway:/rdklogs/logs # pidof process ");
	    LOGGER.info("STEP 2: EXPECTED : Parodus process details should be displayed");
	    LOGGER.info("**********************************************************************************");

	    response = CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_PARODUS);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : parodus process is running on ARM side.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S3";
	    errorMessage = "Unable to retrieve the status of 'Device.X_RDKCENTRAL-COM_Report.NetworkDevicesStatus.Enabled' using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify the Current Value for Network Device Status via WEB Pa Commands.");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the Below commands: curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NetworkDevicesStatus.Enabled ");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Current value for Network Device Status should be retieved successfully using the WEB PA command. ");
	    LOGGER.info("**********************************************************************************");
	    String networkDevicesStatus = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_WIFI_REPORT);
	    LOGGER.info("Network Devices Status retrieved using WebPa = " + networkDevicesStatus);
	    status = CommonMethods.isNotNull(networkDevicesStatus);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Current value for the Network Device Status retrieved successfully");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S4";
	    errorMessage = "Unable to retrieve the Current value for the Network Device Traffic using the WEBPA parameter  Device.X_RDKCENTRAL-COM_Report.NetworkDevicesTraffic.Enabled ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify the Current Value for Netwrok Device Traffic via WEB Pa Commands.");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the Below commands: curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NetworkDevicesTraffic.Enabled ");
	    LOGGER.info(
		    "STEP 4: EXPECTED : Current value for Network Device Traffic should be retieved successfully using the WEB PA command. ");
	    LOGGER.info("**********************************************************************************");
	    String networkDevicesTraffic = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_TRAFFIC_WIFI_REPORT);
	    LOGGER.info("Network Devices Traffic retrieved using WebPa = " + networkDevicesTraffic);
	    status = CommonMethods.isNotNull(networkDevicesTraffic);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Current value for the Network Device Traffic retrieved successfully");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S5";
	    errorMessage = "Unable to retrieve the Current value for the Interface Device WiFi using the WEBPA parameter  Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify retrieving current value for  the Interface Devices Wifi Via WEBPA Command.");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the below command: curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled ");
	    LOGGER.info(
		    "STEP 5: EXPECTED : Current value for Interface Devices Wifi should be retieved successfully using the WEB PA command. ");
	    LOGGER.info("**********************************************************************************");

	    String interfaceDevicesWifi = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
	    LOGGER.info("Network Devices Traffic retrieved using WebPa = " + interfaceDevicesWifi);
	    status = CommonMethods.isNotNull(interfaceDevicesWifi);
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Current value for the Interface Device WiFi retrieved successfully");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S6";
	    errorMessage = "Unable to enable the Network Device Status (Device.X_RDKCENTRAL-COM_Report.NetworkDevicesStatus.Enabled bool true) Parameter to true via WEBPA.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify Enabling the Network Device Status to True Via WEBPA Command.");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the Below commands:                            curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NetworkDevicesStatus.Enabled bool true");
	    LOGGER.info("STEP 6: EXPECTED : Enabling Network Device Status via WEBPA should be successful.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_WIFI_REPORT, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Network Device Status enabled to true successfully");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S7";
	    errorMessage = "Unable to enable the Network Device Trafic (Device.X_RDKCENTRAL-COM_Report.NetworkDevicesTraffic.Enabled bool true) Parameter to true via WEBPA.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify Enabling the Network Device Traffic to True Via WEBPA Command.");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute the Below commands:                            curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NetworkDevicesTraffic.Enabled bool true");
	    LOGGER.info("STEP 7: EXPECTED : Enabling Network Device Traffic via WEBPA should be successful.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_TRAFFIC_WIFI_REPORT,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Network Device Trafic enabled to true successfully");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Unable to set value as true for the parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\"";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify enabling the \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" to true using webpa command");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute the below command: curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled \"");
	    LOGGER.info(
		    "STEP 8: EXPECTED : \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" should get enabled to true");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_ENABLED_REPORT, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled parameter got enabled successfully");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "\"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" value is not returned as true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify the Current Value for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" via WEB Pa Commands.");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the Below command: curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=\"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\"");
	    LOGGER.info(
		    "STEP 9: EXPECTED : \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" should return value as 1");
	    LOGGER.info("**********************************************************************************");

	    String neighboringAP = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_ENABLED_REPORT);
	    LOGGER.info("NeighboringAP retrieved using WebPa = " + neighboringAP);
	    status = CommonMethods.isNotNull(neighboringAP);

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Current value for NeighboringAP value retrieved sucessfully");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "Value didnï¿½t get set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\"using the WEB PA command";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Set value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\" using WEBpa");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute the below command: curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod \"");
	    LOGGER.info(
		    "STEP 10: EXPECTED : Value should be set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\"using the WEB PA command");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_POLLING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_CONSTANT_3600);

	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : Value got set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\"");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Value retrieved is not expected for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\"using the WEB PA command";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Get value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\" using WEBpa");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the below command: curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod \"");
	    LOGGER.info(
		    "STEP 11: EXPECTED : Value should be retrieved successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\"using the WEB PA command");
	    LOGGER.info("**********************************************************************************");

	    String neighboringAPPollingPeriod = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_POLLING_PERIOD);
	    LOGGER.info("NeighboringAP retrieved using WebPa = " + neighboringAPPollingPeriod);
	    status = CommonMethods.isNotNull(neighboringAPPollingPeriod);

	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL : Current value for NeighboringAP Polling period value retrieved sucessfully");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Value didnï¿½t get set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"using the WEB PA command";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Set value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\" using WEBpa");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute the below command: curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"");
	    LOGGER.info(
		    "STEP 12: EXPECTED : Value should be set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"using the WEB PA command");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_REPORTING_PERIOD,
		    BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_CONSTANT_3600);

	    if (status) {
		LOGGER.info(
			"STEP 12: ACTUAL : Value got set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s13";
	    errorMessage = "Value retrieved is not expected for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"using the WEB PA command";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION : Get value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\" using WEBpa");
	    LOGGER.info(
		    "STEP 13: ACTION : Execute the below command: curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"");
	    LOGGER.info(
		    "STEP 13: EXPECTED : Value should be retrieved successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"using the WEB PA command");
	    LOGGER.info("**********************************************************************************");

	    String neighboringAPReportingPeriod = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_REPORTING_PERIOD);
	    LOGGER.info("NeighboringAP retrieved using WebPa = " + neighboringAPReportingPeriod);
	    status = CommonMethods.isNotNull(neighboringAPReportingPeriod);

	    if (status) {
		LOGGER.info(
			"STEP 13: ACTUAL : Current value for NeighboringAP Reporting period value retrieved sucessfully ");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s14";
	    errorMessage = "Harvestor process is not running in console";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION :  Verify the Process harvestor.txt is running in atom or arm console");
	    LOGGER.info("STEP 14: ACTION : Execute pidof harvester");
	    LOGGER.info("STEP 14: EXPECTED : Harvester process details should be displayed successfully");
	    LOGGER.info("**********************************************************************************");
	    isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	    response = CommonMethods.isAtomSyncAvailable(device, tapEnv)
		    ? BroadBandCommonUtils.getPidOfProcessFromAtomConsole(device, tapEnv,
			    BroadBandTestConstants.PROCESS_NAME_HARVESTER)
		    : CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_HARVESTER);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : Harvestor process is running");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S15";
	    errorMessage = "Harvesterlog.txt.0 Log doesnï¿½t conatin the required String.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION : Verify Harvester.txt log contains the required string.");
	    LOGGER.info(
		    "STEP 15: ACTION : Execute following commands: root@Docsis-Gateway:/rdklogs/logs# cat Harvesterlog.txt.0");
	    LOGGER.info(
		    "STEP 15: EXPECTED : Harvestor Log should contain \"Init for parodus Success\"  and \"Sent message successfully to parodus\" should be displayed.\"");
	    LOGGER.info("***********************************************************************************");

	    if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.LOG_MESSAGE_PARODUS_INIT, BroadBandCommandConstants.FILE_HARVESTER_LOG,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
		errorMessage = "Harvesterlog.txt.0 Log doesnï¿½t conatin the init for parodus string";
		response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
			BroadBandTraceConstants.LOG_MESSAGE_SUCCCESS_TO_PARADOUS_LMLITE,
			BroadBandCommandConstants.FILE_HARVESTER_LOG, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		status = CommonMethods.isNotNull(response);
	    }
	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : Harvesterlog.txt.0 Log contains the required string");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S16";
	    errorMessage = "Unable to enable the  Interface Devices Wifi (Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled bool true) Parameter to true via WEBPA.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION : Verify Enabling the Interface Devices Wifi to True Via WEBPA Command.");
	    LOGGER.info(
		    "STEP 16: ACTION : Execute the below command: curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled ");
	    LOGGER.info("STEP 16: EXPECTED : Enabling Interface Devices Wifi via WEBPA should be successful.");
	    LOGGER.info("***********************************************************************************");

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info("STEP 16: ACTUAL : Interface Devices Wifi enabled to true successfully");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S17";
	    errorMessage = " lmlite.txt process is not up and running.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : Verify the Process lmlite.txt is running in arm console.");
	    LOGGER.info("STEP 17: ACTION : Execute following command: /rdklogs/logs# ps | grep lmlite");
	    LOGGER.info(
		    "STEP 17: EXPECTED : lmlite.txt process details should be displayed successfully. (usr/bin/lmlite). ");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PS_GREP_CCSP);
	    status = CommonMethods.patternMatcher(response, BroadBandTestConstants.LMLITE_PROCESS_OUTPUT);
	    status = CommonMethods.isNotNull(response);

	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : lmlite.txt process is up and running");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, false, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info(
		    "POST-CONDITION 1 : DESCRIPTION : Revert the Network Device Status,Network Device Traffic,NeighboringAP,and Interface Device WiFi to its default Values.");
	    LOGGER.info(
		    "POST-CONDITION 1 : ACTION : Execute the below Command : Device.X_RDKCENTRAL-COM_Report.NetworkDeviceStatus.Enabled,Device.X_RDKCENTRAL-COM_Report.NetworkDeviceTraffic.Enabled,Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled,Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled");
	    LOGGER.info(
		    "POST-CONDITION 1 : EXPECTED : Reverting Back the Network Device Status,Network Device Traffic and Interface Device WiFi to its default Values should be succcessful.");
	    HashMap<String, String> privateValueMap = new HashMap<String, String>();
	    privateValueMap.put(BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_WIFI_REPORT,
		    BroadBandTestConstants.FALSE);
	    privateValueMap.put(BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_TRAFFIC_WIFI_REPORT,
		    BroadBandTestConstants.FALSE);

	    privateValueMap.put(BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_ENABLED_REPORT,
		    BroadBandTestConstants.FALSE);
	    privateValueMap.put(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT,
		    BroadBandTestConstants.FALSE);

	    status = BroadBandWebPaUtils.executeMultipleWebpaParametersSet(device, tapEnv, privateValueMap,
		    WebPaDataTypes.BOOLEAN.getValue());
	    if (status) {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION 1 : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	}
	LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-1008");
    }

    /**
     * Verify XDNS Ccspxdns not crashing on overriding default values
     * <ol>
     * <li>PRE-CONDITION : Verify whether WebPA is Up and Running in the Device</li>
     * <li>Step 1: Verify and Retrieve Default value for XDNS using WebPA.</li>
     * <li>Step 2: Verify and Enable the XDNS by setting the WebPA parameter to TRUE.</li>
     * <li>Step 3: Verify and Retrieve value for XDNS using WebPA.</li>
     * <li>Step 4: Verify the file /rdklogs/logs/CcspXdnsSsp is present in ARM Console</li>
     * <li>Step 5: Verify the file /rdklogs/logs/CcspHomeSecurity is present in ARM Console</li>
     * <li>POST-CONDITION : Revert the value for XDNS to its Default Value using WebPA.</li>
     * </ol>
     *
     * @param device
     *            {@link Dut}
     * @author Joseph M
     * @refactor yamini.s
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-WEBPA-1007")
    public void testToVerifyCcspProcessNotCrashingAfterEnablingXdns(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "S1";
	String errorMessage = "";
	boolean status = false;
	String response = null;
	BroadBandResultObject resultObject = null;
	boolean isFactoryReset = false;
	// Variable Declation Ends
	testCaseId = "TC-RDKB-WEBPA-107";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1007");
	LOGGER.info("TEST DESCRIPTION: Verify XDNS Ccspxdns not crashing on overriding default values");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION : Verify whether WebPA is Up and Running in the Device");
	LOGGER.info("1. Verify and Retrieve Default value for XDNS using WebPA.");
	LOGGER.info("2. Verify and Enable the XDNS by setting the WebPA parameter to TRUE.");
	LOGGER.info("3. Verify and Retrieve value for XDNS using WebPA.");
	LOGGER.info("4. Verify the file /rdklogs/logs/CcspXdnsSsp is present in ARM Console");
	LOGGER.info("5. Verify the file /rdklogs/logs/CcspHomeSecurity is present in ARM Console");
	LOGGER.info("POST-CONDITION : Revert the value for XDNS to its Default Value using WebPA.");
	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    isFactoryReset = BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    stepNum = "S1";
	    errorMessage = "Unable to retrieve the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' using WebPA command.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify and Retrieve Default value for XDNS using WebPA.");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute WebPA GET request for the parameter Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	    LOGGER.info(
		    "STEP 1: EXPECTED : WebPA GET request should be successful and default value retrieved for XDNS should be False.");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to retrieve the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' using WebPA command.";
	    status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.FALSE);
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL: Successfully retrieved  the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' using WebPA command.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "Unable to set the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' as 'true'using WebPA command.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify and Enable the XDNS by setting the WebPA parameter to TRUE.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute WebPA SET request for the Parameter Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS bool true");
	    LOGGER.info("STEP 2: EXPECTED : XDNS should be enabled sucessfully via WebPA.");
	    LOGGER.info("**********************************************************************************");
	    List<WebPaParameter> webPaParameters = BroadBandWebPaUtils.getListOfWebpaParametersToEnableOrDisableXdns(
		    BroadBandTestConstants.TRUE, AutomaticsPropertyUtility
			.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY),
			AutomaticsPropertyUtility
			.getProperty(BroadBandPropertyKeyConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY));
	    resultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
		    webPaParameters);
	    status = resultObject.isStatus();
	    errorMessage = resultObject.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL: Successfully able to set the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' as 'true' using WebPA command.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S3";
	    errorMessage = "Unable to retrieve the value for XDNS using WebPA Parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify and Retrieve value for XDNS using WebPA.");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute WebPA GET request for the parameter Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	    LOGGER.info(
		    "STEP 3: EXPECTED : WebPA GET request should be successful and value retrieved for XDNS should be True.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL: Successfully retrieved  the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' using WebPA command.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S4";
	    errorMessage = "CCSPXDNSSSP Process is not up and running for 3 mins. Process is Crashed after enabling XDNS. ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify the file /rdklogs/logs/CcspXdnsSsp is present in ARM Console");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the below Commands in Arm Console /rdklogs/logs# ps | grep CcspXdnsSsp");
	    LOGGER.info(
		    "STEP 4: EXPECTED : CCSPXDNSSSP Process should be up and running for 3 mins without Crashing after  enabling the XDNS.");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.PS_COMMAND_FOR_CCSPXDNSSSP_PROCESS);
	    status = CommonUtils.patternSearchFromTargetString(response,
		    BroadBandTestConstants.STRING_CCSPXDNSSSP_PROCESS);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : CCSPXDNSSSP FILE IS PRESENT IN ARM CONSOLE AND RUNNING.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S5";
	    errorMessage = "CcspHomeSecurity Process is not up and running. Process is Crashed after enabling XDNS. ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify the file /rdklogs/logs/CcspHomeSecurity is present in ARM Console");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the below Commands in Arm Console /rdklogs/logs# ps | grep CcspHomeSecurity");
	    LOGGER.info(
		    "STEP 5: EXPECTED : CcspHomeSecurity Process should be up and running without Crashing after enabling the XDNS.");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.PS_COMMAND_FOR_CCSPHOMESECURITY_PROCESS);
	    status = CommonUtils.patternSearchFromTargetString(response,
		    BroadBandTestConstants.STRING_CCSPHOMESECURITY_PROCESS);
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : CcspHomeSecurity FILE IS PRESENT IN ARM CONSOLE AND RUNNING.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);

	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION : Revert the value for XDNS to its Default Value using WebPA.");
	    LOGGER.info(
		    "POST-CONDITION 1 : ACTION : Execute WebPA SET request for the parameter Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	    LOGGER.info("POST-CONDITION 1: EXPECTED : Reverting the default value for XDNS should be successful.");
	    try {
		List<WebPaParameter> webPaParameters = BroadBandWebPaUtils
			.getListOfWebpaParametersToEnableOrDisableXdns(BroadBandTestConstants.FALSE,
					AutomaticsPropertyUtility
					.getProperty(BroadBandPropertyKeyConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE),
				BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV6_VALUE);
		resultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
			webPaParameters);
		status = resultObject.isStatus();
		errorMessage = resultObject.getErrorMessage();
	    } catch (Exception e) {
		LOGGER.error(e.getMessage());
	    }
	    if (status) {
		LOGGER.info("POST-CONDITION 1: ACTUAL : Default value for XDNS has been reverted successfully");
	    } else {
		LOGGER.error("POST-CONDITION 1: ACTUAL : Failed to revert the default values for XDNS");
	    }
	    if (isFactoryReset) {
		BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
			BroadBandTestConstants.CONSTANT_2);
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-1007");
    }

    /**
     * Verify reboot reason notifications sent for different types of reboot
     * <ol>
     * <li>Pre-conditon 1: Verify whether webpa process is up</li>
     * <li>Verify ManageableNotification feature is enabled by default</li>
     * <li>Tail PAM and PARODUS logs to nvram and reboot the device using WebPA command</li>
     * <li>Verify device goes for reboot and WebPA 404 response during reboot</li>
     * <li>Verify reboot pending notification with reboot reason webpa-reboot in PAMlog</li>
     * <li>Verify reboot pending notification in PARODUSlog</li>
     * <li>Verify Shutdown parodus in Arm/Consolelog</li>
     * <li>Verify Parodus SIGTERM received in PARODUSlog</li>
     * <li>Verify shutdown reason system_restarting in PARODUSlog</li>
     * <li>Verify cloud status set offline in PARODUSlog</li>
     * <li>Set custom maintenance window based on device date</li>
     * <li>Tail PAM and PARODUS logs to nvram and kill syseventd process running on the device</li>
     * <li>Verify device goes for reboot in maintenance window and comes up</li>
     * <li>Verify reboot pending notification with reboot reason Syseventd_crash in PAMlog</li>
     * <li>Verify reboot pending notification in PARODUSlog</li>
     * <li>Verify Shutdown parodus in Arm/Consolelog</li>
     * <li>Verify Parodus SIGTERM received in PARODUSlog</li>
     * <li>Verify shutdown reason system_restarting in PARODUSlog</li>
     * <li>Verify cloud status set offline in PARODUSlog</li>
     * </ol>
     * 
     * @author Ashwin sankara
     * @refactor Govardhan
     * @param device
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-1012")
    public void testVerifyRebootReasonNotifications(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBPA-112";
	String stepNum = "s1";
	String errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.ManageableNotification.Enable";
	String response = null;
	boolean status = false;
	WebPaServerResponse webpaResponse = null;
	int statusCode = BroadBandTestConstants.CONSTANT_0;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1012");
	LOGGER.info("TEST DESCRIPTION: Verify reboot reason notifications sent for different types of reboot");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("Pre-conditon 1: Verify whether webpa process is up");
	LOGGER.info("1. Verify ManageableNotification feature is enabled by default");
	LOGGER.info("2. Tail PAM and PARODUS logs to nvram and reboot the device using WebPA command");
	LOGGER.info("3. Verify device goes for reboot and WebPA 404 response during reboot");
	LOGGER.info("4. Verify reboot pending notification with reboot reason webpa-reboot in PAMlog");
	LOGGER.info("5. Verify reboot pending notification in PARODUSlog");
	LOGGER.info("6. Verify Shutdown parodus in Arm/Consolelog");
	LOGGER.info("7. Verify Parodus SIGTERM received in PARODUSlog");
	LOGGER.info("8. Verify shutdown reason system_restarting in PARODUSlog");
	LOGGER.info("9. Verify cloud status set offline in PARODUSlog");
	LOGGER.info("10. Set custom maintenance window based on device date");
	LOGGER.info("11. Tail PAM and PARODUS logs to nvram and kill syseventd process running on the device");
	LOGGER.info("12. Verify device goes for reboot in maintenance window and comes up");
	LOGGER.info("13. Verify reboot pending notification with reboot reason Syseventd_crash in PAMlog");
	LOGGER.info("14. Verify reboot pending notification in PARODUSlog");
	LOGGER.info("15. Verify Shutdown parodus in Arm/Consolelog");
	LOGGER.info("16. Verify Parodus SIGTERM received in PARODUSlog");
	LOGGER.info("17. Verify shutdown reason system_restarting in PARODUSlog");
	LOGGER.info("18. Verify cloud status set offline in PARODUSlog");

	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");

	    /**
	     * PRE-CONDITION 1 : Verify whether webpa process is up
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 1: DESCRIPTION : Verify WebPA process is up");
	    LOGGER.info("PRE-CONDITION 1: ACTION : Execute WebPa command "
		    + BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_SERIAL_NUMBER);
	    LOGGER.info("PRE-CONDITION 1: EXPECTED : WebPa command should be successfull");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "WebPA process is not up";
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    if (status) {
		LOGGER.info("PRE-CONDITION 1: ACTUAL :  WebPa process is up");
	    } else {
		LOGGER.error("PRE-CONDITION 1 ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
	    }

	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    if (DeviceModeHandler.isBusinessClassDevice(device)) {
		LOGGER.info(
			"STEP 1 is not applicable for BCI devices as they do not support ManageableNotification Enable parameter");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			"BCI devices do not support ManageableNotification Enable parameter", false);
	    } else {
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 1: DESCRIPTION : Verify ManageableNotification feature is enabled by default");
		LOGGER.info(
			"STEP 1: ACTION : Execute webpa or dmcli command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.ManageableNotification.Enable");
		LOGGER.info("STEP 1: EXPECTED : Value of parameter is true by default");
		LOGGER.info("**********************************************************************************");

		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAMETER_MANAGEABLE_NOTIFICATION_ENABLE);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.ManageableNotification.Enable is not true by default";
		    status = response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
		}

		if (status) {
		    LOGGER.info("STEP 1: ACTUAL : Value of parameter is true by default");
		} else {
		    LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    }

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Failed to set value of RebootDevice parameter to Device";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Tail PAM and PARODUS logs to nvram and reboot the device using WebPA command");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute commands:\n1. tail -f /rdklogs/logs/PAMlog.txt.0 > /nvram/PAMtail.txt &"
			    + "\n2. tail -f /rdklogs/logs/PARODUSlog.txt.0 > /nvram/PARODUStail &"
			    + "\n3. tail -f /rdklogs/logs/Consolelog.txt.0 > /nvram/Consoletail (ArmConsolelog for AtomsyncDevice) &"
			    + "\n4. Execute webpa command to set value of Device.X_CISCO_COM_DeviceControl.RebootDevice to Device");
	    LOGGER.info("STEP 2: EXPECTED : Rebooted device using WebPA successfully");
	    LOGGER.info("**********************************************************************************");

	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PAMLOGS_NVRAM);
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PARODUSLOGS_NVRAM);
	    tapEnv.executeCommandUsingSsh(device,
		    (CommonMethods.isAtomSyncAvailable(device, tapEnv)
			    ? BroadBandCommandConstants.CMD_GET_ARMCONSOLELOGS_NVRAM
			    : BroadBandCommandConstants.CMD_GET_CONSOLELOGS_NVRAM));
	    status = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CONTROL_DEVICE_REBOOT, BroadBandTestConstants.DEVICE,
		    BroadBandTestConstants.CONSTANT_0);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Rebooted device using WebPA successfully");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Device did not go for reboot after triggering using webpa";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify device goes for reboot and WebPA 404 response during reboot");
	    LOGGER.info(
		    "STEP 3: ACTION : Check if box is sshable after going for reboot. Execute webpa command during reboot and verify 404 response code. Then wait for device to come up");
	    LOGGER.info("STEP 3: EXPECTED : Device rebooted successfully and verified 404 response during reboot");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.CONSTANT_6)) {
		errorMessage = "Failed to receive 404 response during reboot for webpa command";
		webpaResponse = tapEnv.getTR69ParameterValuesUsingWebPA(device,
			BroadBandWebPaConstants.TR69_PARAM_SERIAL_NUMBER);
		if (null != webpaResponse) {
		    statusCode = webpaResponse.getStatusCode();
		    LOGGER.info("STATUS CODE: " + statusCode);
		    if (statusCode == HttpStatus.SC_NOT_FOUND) {
			errorMessage = "Device did not come up after webpa reboot";
			status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
		    }
		}
	    }

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Device rebooted successfully and verified 404 response during reboot");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    if (DeviceModeHandler.isBusinessClassDevice(device)) {
		LOGGER.info(
			"STEP 4 and 5 are not applicable for BCI devices");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s4", ExecutionStatus.NOT_APPLICABLE,
			"Not applicable for BCI devices", false);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s5", ExecutionStatus.NOT_APPLICABLE,
			"Not applicable for BCI devices", false);
	    } else {
		stepNum = "s4";
		errorMessage = "Unable to find reboot pending notification log message in PAMlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 4: DESCRIPTION : Verify reboot pending notification with reboot reason webpa-reboot in PAMlog");
		LOGGER.info("STEP 4: ACTION : Execute command:grep -i \"reboot-pending\" /nvram/PAMtail");
		LOGGER.info(
			"STEP 4: EXPECTED : Reboot pending notification is present in PAMlog with reason webpa-reboot");
		LOGGER.info("**********************************************************************************");

		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			BroadBandCommandConstants.FILE_PATH_NVRAM_PAM_TAIL);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Unable to find reboot reason as webpa-reboot present in reboot pending notification";
		    status = CommonMethods.patternMatcher(response,
			    BroadBandTelemetryConstants.LAST_REBOOT_REASON_FOR_WEBPA_REBOOT);
		}
		CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
			BroadBandCommandConstants.FILE_PATH_NVRAM_PAM_TAIL);

		if (status) {
		    LOGGER.info(
			    "STEP 4: ACTUAL : Reboot pending notification is present in PAMlog with reason webpa-reboot");
		} else {
		    LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s5";
		errorMessage = "Unable to find reboot pending log message in PARODUSlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 5: DESCRIPTION : Verify reboot pending notification in PARODUSlog");
		LOGGER.info("STEP 5: ACTION : Execute command:grep -i \"reboot-pending\" /nvram/PARODUStail");
		LOGGER.info("STEP 5: EXPECTED : Reboot pending notification is present in PARODUSlog");
		LOGGER.info("**********************************************************************************");

		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));

		if (status) {
		    LOGGER.info("STEP 5: ACTUAL : Reboot pending notification is present in PARODUSlog");
		} else {
		    LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Unable to find Shutdown parodus log message in Arm/Consolelog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify Shutdown parodus in Arm/Consolelog");
	    LOGGER.info("STEP 6: ACTION : Execute command:grep -i \"Shutdown parodus\" /nvram/Consoletail");
	    LOGGER.info("STEP 6: EXPECTED : Shutdown parodus is present in Arm/Consolelog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_SHUTDOWN_PARODUS,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_CONSOLE_TAIL));
	    CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_CONSOLE_TAIL);

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Shutdown parodus is present in Arm/Consolelog");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Unable to find Parodus SIGTERM received log message in PARODUSlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify Parodus SIGTERM received in PARODUSlog");
	    LOGGER.info("STEP 7: ACTION : Execute command:grep -i \"PARODUS: SIGTERM received\" /nvram/PARODUStail");
	    LOGGER.info("STEP 7: EXPECTED : Parodus SIGTERM received is present in PARODUSlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_PARODUS_SIGTERM_RECEIVED,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Parodus SIGTERM received is present in PARODUSlog");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Unable to find shutdown reason system_restarting log message in PARODUSlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify shutdown reason system_restarting in PARODUSlog");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute command:grep -i \"PARODUS: shutdown reason at close system_restarting\" /nvram/PARODUStail");
	    LOGGER.info("STEP 8: EXPECTED : Shutdown reason system_restarting is present in PARODUSlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_PARODUS_SHUTDOWN_REASON,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Shutdown reason system_restarting is present in PARODUSlog");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Unable to find cloud status set offline log message in PARODUSlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify cloud status set offline in PARODUSlog");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute command:grep -i \"PARODUS: cloud_status set as offline after connection close\" /nvram/PARODUStail");
	    LOGGER.info("STEP 9: EXPECTED : Cloud status set offline is present in PARODUSlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_CLOUD_STATUS_SET_OFFLINE,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));
	    CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL);

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Cloud status set offline is present in PARODUSlog");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Set custom maintenance window based on device date");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute commands:1. date -d \"00:00:00\" \"+%s\"2. date +%s3. Get value of Device.Time.TimeOffset3. Set Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeStartTime to \"value of (2) - value of (1) + value of (3) + 120\"4. Set Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeEndTime to FirmwareUpgradeStartTime plus 9005. Verify values have been set successfully");
	    LOGGER.info(
		    "STEP 10: EXPECTED : Set custom maintenance window for 15 minutes from current time successfully");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "WebPA process not up after reboot";
	    if (BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)) {
		if (DeviceModeHandler.isDSLDevice(device)) {
		    BroadBandResultObject result = BroadBandCommonUtils.setCustomMaintenanceWindow(device, tapEnv,
			    BroadBandTestConstants.CONSTANT_NINE_HUNDRED);
		    status = result.isStatus();
		    errorMessage = result.getErrorMessage();
		} else {
		    BroadBandResultObject result = BroadBandCommonUtils.setMaintenanceWindowWithGivenDetail(device,
			    tapEnv, BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.CONSTANT_15);
		    errorMessage = result.getErrorMessage();
		    status = result.isStatus();
		}

	    }

	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : Set custom maintenance window for 15 minutes from current time successfully");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Failed to crash syseventd process";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Tail PAM and PARODUS logs to nvram and kill syseventd process running on the device");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute commands:\n1. tail -f /rdklogs/logs/PAMlog.txt.0 > /nvram/PAMtail.txt &"
			    + "\n2. tail -f /rdklogs/logs/PARODUSlog.txt.0 > /nvram/PARODUStail &"
			    + "\n3. tail -f /rdklogs/logs/Consolelog.txt.0 > /nvram/Consoletail (ArmConsolelog for AtomsyncDevice) &"
			    + "\n4. pidof syseventd\n5. kill -11 <pid>");
	    LOGGER.info("STEP 11: EXPECTED : Successfully simulated syseventd process crash");
	    LOGGER.info("**********************************************************************************");

	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PAMLOGS_NVRAM);
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PARODUSLOGS_NVRAM);
	    tapEnv.executeCommandUsingSsh(device,
		    (CommonMethods.isAtomSyncAvailable(device, tapEnv)
			    ? BroadBandCommandConstants.CMD_GET_ARMCONSOLELOGS_NVRAM
			    : BroadBandCommandConstants.CMD_GET_CONSOLELOGS_NVRAM));
	    status = CommonMethods.restartProcess(device, tapEnv, ProcessRestartOption.KILL_11,
		    BroadBandTestConstants.PROCESS_NAME_SYSEVENTD);

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Successfully simulated syseventd process crash");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Device did not go for reboot in maintenance window after syseventd crash";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Verify device goes for reboot in maintenance window and comes up");
	    LOGGER.info(
		    "STEP 12: ACTION : Check if box is sshable after going for reboot and waiting for appropriate time");
	    LOGGER.info("STEP 12: EXPECTED : Device rebooted successfully");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.CONSTANT_70)) {
		errorMessage = "Device did not come up after webpa reboot";
		status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
	    }

	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Device rebooted successfully");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    if (DeviceModeHandler.isDSLDevice(device)) {
		LOGGER.info(
			"STEP 13 and 14 are not applicable for BCI devices ");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s13", ExecutionStatus.NOT_APPLICABLE,
			"Not applicable for BCI devices ", false);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s14", ExecutionStatus.NOT_APPLICABLE,
			"Not applicable for BCI devices", false);
	    } else {
		stepNum = "s13";
		errorMessage = "Unable to find reboot pending notification log message in PAMlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 13: DESCRIPTION : Verify reboot pending notification with reboot reason Syseventd_crash in PAMlog");
		LOGGER.info("STEP 13: ACTION : Execute command:grep -i \"reboot-pending\" /nvram/PAMtail");
		LOGGER.info(
			"STEP 13: EXPECTED : Reboot pending notification is present in PAMlog with reason Syseventd_crash");
		LOGGER.info("**********************************************************************************");

		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			BroadBandCommandConstants.FILE_PATH_NVRAM_PAM_TAIL);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Unable to find reboot reason as Syseventd_crash present in reboot pending notification";
		    status = CommonMethods.patternMatcher(response,
			    BroadBandTestConstants.REBOOT_REASON_SYSEVENTD_CRASH);
		}
		CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
			BroadBandCommandConstants.FILE_PATH_NVRAM_PAM_TAIL);

		if (status) {
		    LOGGER.info(
			    "STEP 13: ACTUAL : Reboot pending notification is present in PAMlog with reason Syseventd_crash");
		} else {
		    LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s14";
		errorMessage = "Unable to find reboot pending log message in PARODUSlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 14: DESCRIPTION : Verify reboot pending notification in PARODUSlog");
		LOGGER.info("STEP 14: ACTION : Execute command:grep -i \"reboot-pending\" /nvram/PARODUStail");
		LOGGER.info("STEP 14: EXPECTED : Reboot pending notification is present in PARODUSlog");
		LOGGER.info("**********************************************************************************");

		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));

		if (status) {
		    LOGGER.info("STEP 14: ACTUAL : Reboot pending notification is present in PARODUSlog");
		} else {
		    LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s15";
	    errorMessage = "Unable to find Shutdown parodus log message in Arm/Consolelog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION : Verify Shutdown parodus in Arm/Consolelog");
	    LOGGER.info("STEP 15: ACTION : Execute command:grep -i \"Shutdown parodus\" /nvram/Consoletail");
	    LOGGER.info("STEP 15: EXPECTED : Shutdown parodus is present in Arm/Consolelog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_SHUTDOWN_PARODUS,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_CONSOLE_TAIL));
	    CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_CONSOLE_TAIL);

	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : Shutdown parodus is present in Arm/Consolelog");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s16";
	    errorMessage = "Unable to find Parodus SIGTERM received log message in PARODUSlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION : Verify Parodus SIGTERM received in PARODUSlog");
	    LOGGER.info("STEP 16: ACTION : Execute command:grep -i \"PARODUS: SIGTERM received\" /nvram/PARODUStail");
	    LOGGER.info("STEP 16: EXPECTED : Parodus SIGTERM received is present in PARODUSlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_PARODUS_SIGTERM_RECEIVED,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));

	    if (status) {
		LOGGER.info("STEP 16: ACTUAL : Parodus SIGTERM received is present in PARODUSlog");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s17";
	    errorMessage = "Unable to find shutdown reason system_restarting log message in PARODUSlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : Verify shutdown reason system_restarting in PARODUSlog");
	    LOGGER.info(
		    "STEP 17: ACTION : Execute command:grep -i \"PARODUS: shutdown reason at close system_restarting\" /nvram/PARODUStail");
	    LOGGER.info("STEP 17: EXPECTED : Shutdown reason system_restarting is present in PARODUSlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_PARODUS_SHUTDOWN_REASON,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));

	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : Shutdown reason system_restarting is present in PARODUSlog");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s18";
	    errorMessage = "Unable to find cloud status set offline log message in PARODUSlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION : Verify cloud status set offline in PARODUSlog");
	    LOGGER.info(
		    "STEP 18: ACTION : Execute command:grep -i \"PARODUS: cloud_status set as offline after connection close\" /nvram/PARODUStail");
	    LOGGER.info("STEP 18: EXPECTED : Cloud status set offline is present in PARODUSlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_CLOUD_STATUS_SET_OFFLINE,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));
	    CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL);

	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : Cloud status set offline is present in PARODUSlog");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Remove nvram tail files");
	    LOGGER.info(
		    "POST-CONDITION 1 : ACTION : Remove /nvram/PAMtail /nvram/PARODUStail /nvram/Consoletail.txt files");
	    LOGGER.info("POST-CONDITION 1 : EXPECTED : Files removed successfully");

	    status = false;
	    errorMessage = "Failed to remove file: " + BroadBandCommandConstants.FILE_PATH_NVRAM_PAM_TAIL;
	    if (CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PAM_TAIL)) {
		errorMessage = "Failed to remove file: " + BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL;
		if (CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
			BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL)) {
		    errorMessage = "Failed to remove file: " + BroadBandCommandConstants.FILE_PATH_NVRAM_CONSOLE_TAIL;
		    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
			    BroadBandCommandConstants.FILE_PATH_NVRAM_CONSOLE_TAIL);
		}
	    }

	    if (status) {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : Files removed successfully");
	    } else {
		LOGGER.error("POST-CONDITION 1 : ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("POST-CONDITION 2 : DESCRIPTION : Revert maintenance window parameter values");
	    LOGGER.info(
		    "POST-CONDITION 2 : ACTION : 1. Set Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeStartTime to 0\n"
			    + "2. Set Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeEndTime to 10800");
	    LOGGER.info("POST-CONDITION 2 : EXPECTED : Maintenance window values reset successfully");

	    status = false;
	    errorMessage = "WebPA process not up after reboot";
	    if (BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)) {
		errorMessage = "Unable to reset value of FirmwareUpgradeStartTime to 0";
		if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME,
			BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_ZERO)) {
		    errorMessage = "Unable to reset value of FirmwareUpgradeEndTime to 10800";
		    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME,
			    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_VALUE_10800);
		}
	    }

	    if (status) {
		LOGGER.info("POST-CONDITION 2 : ACTUAL : Maintenance window values reset successfully");
	    } else {
		LOGGER.error("POST-CONDITION 2 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-1012");
    }

    /**
     * Verify firmware download event notifications for invalid build with Manageable notification feature enabled
     * 
     * <li>Set the value of ManageableNotificationEnable parameter to true</li>
     * <li>Trigger CDL upgrade device to dummy build using mock XCONF</li>
     * <li>Verify Firmware download started notification in xconf.txt.0</li>
     * <li>Verify firmware download started notification in PAMlog</li>
     * <li>Verify firmware download started notification in PARODUSlog</li>
     * <li>Verify value of FirmwareDownloadStartedNotification parameter is updated</li>
     * <li>Verify Firmware download completed notification in xconf.txt.0</li>
     * <li>Verify firmware download completed failure notification in PAMlog</li>
     * <li>Verify firmware download completed notification in PARODUSlog</li>
     * <li>Verify value of FirmwareDownloadCompletedNotification parameter is not updated</li>
     * 
     * </ol>
     * 
     * @param device
     * @Refactor Rakesh C N
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-1020")
    public void testVerifyFirmwareDownloadEventNotification(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBPA-120";
	String stepNum = "s1";
	String response = null;
	String errorMessage = null;
	int stepCounter = BroadBandTestConstants.CONSTANT_0;
	long startTime = BroadBandTestConstants.CONSTANT_0;
	boolean status = false;
	boolean isCdlDataPosted = false;
	int postConStepNumber = BroadBandTestConstants.CONSTANT_0;
	boolean featureAvailableBuild = false;
	String cdlStartTime = null;
	String currentCdlBuild = null;
	String latestCdlBuild = null;
	String priorityValue = null;

	// Variable Declation Ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1020");
	LOGGER.info(
		"TEST DESCRIPTION: Verify firmware download event notifications for invalid build with Manageable notification feature enabled");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Set the value of ManageableNotificationEnable parameter to true");
	LOGGER.info("2. Trigger CDL upgrade device to dummy build using mock XCONF");
	LOGGER.info("3. Verify Firmware download started notification in xconf.txt.0");
	LOGGER.info("4. Verify firmware download started notification in PAMlog");
	LOGGER.info("5. Verify firmware download started notification in PARODUSlog");
	LOGGER.info("6. Verify value of FirmwareDownloadStartedNotification parameter is updated");
	LOGGER.info("7. Verify Firmware download completed notification in xconf.txt.0");
	LOGGER.info("8. Verify firmware download completed failure notification in PAMlog");
	LOGGER.info("9. Verify firmware download completed notification in PARODUSlog");
	LOGGER.info("10. Verify value of FirmwareDownloadCompletedNotification parameter is not updated");
	LOGGER.info("#######################################################################################");

	try {

	    BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv, postConStepNumber++);

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");

	    stepNum = "s1";
	    errorMessage = "Unable to set value of ManageableNotificationEnable parameter to true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Set the value of ManageableNotificationEnable parameter to true");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute webpa or dmcli command to set value of ManageableNotificationEnable parameter to true");
	    LOGGER.info("STEP 1: EXPECTED : Successfully set the value of parameter to true");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_MANAGEABLE_NOTIFICATION_ENABLE,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE)
		    && BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Successfully set the value of ManageableNotificationEnable parameter to true");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Unable to trigger CDL for dummy build";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Trigger CDL upgrade device to dummy build using mock XCONF");
	    LOGGER.info(
		    "STEP 2: ACTION : Configure mock server with dummy image for estb mac of the device and trigger Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow");
	    LOGGER.info("STEP 2: EXPECTED : Successfully triggered image download to dummy build");
	    LOGGER.info("**********************************************************************************");

	    if (!DeviceModeHandler.isDSLDevice(device)) {
		BroadBandXconfCdlUtils.updateSoftwareUpdateConfigurationOnClient(tapEnv, device);
		BroadBandXconfCdlUtils.configureXconfDownloadFirmwareDetailsWithInvalidURL(tapEnv, device,
			BroadBandTestConstants.STRING_DEVICE_NAME, false,
			BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP, 0);
		startTime = BroadBandCommonUtils.getEpochTimeInSecond(tapEnv, device);
		BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_TRIGGERING_XCONF_CDL, BroadBandTestConstants.CONSTANT_3,
			BroadBandTestConstants.TRUE);
	    } else {
		BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device,
			BroadBandTestConstants.STRING_DEVICE_NAME, true,
			BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
		startTime = BroadBandCommonUtils.getEpochTimeInSecond(tapEnv, device);
		FirmwareDownloadUtils.triggerCdlUsingShellScriptForDSL(tapEnv, device);
	    }
	    status = true;

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully triggered image download to dummy build");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");
	    if (!DeviceModeHandler.isDSLDevice(device)) {

		stepNum = "s3";
		errorMessage = "Unable to find ### httpdownload started ### log message in xconf.txt.0";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 3: DESCRIPTION : Verify Firmware download started notification in xconf.txt.0");
		LOGGER.info("STEP 3: ACTION : Execute grep commands to search for 'httpdownload started' and"
			+ " 'FirmwareDownloadStartedNotification' in /rdklogs/logs/xconf.txt.0");
		LOGGER.info(
			"STEP 3: EXPECTED : Firmware download started notification log message is present in xconf.txt.0");
		LOGGER.info("**********************************************************************************");

		if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			FirmwareDownloadUtils.getCdlDownloadStartedLog(device),
			BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.SIX_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
		    errorMessage = "Unable to find FirmwareDownloadStartedNotification log message in "
			    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
		    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_STARTED_NOTIFICATION,
			    BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
		}

		if (status) {
		    LOGGER.info(
			    "STEP 3: ACTUAL : Firmware download started notification log message is present in xconf.txt.0");
		} else {
		    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		LOGGER.info("**********************************************************************************");

		stepNum = "s4";
		errorMessage = "Unable to find firmware-download-started log message in PAMlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 4: DESCRIPTION : Verify firmware download started notification in PAMlog");
		LOGGER.info(
			"STEP 4: ACTION : Execute command:grep -i \"firmware-download-started\" /rdklogs/logs/PAMlog.txt.0");
		LOGGER.info(
			"STEP 4: EXPECTED : Firmware download started notification log message is present in PAMlog");
		LOGGER.info("**********************************************************************************");

		featureAvailableBuild = BroadBandCommonUtils.verifyFeatureAvailabilityInBuild(tapEnv, device,
			AutomaticsTapApi.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_JAIL_UI_FEATURE));
		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_STARTED,
			BroadBandTestConstants.COMMAND_NTP_LOG_FILE);
		if (featureAvailableBuild) {
		    if (CommonMethods.isNotNull(response)) {
			String jsonStr = CommonMethods.patternFinder(response,
				BroadBandTestConstants.PATTERN_GET_XCONF_PAYLOAD);
			if (CommonMethods.isNotNull(jsonStr)) {
			    LOGGER.info(jsonStr);
			    JSONObject objectName = new JSONObject(jsonStr);
			    if (objectName.has(BroadBandTestConstants.STATUS)
				    && objectName.has(BroadBandTestConstants.PRIORITY)
				    && objectName.has(BroadBandTestConstants.START_TIME)
				    && objectName.has(BroadBandTestConstants.CURRENT_FW_VER)
				    && objectName.has(BroadBandTestConstants.DOWNLOAD_FW_VER)) {
				cdlStartTime = objectName.getString(BroadBandTestConstants.START_TIME).toString();
				currentCdlBuild = objectName.getString(BroadBandTestConstants.CURRENT_FW_VER)
					.toString();
				latestCdlBuild = objectName.getString(BroadBandTestConstants.DOWNLOAD_FW_VER)
					.toString();
				priorityValue = objectName.getString(BroadBandTestConstants.PRIORITY).toString();
				if (CommonMethods.isNotNull(priorityValue) && CommonMethods.isNotNull(cdlStartTime)
					&& CommonMethods.isNotNull(latestCdlBuild)
					&& CommonMethods.isNotNull(currentCdlBuild)) {
				    status = objectName.get(BroadBandTestConstants.STATUS).toString()
					    .equalsIgnoreCase(BroadBandTestConstants.FW_DWN_STARTED)
					    && priorityValue
						    .equalsIgnoreCase(BroadBandTestConstants.FW_DWN_PRIORITY_DEFERRED);
				}

			    }
			}
		    }
		} else {
		    status = CommonMethods.isNotNull(response);
		}

		if (status) {
		    LOGGER.info(
			    "STEP 4: ACTUAL : Firmware download started notification log message is present in PAMlog and all the fields are available");
		} else {
		    LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s5";
		errorMessage = "Unable to find firmware-download-started log message in PARODUSlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 5: DESCRIPTION : Verify firmware download started notification in PARODUSlog");
		LOGGER.info(
			"STEP 5: ACTION : Execute command:grep -i \"firmware-download-started\" /rdklogs/logs/PARODUSlog.txt.0");
		LOGGER.info(
			"STEP 5: EXPECTED : Firmware download started notification log message is present in PARODUSlog");
		LOGGER.info("**********************************************************************************");

		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_STARTED,
			BroadBandCommandConstants.LOG_FILE_PARODUS));

		if (status) {
		    LOGGER.info(
			    "STEP 5: ACTUAL : Firmware download started notification log message is present in PARODUSlog");
		} else {
		    LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s6";
		errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 6: DESCRIPTION : Verify value of FirmwareDownloadStartedNotification parameter is updated");
		LOGGER.info("STEP 6: ACTION : Execute webpa or dmcli command to get value of "
			+ "Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification");
		LOGGER.info("STEP 6: EXPECTED : Should update all the parameters successfully");
		LOGGER.info("**********************************************************************************");

		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAMETER_FIRMWARE_DOWNLOAD_STARTED_NOTIFICATION);

		if (CommonMethods.isNotNull(response)) {
		    if (featureAvailableBuild) {
			errorMessage = "Value of parameter is not matched with xconf log";
			status = CommonUtils.isGivenStringAvailableInCommandOutput(response, priorityValue)
				&& CommonUtils.isGivenStringAvailableInCommandOutput(response, latestCdlBuild)
				&& CommonUtils.isGivenStringAvailableInCommandOutput(response, currentCdlBuild)
				&& CommonUtils.isGivenStringAvailableInCommandOutput(response, cdlStartTime);
		    } else {
			errorMessage = "Value of parameter is less than firmware download start time";
			status = Long.parseLong(response) > startTime;
		    }

		}

		if (status) {
		    LOGGER.info("STEP 6: ACTUAL : Successfully verified parameters in firmware download started");
		} else {
		    LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s7";
		errorMessage = "Unable to find ### httpdownload completed ### log message in xconf.txt.0";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 7: DESCRIPTION : Verify Firmware download completed notification in xconf.txt.0");
		LOGGER.info(
			"STEP 7: ACTION : Execute commands: grep \"httpdownload completed\" /rdklogs/logs/xconf.txt.0"
				+ " , grep \"FirmwareDownloadCompletedNotification\" /rdklogs/logs/xconf.txt.0");
		LOGGER.info(
			"STEP 7: EXPECTED : Firmware download completed notification log message is present in xconf.txt.0");
		LOGGER.info("**********************************************************************************");

		if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			FirmwareDownloadUtils.getCdlDownloadCompletedLog(device),
			BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.SIX_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
		    errorMessage = "Unable to find FirmwareDownloadCompletedNotification log message in "
			    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
		    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_COMPLETED_NOTIFICATION,
			    BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
		}

		if (status) {
		    LOGGER.info(
			    "STEP 7: ACTUAL : Firmware download completed notification log message is present in xconf.txt.0");
		} else {
		    LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s8";
		errorMessage = "Unable to find firmware-download-completed log message in PAMlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 8: DESCRIPTION : Verify firmware download completed failure notification in PAMlog");
		LOGGER.info(
			"STEP 8: ACTION : Execute command:grep -i \"firmware-download-completed\" /rdklogs/logs/PAMlog.txt.0");
		LOGGER.info(
			"STEP 8: EXPECTED : Firmware download completed notification log message is present in PAMlog with status failure");
		LOGGER.info("**********************************************************************************");

		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_COMPLETED,
			BroadBandTestConstants.COMMAND_NTP_LOG_FILE, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Unable to find download status failure in notification payload when download failed";
		    status = CommonMethods.patternMatcher(response, BroadBandTestConstants.STATUS_FAILURE);
		}

		if (status) {
		    LOGGER.info(
			    "STEP 8: ACTUAL : Firmware download completed notification log message is present in PAMlog with status failure");
		} else {
		    LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s9";
		errorMessage = "Unable to find firmware-download-completed log message in PARODUSlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 9: DESCRIPTION : Verify firmware download completed notification in PARODUSlog");
		LOGGER.info(
			"STEP 9: ACTION : Execute command:grep -i \"firmware-download-completed\" /rdklogs/logs/PARODUSlog.txt.0");
		LOGGER.info(
			"STEP 9: EXPECTED : Firmware download completed notification log message is present in PARODUSlog");
		LOGGER.info("**********************************************************************************");

		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_COMPLETED,
			BroadBandCommandConstants.LOG_FILE_PARODUS));

		if (status) {
		    LOGGER.info(
			    "STEP 9: ACTUAL : Firmware download completed notification log message is present in PARODUSlog");
		} else {
		    LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s10";
		errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 10: DESCRIPTION : Verify value of FirmwareDownloadCompletedNotification parameter is not updated");
		LOGGER.info("STEP 10: ACTION : Execute webpa or dmcli command to get value of "
			+ "Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification");
		LOGGER.info(
			"STEP 10: EXPECTED : Value of parameter is not updated to true after firmware download completed");
		LOGGER.info("**********************************************************************************");

		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAMETER_FIRMWARE_DOWNLOAD_COMPLETED_NOTIFICATION);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Value of parameter is true after firmware download completed when disabled";
		    status = !response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
		}

		if (status) {
		    LOGGER.info(
			    "STEP 10: ACTUAL : Value of parameter is not updated to true after firmware download completed");
		} else {
		    LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");
	    } else {
		stepCounter = 3;
		while (stepCounter <= 10) {
		    stepNum = "s" + stepCounter;
		    errorMessage = "This Step " + stepNum + " is not Applicable for device";
		    LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		    stepCounter++;
		}
	    }

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    /**
	     * POST CONDITION 1 : CLEAR THE CDL INFORMATION IN XCONF SERVER
	     */
	    if (isCdlDataPosted) {
		postConStepNumber++;
		BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE, BroadBandTestConstants.CONSTANT_3,
			BroadBandTestConstants.TRUE);
		BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv,
			postConStepNumber);

	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-1020");
    }

    /**
     * Verify firmware download event notifications with Manageable notification feature disabled
     *
     * <li>Set the value of ManageableNotificationEnable parameter to false</li>
     * <li>Trigger CDL upgrade device to original build using mock XCONF</li>
     * <li>Verify Firmware download started notification not present in xconf.txt.0</li>
     * <li>Verify firmware download started notification not present in PAMlog</li>
     * <li>Verify firmware download started notification not present in PARODUSlog</li>
     * <li>Verify value of FirmwareDownloadStartedNotification parameter is not updated</li>
     * <li>Verify Firmware download completed notification not present in xconf.txt.0</li>
     * <li>Verify firmware download completed notification not present in PAMlog</li>
     * <li>Verify firmware download completed notification not present in PARODUSlog</li>
     * <li>Verify value of FirmwareDownloadCompletedNotification parameter is not updated</li>
     * <li>Verify reboot pending notification present without reboot reason in PAMlog</li>
     * <li>Wait for device to reboot and verify it comes up with original image</li>
     * <li>Verify value of ManageableNotificationEnable remains false after reboot</li>
     * <li>Verify WebPA process is up after reboot</li>
     * <li>Verify device fully manageable notification is not present in PAMlog</li>
     * <li>Verify device fully manageable notification is not present in PARODUSlog</li>
     * <li>Verify value of DeviceManageableNotification parameter is 0 when disabled after webpa is up</li>
     * 
     * </ol>
     * 
     * @param device
     * @Refactor Sruthi Santhosh
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-1019")
    public void testVerifyFirmwareDownloadEventNotificationDisabled(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBPA-119";
	String stepNum = "s1";
	String response = null;
	String errorMessage = null;
	String currentImage = null;
	String latestImage = null;
	int stepCounter = BroadBandTestConstants.CONSTANT_0;
	long startTime = BroadBandTestConstants.CONSTANT_0;
	boolean status = false;
	boolean isCdlDataPosted = false;
	int postConStepNumber = BroadBandTestConstants.CONSTANT_0;
	// Variable Declaration Ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1019");
	LOGGER.info(
		"TEST DESCRIPTION: Verify firmware download event notifications  with Manageable notification feature disabled");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Set the value of ManageableNotificationEnable parameter to false");
	LOGGER.info("2. Trigger CDL upgrade device to original build using mock XCONF");
	LOGGER.info("3. Verify firmware download started notification not present in PAMlog");
	LOGGER.info("3. Verify firmware download started notification not present in PARODUSlog");
	LOGGER.info("4. Verify value of FirmwareDownloadStartedNotification parameter is not updated");
	LOGGER.info("5. Verify Firmware download completed notification not present in xconf.txt.0");
	LOGGER.info("6. Verify firmware download completed notification not present in PAMlog");
	LOGGER.info("7. Verify firmware download completed notification not present in PARODUSlog");
	LOGGER.info("8. Verify value of FirmwareDownloadCompletedNotification parameter is not updated");
	LOGGER.info("9. Verify reboot pending notification present without reboot reason in PAMlog");
	LOGGER.info("10. Wait for device to reboot and verify it comes up with original image");
	LOGGER.info("11. Verify value of ManageableNotificationEnable remains false after reboot");
	LOGGER.info("12. Verify WebPA process is up after reboot");
	LOGGER.info("13. Verify device fully manageable notification is not present in PAMlog");
	LOGGER.info("14. Verify device fully manageable notification is not present in PARODUSlog");
	LOGGER.info("15. Verify value of DeviceManageableNotification parameter is 0 when disabled after webpa is up");
	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");

	    stepNum = "s1";
	    errorMessage = "Unable to set value of ManageableNotificationEnable parameter to false";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Set the value of ManageableNotificationEnable parameter to false");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute webpa or dmcli command to set value of ManageableNotificationEnable parameter to false");
	    LOGGER.info("STEP 1: EXPECTED : Successfully set the value of parameter to false");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_MANAGEABLE_NOTIFICATION_ENABLE,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully set the value of parameter to false");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Unable to get latest image for current box model";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Trigger CDL upgrade device to latest stable build using mock XCONF");
	    LOGGER.info("STEP 2: ACTION : Configure mock server with latest image for estb mac of the device"
		    + " and trigger Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow as true");
	    LOGGER.info("STEP 2: EXPECTED : Successfully triggered image download to latest build");
	    LOGGER.info("**********************************************************************************");

	    currentImage = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    latestImage = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestImage);
	    if (CommonMethods.isNull(latestImage)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestImage = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestImage);
	    }

	    if (CommonMethods.isNotNull(latestImage)) {

		BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, latestImage, true,
			BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
		errorMessage = "Unable to trigger CDL latest build - " + latestImage;
		isCdlDataPosted = true;
		startTime = BroadBandCommonUtils.getEpochTimeInSecond(tapEnv, device);
		if (!DeviceModeHandler.isDSLDevice(device)) {
		    LOGGER.info("Triggering CDL using TR181");
		    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_TRIGGERING_XCONF_CDL,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);

		} else {
		    FirmwareDownloadUtils.triggerCdlUsingShellScriptForDSL(tapEnv, device);
		    status = true;
		}
	    }

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully triggered image download to latest stable build");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Unable to find 'http download started' log message in xconf.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify firmware download started notification not present in PAMlog");
	    LOGGER.info("STEP 3: ACTION : Execute commands: grep \"httpdownload started\" /rdklogs/logs/xconf.txt.0"
		    + " , grep -i \"firmware-download-started\" /rdklogs/logs/PAMlog.txt.0");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Firmware download started notification log message is not present in PAMlog after download started");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    FirmwareDownloadUtils.getCdlDownloadStartedLog(device),
		    BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.SIX_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
		errorMessage = "Able to find firmware-download-started log message in PAMlog when disabled";
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_FW_DWN_START_NOTIFY_NOT_SENT,
			BroadBandTestConstants.COMMAND_NTP_LOG_FILE));
	    }

	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Firmware download started notification log message is not present in PAMlog after download started");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Able to find firmware-download-started log message in PARODUSlog when disabled";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify firmware download started notification not present in PARODUSlog");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute command:grep -i \"firmware-download-started\" /rdklogs/logs/PARODUSlog.txt.0");
	    LOGGER.info(
		    "STEP 4: EXPECTED : Firmware download started notification log message not present in PARODUSlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_STARTED,
		    BroadBandCommandConstants.LOG_FILE_PARODUS));

	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Firmware download started notification log message not present in PARODUSlog");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify value of FirmwareDownloadStartedNotification parameter is not updated");
	    LOGGER.info("STEP 5: ACTION : Execute webpa or dmcli command to get value of "
		    + "Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification");
	    LOGGER.info("STEP 5: EXPECTED : Value of parameter is zero after firmware download started");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FIRMWARE_DOWNLOAD_STARTED_NOTIFICATION);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Value of parameter is non-zero after firmware download started when disabled";
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Value of parameter is zero after firmware download started");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Unable to find \"http download completed\" log message in xconf.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify firmware download completed notification not present in PAMlog");
	    LOGGER.info("STEP 6: ACTION : Execute commands:grep \"httpdownload completed\" /rdklogs/logs/xconf.txt.0"
		    + " , grep -i \"firmware-download-completed\" /rdklogs/logs/PAMlog.txt.0");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Firmware download completed notification log message is not present in PAMlog");
	    LOGGER.info("**********************************************************************************");

	    if (!DeviceModeHandler.isDSLDevice(device)) {
		if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			FirmwareDownloadUtils.getCdlDownloadCompletedLog(device),
			BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.SIX_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
		    errorMessage = "Able to find firmware-download-completed log message in PAMlog when disabled";
		    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_FW_DWN_COMPLETE_NOTIFY_NOT_SENT,
			    BroadBandTestConstants.COMMAND_NTP_LOG_FILE));
		}
	    } else {
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_HTTP_DOWNLOAD_COMPLETED,
			BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.SIX_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }
	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Firmware download completed notification log message is not present in PAMlog");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    if (!DeviceModeHandler.isDSLDevice(device)) {

		stepNum = "s7";
		errorMessage = "Able to find firmware-download-completed log message in PARODUSlog when disabled";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 7: DESCRIPTION : Verify firmware download completed notification not present in PARODUSlog");
		LOGGER.info(
			"STEP 7: ACTION : Execute command:grep -i \"firmware-download-completed\" /rdklogs/logs/PARODUSlog.txt.0");
		LOGGER.info(
			"STEP 7: EXPECTED : Firmware download completed notification log message is not present in PARODUSlog");
		LOGGER.info("**********************************************************************************");

		status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_COMPLETED,
			BroadBandCommandConstants.LOG_FILE_PARODUS));

		if (status) {
		    LOGGER.info(
			    "STEP 7: ACTUAL : Firmware download completed notification log message is not present in PARODUSlog");
		} else {
		    LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s8";
		errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 8: DESCRIPTION : Verify value of FirmwareDownloadCompletedNotification parameter is not updated");
		LOGGER.info("STEP 8: ACTION : Execute webpa or dmcli command to get value of "
			+ "Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification");
		LOGGER.info(
			"STEP 8: EXPECTED : Value of parameter is not updated to true after firmware download completed");
		LOGGER.info("**********************************************************************************");

		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAMETER_FIRMWARE_DOWNLOAD_COMPLETED_NOTIFICATION);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Value of parameter is true after firmware download completed when disabled";
		    status = !response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
		}

		if (status) {
		    LOGGER.info(
			    "STEP 8: ACTUAL : Value of parameter is not updated to true after firmware download completed");
		} else {
		    LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s9";
		errorMessage = "Unable to find reboot pending notification log message in PAMlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 9: DESCRIPTION : Verify reboot pending notification present without reboot reason in PAMlog");
		LOGGER.info("STEP 9: ACTION : Execute command:grep -i \"reboot-pending\" /rdklogs/logs/PAMlog.txt.0");
		LOGGER.info("STEP 9: EXPECTED : Reboot pending notification present without reboot reason in PAMlog");
		LOGGER.info("**********************************************************************************");

		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			BroadBandTestConstants.COMMAND_NTP_LOG_FILE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (CommonMethods.isNull(response)) {
		    response = tapEnv.searchAndGetTraceLineWithMatchingString(device, "reboot",
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		}

		status = CommonMethods.isNotNull(response) && (CommonMethods.patternMatcher(response,
			BroadBandTestConstants.UNKNOWN_REBOOT_REASON)
			|| CommonMethods.patternMatcher(response, BroadBandTestConstants.REBOOT_REASON_REBOOT_CMD)
			|| CommonMethods.patternMatcher(response,
				BroadBandCdlConstants.EXPECTED_LAST_REBOOT_REASON_STATUS_DIFD_CDL_VIA_WEBPA_REBOOT)
			|| CommonMethods.patternMatcher(response,
				BroadBandCdlConstants.EXPECTED_LAST_REBOOT_REASON_STATUS_AFTER_CDL)
			|| CommonMethods.patternMatcher(response, BroadBandTestConstants.REBOOT_REASON_FACTORY_RESET)
			|| CommonMethods.patternMatcher(response, BroadBandTestConstants.REBOOT_REASON_RFC_REBOOT)
			|| CommonMethods.patternMatcher(response,
				BroadBandTestConstants.RDKB_REBOOT_REASON_KERNEL_PANIC));

		if (status) {
		    LOGGER.info("STEP 9: ACTUAL : Reboot pending notification present without reboot reason in PAMlog");
		} else {
		    LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");
	    } else {
		stepCounter = 7;
		while (stepCounter <= 9) {
		    stepNum = "s" + stepCounter;
		    errorMessage = "This Step " + stepNum + " is not Applicable for DSL device";
		    LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		    stepCounter++;
		}
	    }
	    stepNum = "s10";
	    errorMessage = "Device did not go for reboot after code download complete and reboot immediately - true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Wait for device to reboot and verify it comes up with original image");
	    LOGGER.info("STEP 10: ACTION : Execute commands: echo test_connection , cat /version.txt");
	    LOGGER.info("STEP 10: EXPECTED : Device came up successfully after reboot with original image");
	    LOGGER.info("**********************************************************************************");
	    if (DeviceModeHandler.isDSLDevice(device)) {
		errorMessage = "Device not accessible after reboot for software upgrade";
		CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
		LOGGER.info("Waiting 1 minute");
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		errorMessage = "Image did not change after reboot for software upgrade";
		FirmwareDownloadUtils.verifyCurrentImageNameBothInArmAndAtomConsole(tapEnv, device, latestImage);
		status = true;
	    } else {
		if (CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
			BroadBandTestConstants.CONSTANT_8)) {
		    errorMessage = "Device not accessible after reboot for software upgrade";
		    if (CommonMethods.waitForEstbIpAcquisition(tapEnv, device)) {
			errorMessage = "Image did not change after reboot for software upgrade";
			status = CodeDownloadUtils.verifyImageVersionFromVersionText(tapEnv, device, latestImage);
		    }
		}
	    }
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Device came up successfully after reboot with original image");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.ManageableNotification.Enable";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Verify value of ManageableNotificationEnable remains false after reboot");
	    LOGGER.info("STEP 11: ACTION : Execute webpa or dmcli command to get value of "
		    + "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.ManageableNotification.Enable");
	    LOGGER.info("STEP 11: EXPECTED : Value of parameter persists false after reboot");
	    LOGGER.info("**********************************************************************************");

	    startTime = System.currentTimeMillis();
	    do {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAMETER_MANAGEABLE_NOTIFICATION_ENABLE);
	    } while (System.currentTimeMillis() - startTime < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
		    && CommonMethods.isNull(response)
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.ManageableNotification.Enable does not remain false after reboot";
		status = response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Value of parameter persists false after reboot");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Failed to verify webpa command working after reboot";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Verify WebPA process is up after reboot");
	    LOGGER.info("STEP 12: ACTION : Execute webpa command to get serial number");
	    LOGGER.info("STEP 12: EXPECTED : WebPA commands working after reboot");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);

	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : WebPA commands working after reboot");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s13";
	    errorMessage = "Able to find device fully-manageable notification log message present in PAMlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Verify device fully manageable notification is not present in PAMlog");
	    LOGGER.info("STEP 13: ACTION : Execute command:grep -i \"fully-manageable\" /rdklogs/logs/PAMlog.txt.0");
	    LOGGER.info("STEP 13: EXPECTED : Fully manageable notification is not present after reboot in PAMlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_FULLY_MANAGEABLE_NOTIFICATION,
		    BroadBandCommandConstants.LOG_FILE_PARODUS));

	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Fully manageable notification is not present after reboot in PAMlog");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s14";
	    errorMessage = "Able to find device fully-manageable notification log message present in PARODUSlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION : Verify device fully manageable notification is not present in PARODUSlog");
	    LOGGER.info(
		    "STEP 14: ACTION : Execute command:grep -i \"fully-manageable\" /rdklogs/logs/PARODUSlog.txt.0");
	    LOGGER.info("STEP 14: EXPECTED : Fully manageable notification is not present after reboot in PARODUSlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_FULLY_MANAGEABLE_NOTIFICATION,
		    BroadBandCommandConstants.LOG_FILE_PARODUS));

	    if (status) {
		LOGGER.info(
			"STEP 14: ACTUAL : Fully manageable notification is not present after reboot in PARODUSlog");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s15";
	    errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.DeviceManageableNotification";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION : Verify value of DeviceManageableNotification parameter is 0 when disabled after webpa is up");
	    LOGGER.info("STEP 15: ACTION : Execute webpa or dmcli command to get value of "
		    + "Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.DeviceManageableNotification, ");
	    LOGGER.info("STEP 15: EXPECTED : Value of parameter is zero after reboot and webpa up when disabled");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_DEVICE_MANAGEABLE_NOTIFICATION);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Value of parameter is non-zero after reboot when ManageableNotification is disabled";
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : Value of parameter is zero after reboot and webpa up when disabled");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    /**
	     * POST CONDITION 1 : CLEAR THE CDL INFORMATION IN XCONF SERVER
	     */
	    if (isCdlDataPosted) {
		postConStepNumber++;
		BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv,
			postConStepNumber);
	    }

	    String imageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);

	    if (CommonMethods.isNotNull(currentImage) && CommonMethods.isNotNull(imageName)
		    && !(currentImage.equalsIgnoreCase(imageName))) {
		status = false;
		errorMessage = "Unable to revert image back to original build";
		LOGGER.info("#######################################################################################");
		LOGGER.info(
			"POST-CONDITION 2 : DESCRIPTION : 1. Reset value of ManageableNotificationEnable to true 2. Revert image to original build if required");
		LOGGER.info("POST-CONDITION 2 : ACTION : Execute webpa or dmcli command to set value of "
			+ "ManageableNotificationEnable parameter to true, Revert image if required");
		LOGGER.info("POST-CONDITION 2 : EXPECTED : Post condition completed successfully");
		LOGGER.info("#######################################################################################");
		if (FirmwareDownloadUtils.triggerCdlUsingTr181OrTftp(tapEnv, device, currentImage)) {
		    errorMessage = "Failed to reset value of Manageable notification enable parameter to true";
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAMETER_MANAGEABLE_NOTIFICATION_ENABLE,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		}

		if (status) {
		    LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
		} else {
		    LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
		}
		LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	    LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-1019");
	}
    }

    /**
     * Verify Implementation of new WifiClient data model for harvester under Device.WiFi.
     * <ol>
     * <li>Verify whether WebPA is Up and Running in Device.</li>
     * <li>Verify setting WiFiClient.Enabled value via Webpa.</li>
     * <li>Verify setting WifiClient.ReportingPeriod value via Webpa.</li>
     * <li>Verify setting WifiClient.MacAddress value via Webpa.</li>
     * <li>Verify Factory Resetting and reactivating the device.</li>
     * <li>Verify getting default value of 'WifiClient.ReportingPeriod'.</li>
     * <li>Verify all the WiFi Client parameters default values after Factory Reset.</li>
     * <li>Verify setting WiFiClient.Enabled value via Webpa.</li>
     * <li>Verify setting WifiClient.ReportingPeriod value via Webpa.</li>
     * <li>Verify setting WifiClient.MacAddress value via Webpa.</li>
     * <li>Verify the WiFiClient parameters value persistence after reboot.</li>
     * <li>POST-CONDITION 1: Verify all WiFiClient parameter values set back to default.</li>
     * </ol>
     * 
     * @param device
     *            Dut
     * @author prashant.mishra
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-WEBPA-1018")
    public void testToVerifyNewWifiClientDataModel(Dut device) {
	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "S1";
	boolean status = false;
	boolean isFactoryReset = false;
	boolean areParamsDefault = false;
	WebPaServerResponse webPaServerResponse = null;
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-WEBPA-118";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1018");
	LOGGER.info(
		"TEST DESCRIPTION: Verify Implementation of new WifiClient data model for harvester under Device.WiFi.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify whether WebPA is Up and Running in Device.");
	LOGGER.info("2. Verify setting WiFiClient.Enabled value via Webpa.");
	LOGGER.info("3. Verify setting WifiClient.ReportingPeriod value via Webpa.");
	LOGGER.info("4. Verify setting WifiClient.MacAddress value via Webpa.");
	LOGGER.info("5. Verify Factory Resetting  and reactivating the device.");
	LOGGER.info("6. Verify getting default value of 'WifiClient.ReportingPeriod'.");
	LOGGER.info("7. Verify all the WiFi Client parameters default values after Factory Reset.");
	LOGGER.info("8. Verify setting WiFiClient.Enabled value via Webpa.");
	LOGGER.info("9. Verify setting WifiClient.ReportingPeriod value via Webpa.");
	LOGGER.info("10. Verify setting WifiClient.MacAddress value via Webpa.");
	LOGGER.info("11. Verify the WiFiClient parameters value persistence after reboot.");
	LOGGER.info("POST CONDITION 1. Verify all WiFiClient parameters values set back to default.");
	LOGGER.info("#######################################################################################");

	try {
	    stepNum = "S1";
	    errorMessage = "Webpa is not up in device.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify whether WebPA is Up and Running in Device.");
	    LOGGER.info(
		    "STEP 1: ACTION : Get Successful webpa Get response ,in case of failure rechecking for 8 minutes.");
	    LOGGER.info("STEP 1: EXPECTED : WebPA should be Up and Running in Device.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Webpa is up and running in the device.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "Unable to set the value of WiFi Client Enabled as 'True'.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify setting WiFiClient.Enabled value via Webpa.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the Webpa Set Command for parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled and set value as 'True'.");
	    LOGGER.info("STEP 2: EXPECTED : Device WiFiClient.Enabled value should be set to 'True' successfully.");
	    LOGGER.info("**********************************************************************************");
	    webPaServerResponse = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_ENABLE, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.CONSTANT_3);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : WiFi Client Enabled is set to 'True' successfully via Webpa.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S3";
	    errorMessage = "Unable to set the value of WifiClient.ReportingPeriod as '30'.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify setting WifiClient.ReportingPeriod value via Webpa.");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the Webpa Set Command for parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod and set value as '30'.");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Device WifiClient.ReportingPeriod Value should be set successfully via Webpa.");
	    LOGGER.info("**********************************************************************************");
	    areParamsDefault = status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_30);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : WifiClient.ReportingPeriod is set to '30' successfully via Webpa.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S4";
	    errorMessage = "";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify setting WifiClient.MacAddress value via Webpa.");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the Webpa Set Command for parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress");
	    LOGGER.info("STEP 4: EXPECTED : Device WifiClient.MacAddress Value should be set successfully via Webpa.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_MAC_ADDRESS, BroadBandTestConstants.CONSTANT_0,
		    AutomaticsPropertyUtility
			.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_MAC_ADDRESS_WIFICLIENT));
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Setting WifiClient.MacAddress value is successful via Webpa.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S5";
	    errorMessage = "Unable to Factory reset the device.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify Factory Resetting  and reactivating the device.");
	    LOGGER.info("STEP 5: ACTION : Factory reset the device and reactivate it.");
	    LOGGER.info("STEP 5: EXPECTED : Factory resetting the device should be successful.");
	    LOGGER.info("**********************************************************************************");
	    isFactoryReset = status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv,
		    device);
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Factory Resetting and Reactivating the device is succesful.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S6";
	    errorMessage = "Unable to get the default value of 'WifiClient.ReportingPeriod'.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify getting default value of 'WifiClient.ReportingPeriod'.");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the Webpa Get command with following parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.ReportingPeriod");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Webpa Get command should be successful and should get the default value of 'WifiClient.ReportingPeriod'.");
	    LOGGER.info("**********************************************************************************");
	    String defaultReportingPeriod = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_DEFAULT_REPORTING_PERIOD);
	    LOGGER.info("DEFAULT REPORTING PERIOD IS: " + defaultReportingPeriod);
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    BroadBandTestConstants.STRING_ZERO, defaultReportingPeriod);
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully got the default value of 'WifiClient.ReportingPeriod'.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S7";
	    errorMessage = "Values for WiFi Client parameters are not set to default after Factory Resetting.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify all the WiFi Client parameters default values after Factory Reset.");
	    LOGGER.info(
		    "STEP 7: ACTION : Verify Default values as WifiClient.Enabled: False, WifiClient.ReportingPeriod: 0, WifiClient.macAddress: 000000000000, WifiClient.Schema: WifiSingleClient.avsc");
	    LOGGER.info(
		    "STEP 7: EXPECTED : All values of WiFi Client parameters should be set to default after Factory Reset.");
	    LOGGER.info("**********************************************************************************");
	    String[] parameters = new String[] { BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_ENABLE,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_MAC_ADDRESS,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_SCHEMA };
	    String[] defaultvalues = new String[] { BroadBandTestConstants.FALSE, BroadBandTestConstants.STRING_ZERO,
		    BroadBandTestConstants.NULL_MAC_ADDRESS_WITHOUT_DELIMETER,
		    BroadBandTestConstants.WIFICLIENT_SCHEMA_TYPE };
	    status = BroadBandWebPaUtils.verifyWiFiClientDataModelDefaultValues(device, tapEnv, parameters,
		    defaultvalues);
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : WiFi Client parameters are set to default after Factory Resetting.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S8";
	    errorMessage = "Unable to set the value of WiFi Client Enabled as 'True'.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify setting WifiClient.ReportingPeriod value via Webpa.");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute the Webpa Set Command for parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled and set value as 'True'.");
	    LOGGER.info("STEP 8: EXPECTED : Device WiFiClient.Enabled value should be set to 'True' successfully.");
	    LOGGER.info("**********************************************************************************");
	    webPaServerResponse = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_ENABLE, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.CONSTANT_3);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : WiFi Client Enabled is set to 'True' successfully via Webpa.");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S9";
	    errorMessage = "Unable to set the value of WifiClient.ReportingPeriod as '30'.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify setting WifiClient.ReportingPeriod value via Webpa.");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the Webpa Set Command for parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod and set value as '30'.");
	    LOGGER.info(
		    "STEP 9: EXPECTED : Device WifiClient.ReportingPeriod Value should be set successfully via Webpa.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_30);
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : WifiClient.ReportingPeriod is set to '30' successfully via Webpa.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S10";
	    errorMessage = "";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify setting WifiClient.MacAddress value via Webpa.");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute the Webpa Set Command for parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress");
	    LOGGER.info("STEP 10: EXPECTED : Device WifiClient.MacAddress Value should be set successfully via Webpa.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_MAC_ADDRESS, BroadBandTestConstants.CONSTANT_0,
		    AutomaticsPropertyUtility
			.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_MAC_ADDRESS_WIFICLIENT));
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Setting WifiClient.MacAddress value is successful via Webpa.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S11";
	    errorMessage = "Device reboot is not successful.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify the WiFiClient parameters value persistence after reboot.");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the Webpa Get Command for Device WiFiClient parameters and verify the values persistence.");
	    LOGGER.info("STEP 11: EXPECTED : All WifiClient Parameters values should be persistent after reboot.");
	    LOGGER.info("**********************************************************************************");
	    String[] expectedValues = new String[] { BroadBandTestConstants.TRUE, BroadBandTestConstants.STRING_30,
	    		AutomaticsPropertyUtility
				.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_MAC_ADDRESS_WIFICLIENT),
		    BroadBandTestConstants.WIFICLIENT_SCHEMA_TYPE };
	    if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
		status = BroadBandWebPaUtils.verifyWiFiClientDataModelPersistence(device, tapEnv, parameters,
			expectedValues);
	    }
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : WiFiClient parameters persistence after reboot verified successfully.");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    errorMessage = "Unable to set all WiFiClient parameters value to default.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION : Verify all WiFiClient parameters values set back to default.");
	    LOGGER.info(
		    "POST-CONDITION 1: ACTION : Execute the Webpa Set command  for all WiFiClient parameters and set values to default.");
	    LOGGER.info(
		    "POST-CONDITION 1: EXPECTED : All Device WifiClient Parameters values should be set to default.");
	    LOGGER.info("**********************************************************************************");
	    if (areParamsDefault) {
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD,
			BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_ZERO)
			&& BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_MAC_ADDRESS,
				BroadBandTestConstants.CONSTANT_0,
				BroadBandTestConstants.NULL_MAC_ADDRESS_WITHOUT_DELIMETER);
	    }
	    if (status) {
		LOGGER.info("POST-CONDITION 1: ACTUAL : All WiFiClient parameters are set back to default.");
	    } else {
		LOGGER.error("POST-CONDITION 1: ACTUAL : " + errorMessage);
	    }

	    if (isFactoryReset) {
		errorMessage = "Unable to reactivate the device after Factory Reset.";
		LOGGER.info(
			"################# REACTIVATING THE DEVICE AFTER SUCCESSFUL FACTORY RESET #################");
		BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-1018");
    }

    /**
     * Verify firmware download event notifications with Manageable notification feature enabled
     * <ol>
     * <li>Verify ManageableNotification feature is enabled by default</li>
     * <li>Trigger CDL upgrade device to latest stable build using mock XCONF</li>
     * <li>Verify Firmware download started notification in xconf.txt.0</li>
     * <li>Verify firmware download started notification in PAMlog</li>
     * <li>Verify firmware download started notification in PARODUSlog</li>
     * <li>Verify value of FirmwareDownloadStartedNotification parameter is updated</li>
     * <li>Verify Firmware download completed notification in xconf.txt.0</li>
     * <li>Verify firmware download completed notification in PAMlog</li>
     * <li>Verify firmware download completed notification in PARODUSlog</li>
     * <li>Verify value of FirmwareDownloadCompletedNotification parameter is updated</li>
     * <li>Verify reboot pending notification with reboot reason software upgrade in PAMlog</li>
     * <li>Verify reboot pending notification in PARODUSlog after download complete</li>
     * <li>Wait for device to reboot and verify it comes up with new image</li>
     * <li>Verify value of DeviceManageableNotification parameter is 0 before device is fully manageable. If not, verify
     * device manageable notfication is present in PARODUSlog</li>
     * <li>Verify WebPA process is up after reboot</li>
     * <li>Verify value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification is
     * reset to zero after reboot</li>
     * <li>Verify value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification
     * is reset to false after reboot</li>
     * <li>Verify device fully manageable notification is present in PAMlog</li>
     * <li>Verify device fully manageable notification is present in PARODUSlog</li>
     * </ol>
     * 
     * @author Ashwin sankara
     * @author ArunKumar Jayachandran
     * @Refactor Alan_Bivera
     * 
     * @param device
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-1011")
    public void testVerifyFirmwareDownloadEventNotificationEnabled(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBPA-111";
	String stepNum = "s1";
	String response = null;
	String errorMessage = null;
	String currentImage = null;
	String latestImage = null;
	int stepCounter = BroadBandTestConstants.CONSTANT_0;
	long startTime = BroadBandTestConstants.CONSTANT_0;
	boolean status = false;
	boolean isCdlDataPosted = false;
	int postConStepNumber = BroadBandTestConstants.CONSTANT_0;
	String cdlStartTime = null;
	String currentCdlBuild = null;
	String latestCdlBuild = null;
	String priorityValue = null;
	boolean featureAvailableBuild = false;
	int preConStepNumber = 1;
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1011");
	LOGGER.info(
		"TEST DESCRIPTION: Verify firmware download event notifications  with Manageable notification feature enabled");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify ManageableNotification feature is enabled by default");
	LOGGER.info("2. Trigger CDL upgrade device to latest stable build using mock XCONF");
	LOGGER.info("3. Verify Firmware download started notification in xconf.txt.0");
	LOGGER.info("4. Verify firmware download started notification in PAMlog");
	LOGGER.info("5. Verify firmware download started notification in PARODUSlog");
	LOGGER.info("6. Verify value of FirmwareDownloadStartedNotification parameter is updated");
	LOGGER.info("7. Verify Firmware download completed notification in xconf.txt.0");
	LOGGER.info("8. Verify firmware download completed notification in PAMlog");
	LOGGER.info("9. Verify firmware download completed notification in PARODUSlog");
	LOGGER.info("10. Verify value of FirmwareDownloadCompletedNotification parameter is updated");
	LOGGER.info("11. Verify reboot pending notification with reboot reason software upgrade in PAMlog");
	LOGGER.info("12. Verify reboot pending notification in PARODUSlog after download complete");
	LOGGER.info("13. Wait for device to reboot and verify it comes up with new image");
	LOGGER.info(
		"14. Verify value of DeviceManageableNotification parameter is 0 before device is fully manageable. "
			+ "If not, verify device manageable notfication is present in PARODUSlog");
	LOGGER.info("15. Verify WebPA process is up after reboot");
	LOGGER.info(
		"16. Verify value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification"
			+ " is reset to zero after reboot");
	LOGGER.info(
		"17. Verify value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification"
			+ " is reset to false after reboot");
	LOGGER.info("18. Verify device fully manageable notification is present in PAMlog");
	LOGGER.info("19. Verify device fully manageable notification is present in PARODUSlog");
	LOGGER.info("20. Verify value of DeviceManageableNotification has been set after device is fully manageable");
	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv, preConStepNumber);

	    /**
	     * PRE-CONDITION 2 : Pre-Condition to disable periodic firmware upgrade.
	     */
	    preConStepNumber++;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("PRE-CONDITION  " + preConStepNumber
		    + ": DESCRIPTION : Disable periodic firmware upgrade using WebPA");
	    LOGGER.info("PRE-CONDITION  " + preConStepNumber
		    + ": ACTION : Execute Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.PeriodicFWCheck.Enable parameter using WebPA/Dmcli");
	    LOGGER.info("PRE-CONDITION " + preConStepNumber
		    + ": EXPECTED : WebPA should return success message with value false");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to disable Periodic firmware check";
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE);
	    status = CommonMethods.isNotNull(response) && Boolean.parseBoolean(response);
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE, BroadBandTestConstants.FALSE);
	    } catch (TestException exception) {
		status = false;
		LOGGER.error(errorMessage + " : " + exception.getMessage());
	    }
	    if (!status) {
		status = false;
		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_PERIODIC_FW_CHECK_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
			BroadBandTestConstants.FALSE);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION " + preConStepNumber
			+ " : ACTUAL : SUCCESSFULLY DISABLED PERIODIC FIRMWARE CHECK PARAMETER");
	    } else {
		LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : "
			+ preConStepNumber + " FAILED : " + errorMessage);
	    }
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.ManageableNotification.Enable";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify ManageableNotification feature is enabled by default");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute webpa or dmcli command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.ManageableNotification.Enable");
	    LOGGER.info("STEP 1: EXPECTED : Value of ManageableNotification parameter is true by default");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_MANAGEABLE_NOTIFICATION_ENABLE);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.ManageableNotification.Enable is not true by default";
		status = response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
	    }
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PAMLOGS_NVRAM);
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PARODUSLOGS_NVRAM);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Value of ManageableNotification parameter is true by default");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Unable to get latest image for current box model";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Trigger CDL upgrade device to latest build using mock XCONF");
	    LOGGER.info("STEP 2: ACTION : Configure mock server with latest stable image for estb mac of the device"
		    + " and trigger Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow as true");
	    LOGGER.info("STEP 2: EXPECTED : Successfully triggered image download to latest build");
	    LOGGER.info("**********************************************************************************");

	    currentImage = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    latestImage = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestImage);
	    if (CommonMethods.isNull(latestImage)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestImage = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestImage);
	    }

	    if (CommonMethods.isNotNull(latestImage)) {

		BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, latestImage, true,
			BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
		errorMessage = "Unable to trigger CDL latest build - " + latestImage;
		isCdlDataPosted = true;
		startTime = BroadBandCommonUtils.getEpochTimeInSecond(tapEnv, device);
		if (!DeviceModeHandler.isDSLDevice(device)) {
		    LOGGER.info("Triggering CDL using TR181");
		    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_TRIGGERING_XCONF_CDL,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);

		} else {
		    FirmwareDownloadUtils.triggerCdlUsingShellScriptForDSL(tapEnv, device);
		    status = true;
		}
	    }

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully triggered image download to latest stable build");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Unable to find ### httpdownload started ### log message in xconf.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify Firmware download started notification in xconf.txt.0");
	    LOGGER.info("STEP 3: ACTION : Execute grep commands to search for 'httpdownload started' and"
		    + " 'FirmwareDownloadStartedNotification' in /rdklogs/logs/xconf.txt.0");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Firmware download started notification log message is present in xconf.txt.0");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    FirmwareDownloadUtils.getCdlDownloadStartedLog(device),
		    BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
		errorMessage = "Unable to find FirmwareDownloadStartedNotification log message in "
			+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_STARTED_NOTIFICATION,
			BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
	    }

	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Firmware download started notification log message is present in xconf.txt.0");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Unable to find firmware-download-started log message in PAMlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify firmware download started notification in PAMlog");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute command: grep -i \"firmware-download-started\" /rdklogs/logs/PAMlog.txt.0 and verify priority, current_fw_ver, download_fw_ver fields, start time fields and verify the priority value is forced");
	    LOGGER.info(
		    "STEP 4: EXPECTED : Firmware download started notification log message is present in PAMlog and all the fields should be present in the log");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_STARTED,
		    BroadBandTestConstants.COMMAND_NTP_LOG_FILE);
	    featureAvailableBuild = BroadBandCommonUtils.verifyFeatureAvailabilityInBuild(tapEnv, device,
		    tapEnv.getSTBPropsValue(BroadBandTestConstants.PROP_KEY_JAIL_UI_FEATURE));
	    if (featureAvailableBuild) {
		if (CommonMethods.isNotNull(response)) {
		    String jsonStr = CommonMethods.patternFinder(response,
			    BroadBandTestConstants.PATTERN_GET_XCONF_PAYLOAD);
		    if (CommonMethods.isNotNull(jsonStr)) {
			JSONObject objectName = new JSONObject(jsonStr);
			if (objectName.has(BroadBandTestConstants.STATUS)
				&& objectName.has(BroadBandTestConstants.PRIORITY)
				&& objectName.has(BroadBandTestConstants.START_TIME)
				&& objectName.has(BroadBandTestConstants.CURRENT_FW_VER)
				&& objectName.has(BroadBandTestConstants.DOWNLOAD_FW_VER)) {
			    cdlStartTime = objectName.getString(BroadBandTestConstants.START_TIME).toString();
			    currentCdlBuild = objectName.getString(BroadBandTestConstants.CURRENT_FW_VER).toString();
			    latestCdlBuild = objectName.getString(BroadBandTestConstants.DOWNLOAD_FW_VER).toString();
			    priorityValue = objectName.getString(BroadBandTestConstants.PRIORITY).toString();
			    if (CommonMethods.isNotNull(priorityValue) && CommonMethods.isNotNull(cdlStartTime)
				    && CommonMethods.isNotNull(latestCdlBuild)
				    && CommonMethods.isNotNull(currentCdlBuild)) {
				status = objectName.get(BroadBandTestConstants.STATUS).toString()
					.equalsIgnoreCase(BroadBandTestConstants.FW_DWN_STARTED)
					&& priorityValue
						.equalsIgnoreCase(BroadBandTestConstants.FW_DWN_PRIORITY_FORCED);
			    }

			}
		    }
		}
	    } else {
		status = CommonMethods.isNotNull(response);
	    }

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Firmware download started notification log message is present in PAMlog");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Unable to find firmware-download-started log message in PARODUSlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify firmware download started notification in PARODUSlog");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute command:grep -i \"firmware-download-started\" /rdklogs/logs/PARODUSlog.txt.0");
	    LOGGER.info(
		    "STEP 5: EXPECTED : Firmware download started notification log message is present in PARODUSlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_STARTED,
		    BroadBandCommandConstants.LOG_FILE_PARODUS));

	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : Firmware download started notification log message is present in PARODUSlog");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify value of FirmwareDownloadStartedNotification parameter is updated");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute webpa or dmcli command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification and check the response values are same as step 4 field values");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Value of parameter is updated with start time, priority, current fw version, download fw version");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FIRMWARE_DOWNLOAD_STARTED_NOTIFICATION);

	    if (CommonMethods.isNotNull(response)) {
		if (featureAvailableBuild) {
		    errorMessage = "Value of parameter is not matched with log message in PAM log file";
		    status = CommonUtils.isGivenStringAvailableInCommandOutput(response, priorityValue)
			    && CommonUtils.isGivenStringAvailableInCommandOutput(response, latestCdlBuild)
			    && CommonUtils.isGivenStringAvailableInCommandOutput(response, currentCdlBuild)
			    && CommonUtils.isGivenStringAvailableInCommandOutput(response, cdlStartTime);
		} else {
		    errorMessage = "Value of parameter is less than firmware download start time";
		    status = Long.parseLong(response) > startTime;
		}
	    }
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully verified all the parameters in PAM log file");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Unable to find ### httpdownload completed ### log message in xconf.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify Firmware download completed notification in xconf.txt.0");
	    LOGGER.info("STEP 7: ACTION : Execute commands: grep \"httpdownload completed\" /rdklogs/logs/xconf.txt.0"
		    + " , grep \"FirmwareDownloadCompletedNotification\" /rdklogs/logs/xconf.txt.0");
	    LOGGER.info(
		    "STEP 7: EXPECTED : Firmware download completed notification log message is present in xconf.txt.0");
	    LOGGER.info("**********************************************************************************");

	    if (!DeviceModeHandler.isDSLDevice(device)) {
		if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			FirmwareDownloadUtils.getCdlDownloadCompletedLog(device),
			BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.SIX_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
		    errorMessage = "Unable to find FirmwareDownloadCompletedNotification log message in "
			    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
		    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_COMPLETED_NOTIFICATION,
			    BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0,
			    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
		}
	    } else {
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_HTTP_DOWNLOAD_COMPLETED,
			BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.SIX_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }
	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL : Firmware download completed notification log message is present in xconf.txt.0");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");
	    if (!DeviceModeHandler.isDSLDevice(device)) {

		stepNum = "s8";
		errorMessage = "Unable to find firmware-download-completed log message in PAMlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 8: DESCRIPTION : Verify firmware download completed notification in PAMlog");
		LOGGER.info(
			"STEP 8: ACTION : Execute command:grep -i \"firmware-download-completed\" /rdklogs/logs/PAMlog.txt.0");
		LOGGER.info(
			"STEP 8: EXPECTED : Firmware download completed notification log message is present in PAMlog");
		LOGGER.info("**********************************************************************************");

		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_COMPLETED,
			BroadBandTestConstants.COMMAND_NTP_LOG_FILE));

		if (status) {
		    LOGGER.info(
			    "STEP 8: ACTUAL : Firmware download completed notification log message is present in PAMlog");
		} else {
		    LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s9";
		errorMessage = "Unable to find firmware-download-completed log message in PARODUSlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 9: DESCRIPTION : Verify firmware download completed notification in PARODUSlog");
		LOGGER.info(
			"STEP 9: ACTION : Execute command:grep -i \"firmware-download-completed\" /rdklogs/logs/PARODUSlog.txt.0");
		LOGGER.info(
			"STEP 9: EXPECTED : Firmware download completed notification log message is present in PARODUSlog");
		LOGGER.info("**********************************************************************************");

		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_FIRMWARE_DOWNLOAD_COMPLETED,
			BroadBandCommandConstants.LOG_FILE_PARODUS));

		if (status) {
		    LOGGER.info(
			    "STEP 9: ACTUAL : Firmware download completed notification log message is present in PARODUSlog");
		} else {
		    LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s10";
		errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 10: DESCRIPTION : Verify value of FirmwareDownloadCompletedNotification parameter is updated");
		LOGGER.info("STEP 10: ACTION : Execute webpa or dmcli command to get value of"
			+ " Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification");
		LOGGER.info(
			"STEP 10: EXPECTED : Value of parameter is updated to true after firmware download completed");
		LOGGER.info("**********************************************************************************");

		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAMETER_FIRMWARE_DOWNLOAD_COMPLETED_NOTIFICATION);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Value of parameter is not true after firmware download completed";
		    status = response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
		}

		if (status) {
		    LOGGER.info(
			    "STEP 10: ACTUAL : Value of parameter is updated to true after firmware download completed");
		} else {
		    LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s11";
		errorMessage = "Unable to find reboot pending notification log message in PAMlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 11: DESCRIPTION : Verify reboot pending notification with reboot reason software upgrade in PAMlog");
		LOGGER.info("STEP 11: ACTION : Execute command:grep -i \"reboot-pending\" /rdklogs/logs/PAMlog.txt.0");
		LOGGER.info(
			"STEP 11: EXPECTED : Reboot pending notification is present in PAMlog with reason software upgrade");
		LOGGER.info("**********************************************************************************");

		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			BroadBandTestConstants.COMMAND_NTP_LOG_FILE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (CommonMethods.isNull(response)) {
		    response = tapEnv.searchAndGetTraceLineWithMatchingString(device,
			    BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		}

		status = CommonMethods.isNotNull(response) && (CommonMethods.patternMatcher(response,
			BroadBandTestConstants.UNKNOWN_REBOOT_REASON)
			|| CommonMethods.patternMatcher(response, BroadBandTestConstants.REBOOT_REASON_REBOOT_CMD)
			|| CommonMethods.patternMatcher(response,
				BroadBandCdlConstants.EXPECTED_LAST_REBOOT_REASON_STATUS_DIFD_CDL_VIA_WEBPA_REBOOT)
			|| CommonMethods.patternMatcher(response,
				BroadBandCdlConstants.EXPECTED_LAST_REBOOT_REASON_STATUS_AFTER_CDL)
			|| CommonMethods.patternMatcher(response, BroadBandTestConstants.REBOOT_REASON_FACTORY_RESET)
			|| CommonMethods.patternMatcher(response, BroadBandTestConstants.REBOOT_REASON_RFC_REBOOT));
		if (!status && CommonMethods.waitForEstbIpAcquisition(tapEnv, device)) {

		    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			    BroadBandCommandConstants.FILE_PATH_NVRAM_PAM_TAIL);
		    status = CommonMethods.isNotNull(response);
		}

		if (status) {
		    LOGGER.info(
			    "STEP 11: ACTUAL : Reboot pending notification is present in PAMlog with reason software upgrade");
		} else {
		    LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");

		stepNum = "s12";
		errorMessage = "Unable to find reboot pending log message in PARODUSlog";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 12: DESCRIPTION : Verify reboot pending notification in PARODUSlog after download complete");
		LOGGER.info(
			"STEP 12: ACTION : Execute command:grep -i \"reboot-pending\" /rdklogs/logs/PARODUSlog.txt.0");
		LOGGER.info("STEP 12: EXPECTED : Reboot pending notification is present in PARODUSlog");
		LOGGER.info("**********************************************************************************");

		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			BroadBandCommandConstants.LOG_FILE_PARODUS));
		if (!status) {
		    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_REBOOT_PENDING_NOTIFICATION,
			    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL);
		    status = CommonMethods.isNotNull(response);
		}

		if (status) {
		    LOGGER.info("STEP 12: ACTUAL : Reboot pending notification is present in PARODUSlog");
		} else {
		    LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("**********************************************************************************");
	    } else {
		stepCounter = 8;
		while (stepCounter <= 12) {
		    stepNum = "s" + stepCounter;
		    errorMessage = "This Step " + stepNum + " is not Applicable for DSL device";
		    LOGGER.error("STEP " + stepCounter + ": ACTUAL : " + errorMessage);
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		    LOGGER.info("**********************************************************************************");
		    stepCounter++;
		}
	    }
	    stepNum = "s13";
	    errorMessage = "Device did not go for reboot after code download complete and reboot immediately - true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Wait for device to reboot and verify it comes up with new image");
	    LOGGER.info("STEP 13: ACTION : Execute commands: echo test_connection , cat /version.txt");
	    LOGGER.info("STEP 13: EXPECTED : Device came up successfully after reboot with new image");
	    LOGGER.info("**********************************************************************************");
	    if (DeviceModeHandler.isDSLDevice(device)) {
		errorMessage = "Device not accessible after reboot for software upgrade";
		CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
		LOGGER.info("Waiting 1 minute");
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		errorMessage = "Image did not change after reboot for software upgrade";
		FirmwareDownloadUtils.verifyCurrentImageNameBothInArmAndAtomConsole(tapEnv, device, latestImage);
		status = true;
	    } else {
		if (CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
			BroadBandTestConstants.CONSTANT_8)) {
		    errorMessage = "Device not accessible after reboot for software upgrade";
		    if (CommonMethods.waitForEstbIpAcquisition(tapEnv, device)) {
			errorMessage = "Image did not change after reboot for software upgrade";
			status = CodeDownloadUtils.verifyImageVersionFromVersionText(tapEnv, device, latestImage);
		    }
		}
	    }
	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Device came up successfully after reboot with new image");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s14";
	    errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.DeviceManageableNotification";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION : Verify value of DeviceManageableNotification parameter is 0 before device is fully manageable."
			    + " If not, verify device manageable notfication is present in PARODUSlog");
	    LOGGER.info("STEP 14: ACTION : Execute webpa or dmcli command to get value of "
		    + "Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.DeviceManageableNotification, "
		    + "If not 0, Execute command:grep -i \"fully-manageable\" /rdklogs/logs/PARODUSlog.txt.0");
	    LOGGER.info(
		    "STEP 14: EXPECTED : Value of parameter is zero after reboot or non-zero after it is fully-manageable");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_DEVICE_MANAGEABLE_NOTIFICATION);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Value of parameter is non-zero when device fully manageable notification is not sent";
		if (!response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO)) {
		    status = CommonMethods
			    .isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
				    BroadBandTraceConstants.LOG_MESSAGE_FULLY_MANAGEABLE_NOTIFICATION,
				    BroadBandCommandConstants.LOG_FILE_PARODUS))
			    || CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
				    BroadBandTraceConstants.LOG_MESSAGE_FULLY_MANAGEABLE_NOTIFICATION,
				    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));
		}
	    }
	    startTime = System.currentTimeMillis();
	    do {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAMETER_DEVICE_MANAGEABLE_NOTIFICATION);
		if (CommonMethods.isNotNull(response)) {
		    errorMessage = "Value of parameter is non-zero when device fully manageable notification is not sent";
		    status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
		}
	    } while (System.currentTimeMillis() - startTime < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS && !status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    // Checking if device is fully manageable when parameter has non zero value
	    if (!status) {
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_FULLY_MANAGEABLE_NOTIFICATION,
			BroadBandTestConstants.COMMAND_NTP_LOG_FILE));
	    }

	    if (status) {
		LOGGER.info(
			"STEP 14: ACTUAL : Value of parameter is zero after reboot or non-zero after it is fully-manageable");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s15";
	    errorMessage = "Failed to verify webpa command working after reboot";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION : Verify WebPA process is up after reboot");
	    LOGGER.info("STEP 15: ACTION : Execute webpa command to get serial number");
	    LOGGER.info("STEP 15: EXPECTED : WebPA commands working after reboot");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);

	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : WebPA commands working after reboot");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s16";
	    errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 16: DESCRIPTION : Verify value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification"
			    + " is reset to zero after reboot");
	    LOGGER.info("STEP 16: ACTION : Execute webpa or dmcli command to get value of "
		    + "Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadStartedNotification");
	    LOGGER.info("STEP 16: EXPECTED : Value of parameter is zero after reboot");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FIRMWARE_DOWNLOAD_STARTED_NOTIFICATION);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Value of parameter is non-zero after reboot";
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 16: ACTUAL : Value of parameter is zero after reboot");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s17";
	    errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 17: DESCRIPTION : Verify value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification"
			    + " is reset to false after reboot");
	    LOGGER.info("STEP 17: ACTION : Execute webpa or dmcli command to get value of "
		    + "Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.FirmwareDownloadCompletedNotification");
	    LOGGER.info("STEP 17: EXPECTED : Value of parameter is false after reboot");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FIRMWARE_DOWNLOAD_COMPLETED_NOTIFICATION);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Value of parameter is not false after reboot";
		status = response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : Value of parameter is false after reboot");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s18";
	    errorMessage = "Unable to find device fully-manageable notification log message present in PAMlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION : Verify device fully manageable notification is present in PAMlog");
	    LOGGER.info("STEP 18: ACTION : Execute command:grep -i \"fully-manageable\" /rdklogs/logs/PAMlog.txt.0");
	    LOGGER.info("STEP 18: EXPECTED : Fully manageable notification is present after reboot in PAMlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods
		    .isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_FULLY_MANAGEABLE_NOTIFICATION,
			    BroadBandTestConstants.COMMAND_NTP_LOG_FILE))
		    || CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_FULLY_MANAGEABLE_NOTIFICATION,
			    BroadBandCommandConstants.FILE_PATH_NVRAM_PAM_TAIL));

	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : Fully manageable notification is present after reboot in PAMlog");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s19";
	    errorMessage = "Unable to find device fully-manageable notification log message present in PARODUSlog";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 19: DESCRIPTION : Verify device fully manageable notification is present in PARODUSlog");
	    LOGGER.info(
		    "STEP 19: ACTION : Execute command:grep -i \"fully-manageable\" /rdklogs/logs/PARODUSlog.txt.0");
	    LOGGER.info("STEP 19: EXPECTED : Fully manageable notification is present after reboot in PARODUSlog");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods
		    .isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_FULLY_MANAGEABLE_NOTIFICATION,
			    BroadBandCommandConstants.LOG_FILE_PARODUS))
		    || CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_FULLY_MANAGEABLE_NOTIFICATION,
			    BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));
	    ;

	    if (status) {
		LOGGER.info("STEP 19: ACTUAL : Fully manageable notification is present after reboot in PARODUSlog");
	    } else {
		LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s20";
	    errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_BootTime";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 20: DESCRIPTION : Verify value of DeviceManageableNotification has been set after device is fully manageable");
	    LOGGER.info(
		    "STEP 20: ACTION : Execute webpa or dmcli command to get value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.DeviceManageableNotification");
	    LOGGER.info("STEP 20: EXPECTED : Value of DeviceManageableNotification parameter updated successfully");
	    LOGGER.info("**********************************************************************************");

	    if (!DeviceModeHandler.isDSLDevice(device)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_BOOTTIME);
		if (CommonMethods.isNotNull(response)) {
		    startTime = Long.parseLong(response);
		    errorMessage = "Failed to obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.RPC.DeviceManageableNotification";
		    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAMETER_DEVICE_MANAGEABLE_NOTIFICATION);
		    if (CommonMethods.isNotNull(response)) {
			errorMessage = "Value of DeviceManageableNotification is not updated to value greater than boot time after device is fully manageable";
			status = Long.parseLong(response) > startTime;
		    }
		}

		if (status) {
		    LOGGER.info(
			    "STEP 20: ACTUAL : Value of DeviceManageableNotification parameter updated successfully");
		} else {
		    LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    } else {
		errorMessage = "This Step " + stepNum + " is not Applicable for DSL device";
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    /**
	     * POST CONDITION 1 : CLEAR THE CDL INFORMATION IN XCONF SERVER
	     */
	    if (isCdlDataPosted) {
		postConStepNumber++;
		BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv,
			postConStepNumber);
	    }

	    String imageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);

	    if (CommonMethods.isNotNull(currentImage) && CommonMethods.isNotNull(imageName)
		    && !(currentImage.equalsIgnoreCase(imageName))) {
		status = false;
		errorMessage = "Unable to revert image back to original build";
		LOGGER.info("#######################################################################################");
		LOGGER.info(
			"POST-CONDITION 2 : DESCRIPTION : 1. Reset value of ManageableNotificationEnable to true 2. Revert image to original build if required");
		LOGGER.info("POST-CONDITION 2 : ACTION : Execute webpa or dmcli command to set value of "
			+ "ManageableNotificationEnable parameter to true, Revert image if required");
		LOGGER.info("POST-CONDITION 2 : EXPECTED : Post condition completed successfully");
		LOGGER.info("#######################################################################################");
		if (FirmwareDownloadUtils.triggerCdlUsingTr181OrTftp(tapEnv, device, currentImage)) {
		    errorMessage = "Failed to reset value of Manageable notification enable parameter to true";
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAMETER_MANAGEABLE_NOTIFICATION_ENABLE,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		}

		if (status) {
		    LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
		} else {
		    LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
		}
		LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-1011");
    }

    /**
     * 
     * Send WebPA Sync Notification when selected data model parameters are changed
     * <ol>
     * <li>Perform factory reset on device (To make device with initial setup and default values)</li>
     * <li>Verify notification is turned ON for parameter
     * \"Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable\" in WEBPA.log.txt</li>
     * <li>Verify notification is turned ON for parameter
     * \"Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\" in WEBPA.log.txt</li>
     * <li>Verify notification is turned ON for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_CloudUIEnable\" in
     * WEBPA.log.txt</li>
     * <li>Verify notification is turned ON for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_AkerEnable\" in
     * WEBPA.log.txt</li>
     * <li>Verify notification is turned ON for parameter \"Device.MoCA.Interface.1.Enable\" in WEBPA.log.txt</li>
     * <li>Verify notify attribute is turned on for parameter
     * \"Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable\" using WEBPA</li>
     * <li>Verify notify attribute is turned on for parameter
     * \"Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\" using WEBPA</li>
     * <li>Verify notify attribute is turned on for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_CloudUIEnable\" using
     * WEBPA</li>
     * <li>Verify notify attribute is turned on for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_AkerEnable\" using
     * WEBPA</li>
     * <li>Verify notify attribute is turned on for parameter \"Device.MoCA.Interface.1.Enable\" using WEBPA</li>
     * <li>SET value as False for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA</li>
     * <li>SET value as False Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA</li>
     * <li>Reboot the device and check webpa process is up after boot up</li>
     * <li>GET value as False for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA</li>
     * <li>Verify in WEBPA log that CUJO/finger print is disabled for the device</li>
     * <li>GET value as False Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA</li>
     * <li>Verify notify attribute is turned ON for parameter
     * \"Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client\" using WEBPA</li>
     * <li>Verify in WEBPA log for MESH disabled Scenario</li>
     * <li>Collect CPU and memory usage stats for 10 minutes when feature is disabled</li>
     * <li>SET value as True for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA</li>
     * <li>SET value as True Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA</li>
     * <li>Collect CPU and memory usage stats for 10 minutes when feature is enabled</li>
     * <li>Compare the results from Step 22 and Step 25</li>
     * <li>Reboot the device and check webpa process is up after boot up</li>
     * <li>GET value as TRUE for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA</li>
     * <li>Verify notify attribute is turned on for parameter
     * \"Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable\" using WEBPA</li>
     * <li>Verify notify attribute is turned on for parameter
     * \"Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable\" using WEBPA</li>
     * <li>Verify in WEBPA log that CUJO/finger print is enabled for the device</li>
     * <li>GET value for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA</li>
     * <li>Verify notify attribute is turned OFF for parameter
     * \"Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client\" using WEBPA</li>
     * <li>Verify in WEBPA log for MESH enabled Scenario</li>
     * <li>SET value as True Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA</li>
     * <li>Verify in WEBPA.log.txt.o for notification payload</li>
     * </ol>
     * 
     * @author Betel Costrow
     * @Refactor Sruthi Santhosh
     * 
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
	    BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-1016")
    public void testToVerifyWebpaSyncNotification(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBPA-116";
	String stepNum = "s1";
	String beforeEnablingFeature = null;
	String afterEnablingFeature = null;
	String errorMessage = "Device not coming up after factory reset";
	boolean status = false;
	String response = null;
	String defaultFingerprintStatus = null;
	String defaultMeshStatus = null;
	// Variable Declation Ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1016");
	LOGGER.info("TEST DESCRIPTION: Send WebPA Sync Notification when selected data model parameters are changed");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Perform factory reset on device (To make device with initial setup and default values)");
	LOGGER.info(
		"2. Verify notification is turned ON for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable\" in WEBPA.log.txt");
	LOGGER.info(
		"3. Verify notification is turned ON for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\" in WEBPA.log.txt");
	LOGGER.info(
		"4. Verify notification is turned ON for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_CloudUIEnable\" in WEBPA.log.txt");
	LOGGER.info(
		"5. Verify notification is turned ON for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_AkerEnable\" in WEBPA.log.txt");
	LOGGER.info(
		"6. Verify notification is turned ON for parameter \"Device.MoCA.Interface.1.Enable\" in WEBPA.log.txt");
	LOGGER.info(
		"7. Verify notify attribute is turned on for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable\" using WEBPA ");
	LOGGER.info(
		"8. Verify notify attribute is turned on for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\" using WEBPA ");
	LOGGER.info(
		"9. Verify notify attribute is turned on for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_CloudUIEnable\" using WEBPA ");
	LOGGER.info(
		"10. Verify notify attribute is turned on for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_AkerEnable\" using WEBPA ");
	LOGGER.info(
		"11. Verify notify attribute is turned on for parameter \"Device.MoCA.Interface.1.Enable\" using WEBPA ");
	LOGGER.info(
		"12. SET value as False for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	LOGGER.info("13. SET value as False Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA");
	LOGGER.info("14. Reboot the device and check webpa process is up after boot up");
	LOGGER.info(
		"15. GET value as False for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	LOGGER.info("16.Verify in WEBPA log that CUJO/finger print is disabled for the device");
	LOGGER.info("17.GET value as False Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA");
	LOGGER.info(
		"18.Verify notify attribute is turned ON for parameter \"Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client\" using WEBPA");
	LOGGER.info("19.Verify in WEBPA log for MESH disabled Scenario");
	LOGGER.info("20.Collect CPU and memory usage stats for 10 minutes when feature is disabled");
	LOGGER.info("21.SET value as True for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	LOGGER.info("22.SET value as True Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA");
	LOGGER.info("23.Collect CPU and memory usage stats for 10 minutes when feature is enabled");
	LOGGER.info("24.Compare the results from Step 22 and Step 25");
	LOGGER.info("25.Reboot the device and check webpa process is up after boot up");
	LOGGER.info("26.GET value as TRUE for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	LOGGER.info(
		"27.Verify notify attribute is turned on for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable\" using WEBPA");
	LOGGER.info(
		"28.Verify notify attribute is turned on for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable\" using WEBPA");
	LOGGER.info("29.Verify in WEBPA log that CUJO/finger print is enabled for the device");
	LOGGER.info("30.GET value for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA");
	LOGGER.info(
		"31.Verify notify attribute is turned OFF for parameter \"Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client\" using WEBPA");
	LOGGER.info("32.Verify in WEBPA log for MESH enabled Scenario");
	LOGGER.info("33.SET value as True Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	LOGGER.info("34.Verify in WEBPA.log.txt.o for notification payload");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Perform factory reset on device (To make device with initial setup and default values)");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute webpa set command:Parameter: Device.X_CISCO_COM_DeviceControl.FactoryResetdata type: stringvalue: \"Router,Wifi,VoIP,Dect,MoCA\"");
	    LOGGER.info("STEP 1: EXPECTED : Device should come up after factory reset");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);
	    if (status) {
		BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
		defaultFingerprintStatus = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE);
		defaultMeshStatus = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
		LOGGER.info("STEP 1: ACTUAL : Successfully performed factory reset.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    String parameter = null;
	    for (int count = 2; count <= 6; count++) {
		if (count == 2) {
		    parameter = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE;
		} else if (count == 3) {
		    parameter = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE;
		} else if (count == 4) {
		    parameter = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CLOUD_UI;
		} else if (count == 5) {
		    parameter = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_AKER_ENABLE;
		} else if (count == 6) {
		    parameter = BroadBandWebPaConstants.WEBPA_PARAM_ENABLE_MOCA;
		}
		stepNum = "s" + count;
		errorMessage = "Notification is not SET ON for parameter " + parameter;
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + count + ": DESCRIPTION : Verify notification is turned ON for parameter "
			+ parameter + " in WEBPA.log.txt");
		LOGGER.info("STEP " + count
			+ ": ACTION : Execute command:grep -i \"Successfully set notification ON for parameter : \""
			+ parameter + " /rdklogs/logs/WEBPAlog.txt.0");
		LOGGER.info("STEP " + count
			+ ": EXPECTED : Should get a response as Successfully set notification ON for parameter : "
			+ parameter + " ret: 0");
		LOGGER.info("**********************************************************************************");
		if (DeviceModeHandler.isDSLDevice(device) && count == 6) {
		    LOGGER.info("Step 6 Executing" + device);
		    errorMessage = "MOCA is not applicable for DSL device";
		    LOGGER.info("STEP 6: ACTUAL : " + errorMessage);
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {
		    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
			    BroadBandTraceConstants.WEBPALOG_CONSTANT_FOR_NOTIFICATION_ON_STATUS + parameter,
			    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		    status = CommonMethods.isNotNull(response);
		    if (status) {
			LOGGER.info("STEP " + count
				+ ": ACTUAL : Verified expected message in /rdklogs/logs/WEBPAlog.txt.0");
		    } else {
			LOGGER.error("STEP " + count + ": ACTUAL : " + errorMessage);
		    }
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		    LOGGER.info("**********************************************************************************");
		}
	    }
	    for (int count = 7; count <= 11; count++) {
		if (count == 7) {
		    parameter = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE;
		} else if (count == 8) {
		    parameter = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE;
		} else if (count == 9) {
		    parameter = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CLOUD_UI;
		} else if (count == 10) {
		    parameter = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_AKER_ENABLE;
		} else if (count == 11) {
		    parameter = BroadBandWebPaConstants.WEBPA_PARAM_ENABLE_MOCA;
		}
		stepNum = "s" + count;
		errorMessage = "Notification is not SET ON for parameter " + parameter;
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + count + ": DESCRIPTION : Verify notification is turned ON for parameter "
			+ parameter + " using WEBPA");
		LOGGER.info("STEP " + count + ": ACTION : Execute command:dmcli eRT getattr " + parameter);
		LOGGER.info("STEP " + count
			+ ": EXPECTED : Should get a response as Successfully set notification ON for parameter : "
			+ parameter);
		LOGGER.info("**********************************************************************************");
		if (DeviceModeHandler.isDSLDevice(device) && count == 11) {
		    LOGGER.info("Step 11 Executing" + device);
		    errorMessage = "MOCA is not applicable for DSL device";
		    LOGGER.info("STEP 11: ACTUAL : " + errorMessage);
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {
		    status = BroadBandWebPaUtils.executeWebPaCommandGetAttribute(tapEnv, device, parameter,
			    BroadBandTestConstants.NOTIFY + AutomaticsConstants.COLON
				    + BroadBandTestConstants.STRING_VALUE_ONE);
		    if (status) {
			LOGGER.info("STEP " + count + ": ACTUAL : Successfully verified expected attribute");
		    } else {
			LOGGER.error("STEP " + count + ": ACTUAL : " + errorMessage);
		    }
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		    LOGGER.info("**********************************************************************************");
		}
	    }
	    stepNum = "s12";
	    errorMessage = "Value of parameter is not getting set as False for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : SET value as False for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute command:curl -X PATCH  <WEBPA_URL>/api/v2/device/mac:<MAC ADDRESS>/config -d \"{\"parameters\":[{\"name\":\"Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\",\"value\":\"True\",\"dataType\":3}]}\"-H \"Content-Type:application/json\" -H \"Accept:application/json\" -H \"AuThorization:Bearer    <Authtoken>\"");
	    LOGGER.info(
		    "STEP 12: EXPECTED : Value of parameter should set as False for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.FALSE);
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Successfully disabled Device finger print.");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s13";
	    errorMessage = "Value of parameter is not getting set as false for dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION : SET value as False Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA");
	    LOGGER.info(
		    "STEP 13: ACTION : Execute command:curl -X PATCH <WEBPA_URL>/api/v2/device/mac:<MAC ADDRESS>/config -d \"{\"parameters\":[{\"name\":\"dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable\",\"value\":\"true\",\"dataType\":3}]}\" -H\"Content-Type:application/json\" -H \"Accept:application/json\" -H \"AuThorization:Bearer <Authtoken>\"");
	    LOGGER.info(
		    "STEP 13: EXPECTED : Value of parameter should set as false for dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Successfully verified Mesh is enabled by default.");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s14";
	    errorMessage = "Device not coming up after reboot";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION : Reboot the device and check webpa process is up after boot up ");
	    LOGGER.info("STEP 14: ACTION : Execute command:1.reboot2.pidof webpa");
	    LOGGER.info("STEP 14: EXPECTED : Device should get rebooted and process should come up");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : Successfully performed reboot");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s15";
	    errorMessage = "Value of parameter is not getting set as False for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION : GET value as False for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	    LOGGER.info(
		    "STEP 15: ACTION : Execute command:curl -X PATCH <WEBPA_URL>/api/v2/device/mac:<MAC ADDRESS>/config -d \"{\"parameters\":[{\"name\":\"Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\",\"value\":\"True\",\"dataType\":3}]}\" -H \"Content-Type:application/json\" -H \"Accept:application/json\" -H \"AuThorization:Bearer <Authtoken>\"");
	    LOGGER.info(
		    "STEP 15: EXPECTED : Value of parameter should get as False for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE, BroadBandTestConstants.FALSE);
	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : Successfully verified Device finger print is disabled");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s16";
	    errorMessage = "\"WEBPA: Fingerprint/cujo is disabled. Removing Advance Security parameters fromnotification list\" log string is not returned";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION : Verify in WEBPA log that CUJO/finger print is disabled for the device");
	    LOGGER.info(
		    "STEP 16: ACTION : Execute command:cat /rdklogs/logs/WEBPAlog.txt.0 | grep -i Removing Advance Security parameters from notification list");
	    LOGGER.info(
		    "STEP 16: EXPECTED : \"WEBPA: Fingerprint/cujo is disabled. Removing Advance Security parameters from notification list\" log string should be returned");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.LOG_MSG_FOR_REMOVED_SECURITY_PARAMETERS_FROM_NOTIFICATION_LIST,
		    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		LOGGER.info("STEP 16: ACTUAL : Verified expected message in /rdklogs/logs/WEBPAlog.txt.0");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s17";
	    errorMessage = "Value of parameter is not getting set as false for dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 17: DESCRIPTION : GET value as False Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable  using WEBPA");
	    LOGGER.info(
		    "STEP 17: ACTION : Execute command:curl -X PATCH <WEBPA_URL>/api/v2/device/mac:<MAC ADDRESS>/config -d \"{\"parameters\":[{\"name\":\"dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable\",\"value\":\"true\",\"dataType\":3}]}\" -H "
			    + "\"Content-Type:application/json\" -H \"Accept:application/json\" -H \"AuThorization:Bearer <Authtoken>\"");
	    LOGGER.info(
		    "STEP 17: EXPECTED : Value of parameter should get as false for dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE, BroadBandTestConstants.FALSE);
	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : Successfully verified Mesh is disabled");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s18";
	    errorMessage = "Notification attribute is not SET ON for parameter  Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 18: DESCRIPTION : Verify notify attribute is turned ON for parameter \"Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client\" using WEBPA ");
	    LOGGER.info(
		    "STEP 18: ACTION : Execute command:curl -i -H \"Authorization:Bearer ${WEBPA_SAT}\" -H \"Accept:  application/json\" -w %{time_total} -k \"<WEBPA_URL>/api/v2/device/mac:<MAC_ADDRESS>/config?names=Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client&attributes=not");
	    LOGGER.info(
		    "STEP 18: EXPECTED : Notification should be turned ON for the parameter : Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.executeWebPaCommandGetAttribute(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_NOTIFY_COMPONENT_CONNECTED_CLIENT,
		    BroadBandTestConstants.NOTIFY + AutomaticsConstants.COLON
			    + BroadBandTestConstants.STRING_VALUE_ONE);
	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : Successfully verified expected attribute");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s19";
	    errorMessage = "WEBPA: Successfully set notification ON for parameter : Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client ret: 0 log string is not returned";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 19: DESCRIPTION : Verify in WEBPA log for MESH disabled Scenario");
	    LOGGER.info(
		    "STEP 19: ACTION : Execute command:cat /rdklogs/logs/WEBPAlog.txt.0 | grep -i \"Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client\"");
	    LOGGER.info(
		    "STEP 19: EXPECTED : \"WEBPA: Successfully set notification ON for parameter :  Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client ret: 0\" log string should be returned");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.WEBPALOG_CONSTANT_FOR_NOTIFICATION_ON_STATUS
			    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_NOTIFY_COMPONENT_CONNECTED_CLIENT,
		    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		LOGGER.info("STEP 19: ACTUAL : Verified expected message in /rdklogs/logs/WEBPAlog.txt.0");
	    } else {
		LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s20";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info(
		    "STEP 20: DESCRIPTION: Collect CPU and memory usage stats for 10 minutes when feature is disabled");
	    LOGGER.info(
		    "STEP 20: ACTION: a) execute the following command inside the RG console of the gateway for every one  minute and collect the data for CPU and memory usage, "
			    + "\"top -n 1 |grep -i Mem |sed 's/^[^0-9]//;s/[^0-9].*$//'\" and \"top -n 1 |grep CPU: |sed 's/^[^0-9]//;s/[^0-9].*$//'\"\n b) Calculate the average for the data collected ");
	    LOGGER.info("STEP 20: EXPECTED: Command execution on the device should be successful");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Unable to collect CPU and memory usage data";
	    response = BroadBandWifiWhixUtils.calculateAverageCpuAndMemoryUtilisation(device, tapEnv,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    LOGGER.info("STEP 20: Response for CPU and memory utilisation: " + response);
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		beforeEnablingFeature = response;
		LOGGER.info(
			"STEP 20: ACTUAL : Calculating the average CPU and Memory utilisation for 10 minutes before enabling logging");
	    } else {
		LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s21";
	    errorMessage = "Value of parameter is not getting set as True for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 21: DESCRIPTION : SET value as True for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable  using WEBPA");
	    LOGGER.info(
		    "STEP 21: ACTION : Execute command:curl -X PATCH <WEBPA_URL>/api/v2/device/mac:<MAC ADDRESS>/config -d\"{\"parameters\":[{\"name\":\"Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\",\"value\":\"True\",\"dataType\":3}]}\" -H \"Content-Type:application/json\" -H \"Accept:application/json\" -H \"AuThorization:Bearer <Authtoken>\"");
	    LOGGER.info(
		    "STEP 21: EXPECTED : Value of parameter should set as True for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info("STEP 21: ACTUAL : Successfully verified Device finger print is enabled");
	    } else {
		LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s22";
	    errorMessage = "Value of parameter is not getting set as true for dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 22: DESCRIPTION : SET value as True Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable  using WEBPA");
	    LOGGER.info(
		    "STEP 22: ACTION : Execute command:curl -X PATCH <WEBPA_URL>/api/v2/device/mac:<MAC ADDRESS>/config -d\"{\"parameters\":[{\"name\":\"dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable\",\"value\":\"true\",\"dataType\":3}]}\" -H \"Content-Type:application/json\" -H \"Accept:application/json\" -H \"AuThorization:Bearer <Authtoken>\"");
	    LOGGER.info(
		    "STEP 22: EXPECTED : Value of parameter should set as true for dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandMeshUtils.enableOrDisableMesh(device, tapEnv, true);
	    if (status) {
		LOGGER.info("STEP 22: ACTUAL : Successfully verified Mesh is enabled.");
	    } else {
		LOGGER.error("STEP 22: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s23";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info(
		    "STEP 23: DESCRIPTION: Collect CPU and memory usage stats for 10 minutes when feature is enabled");
	    LOGGER.info(
		    "STEP 23: ACTION: a) execute the following command inside the RG console of the gateway for every one minute and collect the data for CPU and memory usage, "
			    + "\"top -n 1 |grep -i Mem |sed 's/^[^0-9]/;s/[^0-9].*$//'\" and \"top -n 1 |grep CPU: |sed's/^[^0-9]/;s/[^0-9].*$//'\"\n b) Calculate the average for the data collected ");
	    LOGGER.info("STEP 23: EXPECTED: Command execution on the device should be successful");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Unable to collect CPU and memory usage data";
	    response = BroadBandWifiWhixUtils.calculateAverageCpuAndMemoryUtilisation(device, tapEnv,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    LOGGER.info("STEP 23: Response for CPU and memory utilisation: " + response);
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		afterEnablingFeature = response;
		LOGGER.info(
			"STEP 23: ACTUAL : Calculating the average CPU and Memory utilisation for 10 minutes after enabling log");
	    } else {
		LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s24";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 24: DESCRIPTION: Compare the results from Step 22 and Step 25");
	    LOGGER.info("STEP 24: ACTION: Compare the averages calculated for CPU utilisation and memory utilisation");
	    LOGGER.info(
		    "STEP 24: EXPECTED: The difference in average should be within 10%, indicating that the feature doesn't have any negative impact on the device");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "The feature causes negative impact on the device";
	    BroadBandResultObject bandResultObject = null;
	    bandResultObject = BroadBandWifiWhixUtils
		    .validateCpuAndMemoryUtilisationForNegativeEffect(beforeEnablingFeature, afterEnablingFeature);
	    if (bandResultObject.isStatus()) {
		LOGGER.info("STEP 24: ACTUAL : There is no negative impact on the device when this feature is enabled");
	    } else {
		LOGGER.error("STEP 24: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, bandResultObject.isStatus(),
		    bandResultObject.getErrorMessage(), true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s25";
	    errorMessage = "Device not coming up after reboot";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 25: DESCRIPTION : Reboot the device and check webpa process is up after boot up ");
	    LOGGER.info("STEP 25: ACTION : Execute command:1.reboot2.pidof webpa");
	    LOGGER.info("STEP 25: EXPECTED : Device should get rebooted and process should come up");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    if (status) {
		LOGGER.info("STEP 25: ACTUAL : Successfully performed reboot");
	    } else {
		LOGGER.error("STEP 25: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s26";
	    errorMessage = "Value of parameter is not getting set as True for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 26: DESCRIPTION : GET value as TRUE for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	    LOGGER.info(
		    "STEP 26: ACTION : Execute command:curl -X PATCH <WEBPA_URL>/api/v2/device/mac:<MAC ADDRESS>/config -d \"{\"parameters\":[{\"name\":\"Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\",\"value\":\"True\",\"dataType\":3}]}\" -H \"Content-Type:application/json\" -H \"Accept:application/json\" -H \"AuThorization:Bearer <Authtoken>\"");
	    LOGGER.info(
		    "STEP 26: EXPECTED : Value of parameter should get as True for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE, BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info("STEP 26: ACTUAL : Successfully verified Device finger print is enabled");
	    } else {
		LOGGER.error("STEP 26: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s27";
	    errorMessage = "Notification attribute is not SET ON for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 27: DESCRIPTION : Verify notify attribute is turned on for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable\" using WEBPA ");
	    LOGGER.info(
		    "STEP 27: ACTION : Execute command:curl -i -H \"Authorization:Bearer ${WEBPA_SAT}\" -H \"Accept: application/json\" -w %{time_total} -k\"<WEBPA_URL>/api/v2/device/mac:<MAC_ADDRESS>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable&attributes=not");
	    LOGGER.info(
		    "STEP 27: EXPECTED : Notification should be turned ON for the parameter :  Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable ");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.executeWebPaCommandGetAttribute(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SOFTFLOWD, BroadBandTestConstants.NOTIFY
			    + AutomaticsConstants.COLON + BroadBandTestConstants.STRING_VALUE_ONE);
	    if (status) {
		LOGGER.info("STEP 27: ACTUAL : Successfully verified expected attribute");
	    } else {
		LOGGER.error("STEP 27: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s28";
	    errorMessage = "Notification attribute is not SET ON for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 28: DESCRIPTION : Verify notify attribute is turned on for parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable\" using WEBPA ");
	    LOGGER.info(
		    "STEP 28: ACTION : Execute command:curl -i -H \"Authorization:Bearer ${WEBPA_SAT}\" -H \"Accept: application/json\" -w %{time_total} -k \"<WEBPA_URL>/api/v2/device/mac:<MAC_ADDRESS>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable&attributes=not");
	    LOGGER.info(
		    "STEP 28: EXPECTED : Notification should be turned ON for the parameter : Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.executeWebPaCommandGetAttribute(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SAFE_BROWSING, BroadBandTestConstants.NOTIFY
			    + AutomaticsConstants.COLON + BroadBandTestConstants.STRING_VALUE_ONE);
	    if (status) {
		LOGGER.info("STEP 28: ACTUAL : Successfully verified expected attribute");
	    } else {
		LOGGER.error("STEP 28: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s29";
	    errorMessage = "WEBPA: Successfully set notification ON for parameter : Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable ret: 0 log string is not returned";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 29: DESCRIPTION : Verify in WEBPA log that CUJO/finger print is enabled for the device");
	    LOGGER.info(
		    "STEP 29: ACTION : Execute command: grep -i \"Successfully set notification ON for parameter : Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\" /rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info(
		    "STEP 29: EXPECTED : \"WEBPA: Successfully set notification ON for parameter : Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable ret: 0\" log string should be returned");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.WEBPALOG_CONSTANT_FOR_NOTIFICATION_ON_STATUS
			    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE,
		    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		LOGGER.info("STEP 29: ACTUAL : Verified expected message in /rdklogs/logs/WEBPAlog.txt.0");
	    } else {
		LOGGER.error("STEP 29: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s30";
	    errorMessage = "Value of parameter is not returned as True for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 30: DESCRIPTION : GET value for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using  WEBPA");
	    LOGGER.info(
		    "STEP 30: ACTION : Execute command:curl \"<webpa_url>/api/v2/device/mac:<MAC  ADDRESS>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable\" -H \"AuThorization: Bearer <Authtoken>\"");
	    LOGGER.info(
		    "STEP 30: EXPECTED : Value of parameter should return as True for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable using WEBPA");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE, BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info("STEP 30: ACTUAL : Successfully verified mesh value after reboot");
	    } else {
		LOGGER.error("STEP 30: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s31";
	    errorMessage = "Notification attribute is not SET OFF for parameter Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 31: DESCRIPTION : Verify notify attribute is turned OFF for parameter \"Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client\" using WEBPA ");
	    LOGGER.info(
		    "STEP 31: ACTION : Execute command:curl -i -H \"Authorization:Bearer ${WEBPA_SAT}\" -H \"Accept:application/json\" -w %{time_total} -k \"<WEBPA_URL>/api/v2/device/mac:<MAC_ADDRESS>/config?names=Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client&attributes=not");
	    LOGGER.info(
		    "STEP 31: EXPECTED : Notification should be turned OFF for the parameter : Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.executeWebPaCommandGetAttribute(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_NOTIFY_COMPONENT_CONNECTED_CLIENT,
		    BroadBandTestConstants.NOTIFY + AutomaticsConstants.COLON + BroadBandTestConstants.STRING_ZERO);
	    if (status) {
		LOGGER.info("STEP 31: ACTUAL : Successfully verified expected attribute");
	    } else {
		LOGGER.error("STEP 31: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s32";
	    errorMessage = "WEBPA: Removing Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client from notification list log string is not returned";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 32: DESCRIPTION : Verify in WEBPA log for MESH enabled Scenario");
	    LOGGER.info(
		    "STEP 32: ACTION : Execute command: grep -i \"Removing Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client from notification\" /rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info(
		    "STEP 32: EXPECTED : \"WEBPA: Removing Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client from notification list\" log string should be returned");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.LOG_MSG_FOR_REMOVED_CLIENT_FROM_NOTIFICATION_LIST,
		    BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		LOGGER.info("STEP 32: ACTUAL : Verified expected message in /rdklogs/logs/WEBPAlog.txt.0");
	    } else {
		LOGGER.error("STEP 32: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s33";
	    errorMessage = "Value of parameter is not getting set as True for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 33: DESCRIPTION : SET value as True for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	    LOGGER.info(
		    "STEP 33: ACTION : Execute command:curl -X PATCH <WEBPA_URL>/api/v2/device/mac:<MAC ADDRESS>/config -d\"{\"parameters\":[{\"name\":\"Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable\",\"value\":\"True\",\"dataType\":3}]}\"-H \"Content-Type:application/json\" -H \"Accept:application/json\" -H \"AuThorization:Bearer <Authtoken>\"");
	    LOGGER.info(
		    "STEP 33: EXPECTED : Value of parameter should set as True for Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable using WEBPA");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info("STEP 33: ACTUAL : Successfully verified Device finger print is enabled");
	    } else {
		LOGGER.error("STEP 33: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");
	    stepNum = "s34";
	    errorMessage = "Values are not  getting logged in WEBPA.log.txt.0";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 34: DESCRIPTION : Verify in WEBPA.log.txt.o for notification payload");
	    LOGGER.info("STEP 34: ACTION : Execute command:1.cat /rdklogs/logs/webpa.txt.0");
	    LOGGER.info("STEP 34: EXPECTED : Value stirng should be Notification payload");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.LOG_MSG_NOTIFICATION_PAYLOAD, BroadBandCommandConstants.LOG_FILE_WEBPA,
		    BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.isGivenStringAvailableInCommandOutput(response, "Notification payload");
	    if (status) {
		LOGGER.info(
			"STEP 34: ACTUAL : Successfully verified notification payload message logged in /rdklogs/logs/webpa.txt.0");
	    } else {
		LOGGER.error("STEP 34: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    boolean postCondition = false;
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION : DESCRIPTION : Change finger print and mesh parmeters to default status");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : 1)dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable bool status "
			    + "2)dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable bool status ");
	    LOGGER.info("POST-CONDITION : EXPECTED : Finger print and Mesh should change to default value.");
	    if (CommonMethods.isNotNull(defaultMeshStatus) && CommonMethods.isNotNull(defaultFingerprintStatus)) {
		tapEnv.executeCommandUsingSsh(device,
			"dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable bool "
				+ defaultMeshStatus);
		postCondition = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE,
			BroadBandTestConstants.CONSTANT_3, defaultFingerprintStatus)
			&& tapEnv
				.executeCommandUsingSsh(device,
					"dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable")
				.contains(defaultMeshStatus);
	    }
	    if (postCondition) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + postCondition);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-1016");
    }

    /**
     * Test to verify the wireless client connected to 5 GHz Private Wi-Fi network is blocked for all days & for
     * particular time period
     * 
     * <ol>
     * <li>PRE-CONDITION: Verify whether Private 5 GHz SSID 'Device.WiFi.SSID.10001.Enable' is enabled using WebPA</li>
     * <li>STEP 1: Connect the device to 5 GHz SSID and verify connection status</li>
     * <li>STEP 2: Check if the wirless connected client has an IP address from the gateway</li>
     * <li>STEP 3: Getting the Wifi Mac address of Connected client having 5GHZ wifi Capability</li>
     * <li>STEP 4: Verify the Managed devices is enabled via WebPA</li>
     * <li>STEP 5: Verify the 'Allow All' under Managed devices is set to true via WebPA</li>
     * <li>STEP 6: Verify the client connected to 5 Ghz Private Wi-Fi is added to blocked list for all days</li>
     * <li>STEP 7: Verify if the connected client is not having internet access via gateway</li>
     * <li>STEP 8: Verify the client connected to 5 Ghz Private Wi-Fi is removed from blocked list</li>
     * <li>STEP 9: Verify the Managed devices is disabled via WebPA</li>
     * <li>STEP 10: Verify if the connected client has internet access via gateway</li>
     * <li>STEP 11: Verify the Managed devices is enabled via WebPA</li>
     * <li>STEP 12: Verify the 'Allow All' under Managed devices is set to true via WebPA</li>
     * <li>STEP 13: Verify the client connected to 5 Ghz Private Wi-Fi is added to blocked list for particular time
     * period</li>
     * <li>STEP 14: Verify if the connected client is not having internet access via gateway</li>
     * <li>STEP 15: Verify the Managed devices is disabled via WebPA</li>
     * <li>STEP 16: Verify if the connected ethernet client has internet access via gateway</li>
     * <li>POST-CONDITION: Verify the client connected to 5 Ghz Private Wi-Fi is removed from blocked list</li>
     * </ol>
     * 
     * @param device
     * 
     * @refactor yamini.s
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WEBPA)
    @TestDetails(testUID = "TC-RDKB-PC-MANAGE-DEVICE-1001")
    public void testParentalControlManageDevicesFunctionalityFor5GhzConnectedClient(Dut device) {

	String testId = "TC-RDKB-PC-MANAGE-DEVICE-101";
	String testStepNumber = "s1";
	String errorMessage = null;
	boolean status = false;
	Dut connectedDeviceActivated = null; // connected device to be verified
	String wifiMacAddress = null;
	boolean isEnabled = false;
	String parentalControlManageDeviceTableAddRowResponse = null;

	try {

	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-PC-MANAGE-DEVICE-1001 #####################");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verification of Parental Control - Manage Devices Functionality - Blocking of 5 GHz Private Wi-Fi connected device for \"All Days\" and/or for \"Particular time period\"");

	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info(
		    "PRE-CONDITION: Verify whether Private 5 GHz SSID 'Device.WiFi.SSID.10001.Enable' is enabled using WebPA");
	    LOGGER.info("1. Connect the device to 5 GHz SSID and verify connection status");
	    LOGGER.info("2. Check if the wirless connected client has an IP address from the gateway");
	    LOGGER.info("3. Getting the Wifi Mac address of Connected client having 5GHZ wifi Capability");
	    LOGGER.info("4. Verify the Managed devices is enabled via WebPA");
	    LOGGER.info("5. Verify the 'Allow All' under Managed devices is set to true via WebPA");
	    LOGGER.info("6. Verify the client connected to 5 Ghz Private Wi-Fi is added to blocked list for all days");
	    LOGGER.info("7. Verify if the connected client is not having internet access via gateway");
	    LOGGER.info("8. Verify the client connected to 5 Ghz Private Wi-Fi is removed from blocked list");
	    LOGGER.info("9. Verify the Managed devices is disabled via WebPA");
	    LOGGER.info("10. Verify if the connected client has internet access via gateway");
	    LOGGER.info("11. Verify the Managed devices is enabled via WebPA");
	    LOGGER.info("12. Verify the 'Allow All' under Managed devices is set to true via WebPA");
	    LOGGER.info(
		    "13. Verify the client connected to 5 Ghz Private Wi-Fi is added to blocked list for particular time period");
	    LOGGER.info("14. Verify if the connected client is not having internet access via gateway");
	    LOGGER.info("15. Verify the Managed devices is disabled via WebPA");
	    LOGGER.info("16. Verify if the connected ethernet client has internet access via gateway");
	    LOGGER.info(
		    "POST-CONDITION: Verify the client connected to 5 Ghz Private Wi-Fi is removed from blocked list");
	    LOGGER.info("#####################################################################################");

	    LOGGER.info("############################# STARTING PRE-CONFIGURATIONS #############################");
	    LOGGER.info("PRE-CONDITION: DESCRIPTION: Verify whether 5 GHz Private SSID is enabled using WebPA");
	    LOGGER.info("PRE-CONDITION: EXPECTED: 5 GHz Private Wi-Fi SSID should be enabled using WebPA");
	    status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
		    WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, true);
	    if (!status) {
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
			+ "NOT ABLE TO ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
	    }
	    LOGGER.info("PRE-CONDITION : ACTUAL: ENABLING 5GHZ PRIVATE SSID ON THIS DEVICE IS SUCCESSFUL");

	    LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");

	    /**
	     * Step 1: Connect the device to 5 GHz SSID and verify connection status
	     * 
	     */
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 1: Connect the device to 5 GHz SSID and verify connection status");
	    LOGGER.info("STEP 1: EXPECTED: Device should be connected with 5 GHz wifi network");
	    connectedDeviceActivated = BroadBandConnectedClientUtils
		    .get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
	    status = null != connectedDeviceActivated;
	    errorMessage = "Unable to connect to 5 GHz private Wi-Fi Network Or 5 GHz WiFi capable devices are not available";
	    if (status) {
		LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		LOGGER.info("S1 ACTUAL: Device has been connected with 5 GHz private Wi-Fi network");
	    } else {
		LOGGER.error("S1 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 2: Check if the wireless connected client has an IP address from the gateway
	     *
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 2: Check if the wireless connected client has an IP address from the gateway");
	    LOGGER.info(
		    "STEP 2: EXPECTED: DHCP Range IP Address should be assigned to 5 GHz Wireless Connected device");
	    status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
		    connectedDeviceActivated);
	    errorMessage = "Cilent connected to 5 GHz private Wi-Fi haven't received valid IP Address from Gateway";
	    if (status) {
		LOGGER.info(
			"S2 ACTUAL: Client connected to 5 Ghz private Wi-Fi network has got IP Address from Gateway");
	    } else {
		LOGGER.error("S2 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 3: Getting the Wifi Mac address of Connected client having 5GHZ wifi Capability
	     *
	     */
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 3: Getting the Wifi Mac address of Connected client having 5GHZ wifi Capability");
	    LOGGER.info("STEP 3: EXPECTED: Mac address of the Wi-Fi adapter should be retrieved successfully");
	    wifiMacAddress = ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress();
	    LOGGER.info(
		    "Wifi Mac Address of the Connected client having 5GHZ Capability obtained is : " + wifiMacAddress);
	    status = CommonMethods.isNotNull(wifiMacAddress);
	    errorMessage = "Unable to retrieve the Wifi Mac address of the connected client having 5 GHz wifi Capability OR WiFi-Mac Address is not configured properly in MDS/Inventory";
	    if (status) {
		LOGGER.info(
			"S3 ACTUAL: Successfully retrieved the Wifi Mac address of the connected client having 5 GHz wifi Capability");
	    } else {
		LOGGER.error("S3 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 4: Verify the Parental Control - Manage Devices is enabled via WebPA
	     *
	     */
	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 4: Verify the Parental Control - Manage Devices is enabled via WebPA");
	    LOGGER.info("STEP 4: EXPECTED: Parental Control - Manage Devices should be enabled via WebPA");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROL_MANAGED_DEVICES_FEATURE,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    isEnabled = status;
	    errorMessage = "Parental Control - Manage Devices cannot be enabled via WebPa";
	    if (status) {
		LOGGER.info("S4 ACTUAL: Parental Control - Manage Devices has been successfully enabled via WebPA");
	    } else {
		LOGGER.error("S4 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 5: Verify the 'Allow All' under Parental Control - Manage Devices is set to true via WebPA
	     *
	     */
	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 5: Verify the 'Allow All' under Parental Control - Manage Devices is set to true via WebPA ");
	    LOGGER.info("STEP 5: EXPECTED: Allow All should be set to true");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROL_ALLOW_ALL,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    errorMessage = "'Allow All' under Parental Control - Manage Devices cannot be enable via WebPA";
	    if (status) {
		LOGGER.info(
			"S5 ACTUAL: 'Allow All' under Parental Control - Manage Devices has been set to 'true' via WebPA");
	    } else {
		LOGGER.error("S5 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 6: Verify the client connected to 5 Ghz Private Wi-Fi is added to blocked list for all days
	     *
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 6: Verify the client connected to 5 Ghz Private Wi-Fi is added to blocked list for all days");
	    LOGGER.info(
		    "STEP 6: EXPECTED: The 5Ghz Connected client details should be added under blocked device list for all days");
	    parentalControlManageDeviceTableAddRowResponse = BroadBandParentalControlUtils
		    .addConnectedClientToBlockList(tapEnv, device,
			    BroadBandTestConstants.PARENTAL_CONTROL_MANAGE_DEVICE_RULE_TYPE,
			    BroadBandTestConstants.CONNECTION_TYPE_WIFI_5_GHZ, wifiMacAddress,
			    BroadBandTestConstants.TRUE, BroadBandTestConstants.EMPTY_STRING,
			    BroadBandTestConstants.EMPTY_STRING, BroadBandTestConstants.EMPTY_STRING);
	    errorMessage = "Null response obtained for setting Parental Control - Managed Device Rule";
	    if (CommonMethods.isNotNull(parentalControlManageDeviceTableAddRowResponse)) {
		status = CommonUtils.patternSearchFromTargetString(parentalControlManageDeviceTableAddRowResponse,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PARENTAL_CONTROL_MANAGED_DEVICES_TABLE);
		errorMessage = "Rule to block internet access to the client connected to 5 GHz private Wi-Fi network cannot be added";
	    }
	    if (status) {
		LOGGER.info(
			"S6 ACTUAL: Rule to block internet access to the client connected to 5 GHz private Wi-Fi network has been added successfully for all days");
	    } else {
		LOGGER.error("S6 ACTUAL: " + errorMessage + " ACTUAL RESPONSE: "
			+ parentalControlManageDeviceTableAddRowResponse);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 7: Verify if the connected client is not having internet access via gateway
	     *
	     */
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 7: Verify if the connected client is not having internet access via gateway");
	    LOGGER.info("STEP 7: EXPECTED: Ping to the destination host should fail");
	    LOGGER.info("################## Waiting for 90 seconds to reflect the changes ####################");
	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	    status = !ConnectedNattedClientsUtils.verifyPingConnection(connectedDeviceActivated, tapEnv,
		    BroadBandTestConstants.PING_TO_GOOGLE);
	    errorMessage = "Internet is accessible even after adding the client to blocked list";
	    if (status) {
		LOGGER.info(
			"S7 ACTUAL: Ping to web failed, internet access to the client connected to 5 GHz private Wi-Fi network has been blocked");
	    } else {
		LOGGER.error("S7 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 8: Verify the client connected to 5 Ghz Private Wi-Fi is removed from blocked list
	     *
	     */
	    testStepNumber = "s8";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 8: Verify the client connected to 5 Ghz Private Wi-Fi is removed from blocked list");
	    LOGGER.info(
		    "STEP 8: EXPECTED: The 5Ghz Connected client details should be removed from blocked device list");
	    WebPaServerResponse deleteResponse = tapEnv.deleteTableRowUsingRestApi(device,
		    parentalControlManageDeviceTableAddRowResponse);
	    errorMessage = "Null response obtained for deleting Parental Control - Managed Device Rule";
	    if (CommonMethods.isNotNull(deleteResponse.getMessage())) {
		status = deleteResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		errorMessage = "Unable to remove the client connected to 5 GHz private Wi-Fi network from blocked device list";
	    }
	    if (status) {
		LOGGER.info(
			"S8 ACTUAL: Client connected to 5 GHz private Wi-Fi network has been successfully removed from blocked device list");
	    } else {
		LOGGER.error("S8 ACTUAL: " + errorMessage + " ACTUAL RESPONSE: " + deleteResponse.getMessage());
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 9: Verify the Parental Control - Manage Devices is disabled via WebPA
	     *
	     */
	    testStepNumber = "s9";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 9: Verify the Parental Control - Manage Devices is disabled via WebPA");
	    LOGGER.info("STEP 9: EXPECTED: Parental Control - Manage Devices should be disabled");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROL_MANAGED_DEVICES_FEATURE,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
	    isEnabled = !status;
	    errorMessage = "Parental Control - Manage Devices cannot be disabled via WebPa";
	    if (status) {
		LOGGER.info("S9 ACTUAL : Parental Control - Manage Devices has been successfully disabled via WebPA");
	    } else {
		LOGGER.error("S9 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 10: Verify if the connected client has internet access via gateway
	     *
	     */
	    testStepNumber = "s10";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 10: Verify if the connected client has internet access via gateway");
	    LOGGER.info("STEP 10: EXPECTED: Ping to the destination host should be successful");
	    LOGGER.info("################## Waiting for 90 seconds to reflect the changes ####################");
	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	    status = ConnectedNattedClientsUtils.verifyPingConnection(connectedDeviceActivated, tapEnv,
		    BroadBandTestConstants.PING_TO_GOOGLE);
	    errorMessage = "Internet cannot accessible even after removing the client from blocked list";
	    if (status) {
		LOGGER.info(
			"S10 ACTUAL: Internet can be accessible in the client connected to 5 GHz private Wi-Fi network after removing from blocked list");
	    } else {
		LOGGER.error("S10 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 11: Verify the Parental Control - Manage Devices is enabled via WebPA
	     *
	     */
	    testStepNumber = "s11";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 11: Verify the Parental Control - Manage Devices is enabled via WebPA");
	    LOGGER.info("STEP 11: EXPECTED: Parental Control - Manage Devices should be enabled via WebPA");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROL_MANAGED_DEVICES_FEATURE,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    isEnabled = status;
	    errorMessage = "Parental Control - Manage Devices cannot be enabled via WebPa";
	    if (status) {
		LOGGER.info("S11 ACTUAL: Parental Control - Manage Devices has been successfully enabled via WebPA");
	    } else {
		LOGGER.error("S11 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 12: Verify the 'Allow All' under Parental Control - Manage Devices is set to true via WebPA
	     *
	     */
	    testStepNumber = "s12";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 12: Verify the 'Allow All' under Parental Control - Manage Devices is set to true via WebPA ");
	    LOGGER.info("STEP 12: EXPECTED: Allow All should be set to true");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROL_ALLOW_ALL,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    errorMessage = "'Allow All' under Parental Control - Manage Devices cannot be enable via WebPA";
	    if (status) {
		LOGGER.info(
			"S12 ACTUAL: 'Allow All' under Parental Control - Manage Devices has been set to 'true' via WebPA");
	    } else {
		LOGGER.error("S12 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 13: Verify the client connected to 5 Ghz Private Wi-Fi is added to blocked list for particular time
	     * period
	     */
	    testStepNumber = "s13";
	    status = false;
	    parentalControlManageDeviceTableAddRowResponse = null;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 13: Verify the client connected to 5 Ghz Private Wi-Fi is added to blocked list for particular time period");
	    LOGGER.info(
		    "STEP 13: EXPECTED: The 5Ghz Connected client details should be added under blocked device list for particular time period");
	    String blockDays = BroadBandParentalControlUtils
		    .getCurrentDayToAddParentalControlRuleWhenAlwaysBlockIsDisabled(tapEnv, device);
	    errorMessage = "Null value obtained for the current day which needs to added in parental Control - Managed Device Rule. Actual value Obtained: "
		    + blockDays;
	    if (CommonMethods.isNotNull(blockDays)) {
		parentalControlManageDeviceTableAddRowResponse = BroadBandParentalControlUtils
			.addConnectedClientToBlockList(tapEnv, device,
				BroadBandTestConstants.PARENTAL_CONTROL_MANAGE_DEVICE_RULE_TYPE,
				BroadBandTestConstants.CONNECTION_TYPE_WIFI_24_GHZ, wifiMacAddress,
				BroadBandTestConstants.FALSE, BroadBandTestConstants.HOURS_12_AM,
				BroadBandTestConstants.HOURS_23_59_PM, blockDays);
		errorMessage = "Null response obtained for setting Parental Control - Managed Device Rule. Actual response: "
			+ parentalControlManageDeviceTableAddRowResponse;
		if (CommonMethods.isNotNull(parentalControlManageDeviceTableAddRowResponse)) {
		    status = CommonUtils.patternSearchFromTargetString(parentalControlManageDeviceTableAddRowResponse,
			    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PARENTAL_CONTROL_MANAGED_DEVICES_TABLE);
		    errorMessage = "Rule to block internet access to the client connected to 5 GHz private Wi-Fi network cannot be added. Actual response: "
			    + parentalControlManageDeviceTableAddRowResponse;
		}
	    }
	    if (status) {
		LOGGER.info(
			"S13 ACTUAL: Rule to block internet access to the client connected to 5 GHz private Wi-Fi network has been added successfully for particular time period");
	    } else {
		LOGGER.error("S13 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 14: Verify if the connected client is not having internet access via gateway
	     *
	     */
	    testStepNumber = "s14";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 14: Verify if the connected client is not having internet access via gateway");
	    LOGGER.info("STEP 14: EXPECTED: Ping to the destination host should fail");
	    LOGGER.info("################## Waiting for 90 seconds to reflect the changes ####################");
	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	    status = !ConnectedNattedClientsUtils.verifyPingConnection(connectedDeviceActivated, tapEnv,
		    BroadBandTestConstants.PING_TO_GOOGLE);
	    errorMessage = "Internet is accessible even after adding the client to blocked list";
	    if (status) {
		LOGGER.info(
			"S14 ACTUAL: Ping to web failed, internet access to the client connected to 5 GHz private Wi-Fi network has been blocked");
	    } else {
		LOGGER.error("S14 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 15: Verify the Parental Control - Manage Devices is disabled via WebPA
	     *
	     */
	    testStepNumber = "s15";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 15: Verify the Parental Control - Manage Devices is disabled via WebPA");
	    LOGGER.info("STEP 15: EXPECTED: Parental Control - Manage Devices should be disabled");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROL_MANAGED_DEVICES_FEATURE,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
	    isEnabled = !status;
	    errorMessage = "Parental Control - Manage Devices cannot be disabled via WebPa";
	    if (status) {
		LOGGER.info("S15 ACTUAL: Parental Control - Manage Devices has been successfully disabled via WebPA");
	    } else {
		LOGGER.error("S15 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 16: Verify if the connected client has internet access via gateway
	     *
	     */
	    testStepNumber = "s16";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 16: Verify if the connected client has internet access via gateway");
	    LOGGER.info("STEP 16: EXPECTED: Ping to the destination host should be successful");
	    LOGGER.info("################## Waiting for 90 seconds to reflect the changes ####################");
	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	    status = ConnectedNattedClientsUtils.verifyPingConnection(connectedDeviceActivated, tapEnv,
		    BroadBandTestConstants.PING_TO_GOOGLE);
	    errorMessage = "Internet cannot accessible even after removing the client from blocked list";
	    if (status) {
		LOGGER.info(
			"S16 ACTUAL: Internet can be accessible in the client connected to 5 GHz private Wi-Fi network after removing from blocked list");
	    } else {
		LOGGER.error("S16 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception testException) {
	    errorMessage = testException.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE BLOCKING/UNBLOCKING THE INTERNET ACCESS TO THE CLIENT CONNECTED TO 5 GHz PRIVATE WIFI NETWORK : "
			    + errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} finally {

	    if (CommonMethods.isNotNull(parentalControlManageDeviceTableAddRowResponse)
		    && CommonUtils.patternSearchFromTargetString(parentalControlManageDeviceTableAddRowResponse,
			    BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PARENTAL_CONTROL_MANAGED_DEVICES_TABLE)) {
		LOGGER.info("########################### STARTING POST-CONFIGURATIONS ###########################");
		LOGGER.info(
			"POST-CONDITION: DESCRIPTION: Verify the client connected to 5 Ghz Private Wi-Fi is removed from blocked list");
		LOGGER.info(
			"POST-CONDITION: EXPECTED: Client connected to 5 GHz should be removed from blocked device list");
		WebPaServerResponse deleteResponse = tapEnv.deleteTableRowUsingRestApi(device,
			parentalControlManageDeviceTableAddRowResponse);
		if (!(CommonMethods.isNotNull(deleteResponse.getMessage())
			&& deleteResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT))) {
		    LOGGER.error(
			    "POST-CONDITION FAILED: Unable to remove the client connected to 5 GHz private Wi-Fi network from blocked device list");
		} else {
		    LOGGER.info(
			    "POST-CONDITION PASSED: Client connected to 5 GHz private Wi-Fi network has been successfully removed from blocked device list");
		}
		LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");
	    }

	    if (isEnabled) {
		LOGGER.info("########################### STARTING POST-CONFIGURATIONS ###########################");
		LOGGER.info(
			"POST-CONDITION: DESCRIPTION: Verify the Parental Control - Manage Devices is disabled via WebPA");
		LOGGER.info("POST-CONDITION: EXPECTED: Parental Control - Manage Devices should be disabled via WebPA");
		if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROL_MANAGED_DEVICES_FEATURE,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE)) {
		    LOGGER.error(
			    "POST-CONDITION FAILED: Parental Control - Manage Devices cannot be disabled via WebPa");
		} else {
		    LOGGER.info(
			    "POST-CONDITION PASSED: Parental Control - Manage Devices has been successfully disabled via WebPA");
		}

		LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");
	    }
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-PC-MANAGE-DEVICE-1001");

    }

    /**
     * Verify ManagementServer - Basic Parameters
     * <ol>
     * <li>Pre Condition 1 : Factory reset the device and Reactivate the Gateway</li>
     * <li>1. Verify whether the value of EnableCWMP is the default value</li>
     * <li>2. Verify whether the value of PeriodicInformEnable is the default value</li>
     * <li>3. Verify whether the value of PeriodicInformInterval is a non-null value</li>
     * <li>4. Verify the value of PeriodicInformTime</li>
     * <li>5. Verify the value of DefaultActiveNotificationThrottle</li>
     * <li>6. Verify the value of CWMPRetryMinimumWaitInterval</li>
     * <li>7. Verify the value of CWMPRetryIntervalMultiplie</li>
     * <li>8. Verify the value of AliasBasedAddressing</li>
     * <li>9. Verify the value of ParameterKey</li>
     * <li>10. Reboot the device and wait for ipacquisition</li>
     * <li>11. Verify whether the value of EnableCWMP persist even after reboot</li>
     * <li>12. Verify whether the value of PeriodicInformEnable persist even after reboot</li>
     * <li>13. Verify whether the value of PeriodicInformInterval persist even after reboot</li>
     * <li>14. Verify the value of PeriodicInformTime persist even after reboot</li>
     * <li>15. Verify the value of DefaultActiveNotificationThrottle persist even after reboot</li>
     * <li>16. Verify the value of CWMPRetryMinimumWaitInterval persist even after reboot</li>
     * <li>17. Verify the value of CWMPRetryIntervalMultiplie persist even after reboot</li>
     * <li>18. Verify the value of AliasBasedAddressing persist even after reboot</li>
     * <li>19. Verify the value of ParameterKey persist even after reboot</li>
     * <li>20. Perform factory reset on the device using WebPA/Snmp Command</li>
     * <li>21. Verify whether the value of EnableCWMP persist even after Factory reset</li>
     * <li>22. Verify whether the value of PeriodicInformEnable persist even after Factory reset</li>
     * <li>23. Verify whether the value of PeriodicInformInterval persist even after Factory reset</li>
     * <li>24. Verify the value of PeriodicInformTime persist even after Factory reset</li>
     * <li>25. Verify the value of DefaultActiveNotificationThrottle persist even after Factory reset</li>
     * <li>26. Verify the value of CWMPRetryMinimumWaitInterval persist even after Factory reset</li>
     * <li>27. Verify the value of CWMPRetryIntervalMultiplie persist even after Factory reset</li>
     * <li>28. Verify the value of AliasBasedAddressing persist even after Factory reset</li>
     * <li>29. Verify the value of ParameterKey persist even after Factory reset</li>
     * <li>Post Condition 1: Reactivate device using snmp,Webpa</li>
     * </ol>
     * 
     * @param settop
     *            {@link Settop}
     * @author Deepika Sekar
     * @refactor Alan_Bivera
     *
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WEBPA)
    @TestDetails(testUID = "TC-RDKB-WEBPA-1017")
    public void testToVerifySnmpSecurityMode(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WEBPA-117";
	String step = null;
	String errorMessage = "";
	boolean status = false;
	String webPaResponse = null;
	int stepNumber = 1;
	ArrayList<String> webpaResponseList = new ArrayList<String>();
	HashMap<String, String> PartnerIdAndCustomerIdMap = new HashMap<String, String>();
	String custId = null;
	String[] messageList = { "reboot.", "factory reset." };
	boolean isfactoryReset = false;

	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("Pre Condition 1 : Factory reset and Reactivate the Gateway.");
	LOGGER.info("1. Verify whether the value of EnableCWMP is the default value");
	LOGGER.info("2. Verify whether the value of PeriodicInformEnable is the default value");
	LOGGER.info("3. Verify whether the value of PeriodicInformInterval is a non-null value");
	LOGGER.info("4. Verify the value of PeriodicInformTime");
	LOGGER.info("5. Verify the value of DefaultActiveNotificationThrottle");
	LOGGER.info("6. Verify the value of CWMPRetryMinimumWaitInterval");
	LOGGER.info("7. Verify the value of CWMPRetryIntervalMultiplie");
	LOGGER.info("8. Verify the value of AliasBasedAddressing");
	LOGGER.info("9. Verify the value of ParameterKey");
	LOGGER.info("10. Reboot the device and wait for ipacquisition");
	LOGGER.info("11. Verify whether the value of EnableCWMP persist even after reboot");
	LOGGER.info("12. Verify whether the value of PeriodicInformEnable persist even after reboot");
	LOGGER.info("13. Verify whether the value of PeriodicInformInterval persist even after reboot");
	LOGGER.info("14. Verify the value of PeriodicInformTime persist even after reboot");
	LOGGER.info("15. Verify the value of DefaultActiveNotificationThrottle persist even after reboot");
	LOGGER.info("16. Verify the value of CWMPRetryMinimumWaitInterval persist even after reboot");
	LOGGER.info("17. Verify the value of CWMPRetryIntervalMultiplie persist even after reboot");
	LOGGER.info("18. Verify the value of AliasBasedAddressing persist even after reboot");
	LOGGER.info("19. Verify the value of ParameterKey persist even after reboot");
	LOGGER.info("20. Perform factory reset on the device using WebPA/Snmp Command ");
	LOGGER.info("21. Verify whether the value of EnableCWMP persist even after Factory Reset");
	LOGGER.info("22. Verify whether the value of PeriodicInformEnable persist even after Factory Reset");
	LOGGER.info("23. Verify whether the value of PeriodicInformInterval persist even after Factory Reset");
	LOGGER.info("24. Verify the value of PeriodicInformTime persist even after Factory Reset");
	LOGGER.info("25. Verify the value of DefaultActiveNotificationThrottle persist even after Factory Reset");
	LOGGER.info("26. Verify the value of CWMPRetryMinimumWaitInterval persist even after Factory Reset");
	LOGGER.info("27. Verify the value of CWMPRetryIntervalMultiplie persist even after Factory Reset");
	LOGGER.info("28. Verify the value of AliasBasedAddressing persist even after Factory Reset");
	LOGGER.info("29. Verify the value of ParameterKey persist even after Factory Reset");
	LOGGER.info("Post Condition 1: Reactivate device using snmp,Webpa");
	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Factory reset and Reactivate the Gateway.
	     */
	    BroadBandPreConditionUtils.executePreConditionToFactoryResetAndReacitivateDevice(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_3, true);
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	    /**
	     * Step 1:Verify whether the value of EnableCWMP is the default value
	     *
	     */

	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of EnableICWMP is  not as expected.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether the value of EnableCWMP is the default value");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_CWMP.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : The expected value is 1 or true");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_CWMP.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse);
		webpaResponseList.add(webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_CWMP.getWebpa().toString() + " is "
			+ webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 2:Verify whether the value of PeriodicInformEnable is the default value
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of PeriodicInformEnable is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether the value of PeriodicInformEnable  is the default value");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_PERIODIC_INFORM.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : The expected value is 1 or true");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_PERIODIC_INFORM.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse);
		webpaResponseList.add(webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_PERIODIC_INFORM.getWebpa().toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 3:Verify whether the value of PeriodicInformInterval is a non-null value
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of PeriodicInformInterval is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether the value of PeriodicInformInterval is a non-null value");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_INTERVAL_PERIODIC_INFORM.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_INTERVAL_PERIODIC_INFORM.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse);
		webpaResponseList.add(webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of  "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_INTERVAL_PERIODIC_INFORM.getWebpa().toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 4:Verify the value of PeriodicInformTime
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of PeriodicInformTime is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify  the value of PeriodicInformTime ");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_TIME_PERIODIC_INFORM.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : PeriodicInformTime value should be Midnight UTC in this testing. Date has not significance in the value but the time should be 00:00:00 e.g. 2013-10-18T00:00:00.000Z.");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_TIME_PERIODIC_INFORM.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse);
		webpaResponseList.add(webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_TIME_PERIODIC_INFORM.getWebpa().toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 5:Verify the value of DefaultActiveNotificationThrottle
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of DefaultActiveNotificationThrottle  is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Verify  the value of DefaultActiveNotificationThrottle");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_ACTIVE_NOTIFICATION_THROTTLE.getWebpa()
			    .toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_ACTIVE_NOTIFICATION_THROTTLE.getWebpa()
				.toString());
		status = CommonMethods.isNotNull(webPaResponse);
		webpaResponseList.add(webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_ACTIVE_NOTIFICATION_THROTTLE.getWebpa()
				.toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 6:Verify the value of CWMPRetryMinimumWaitInterval
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of CWMPRetryMinimumWaitInterval  is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION :Verify  the value of CWMPRetryMinimumWaitInterval");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_MIN_WAIT_INTERVAL.getWebpa()
			    .toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_MIN_WAIT_INTERVAL.getWebpa()
				.toString());
		status = CommonMethods.isNotNull(webPaResponse);
		webpaResponseList.add(webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_MIN_WAIT_INTERVAL.getWebpa()
				.toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 7:Verify the value of CWMPRetryIntervalMultiplie
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of CWMPRetryIntervalMultiplier is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION :Verify  the value of CWMPRetryIntervalMultiplie");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_INTERVAL_MULTIPLIER.getWebpa()
			    .toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_INTERVAL_MULTIPLIER.getWebpa()
				.toString());
		status = CommonMethods.isNotNull(webPaResponse);
		webpaResponseList.add(webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_INTERVAL_MULTIPLIER.getWebpa()
				.toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 8:Verify the value of AliasBasedAddressing
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of AliasBasedAddressing  is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify  the value of AliasBasedAddressing");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_ALIAS_ADDRESSING.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_ALIAS_ADDRESSING.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse);
		webpaResponseList.add(webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_ALIAS_ADDRESSING.getWebpa().toString() + " is "
			+ webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 9:Verify the value of ParameterKey
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of ParameterKey  is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify  the value of ParameterKey");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_PARAMETER_KEY.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_PARAMETER_KEY.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse);
		webpaResponseList.add(webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_PARAMETER_KEY.getWebpa().toString() + " is "
			+ webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 10 : Reboot the device and wait for ipacquisition
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify device reboot is successful and wait for ipacquisition");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Execute command :/sbin/reboot");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : Device must reboot successfully");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Device reboot operation failed";
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)
		    && BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : Device rebooted successfully");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /**
	     * Step 11-19 : ManageServer parameter verification after reboot
	     */
	    managementServerParametersAfterReboot(tapEnv, device, stepNumber, testCaseId, webpaResponseList,
		    messageList[0]);

	    /**
	     * Step 20 : Perform factory reset on the device using WebPA/Snmp Command
	     */
	    stepNumber = 20;
	    step = "S" + stepNumber;
	    errorMessage = "Unable to factory reset device/Unable to reactivate device after factory reset";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " DESCRIPTION : Perform factory reset on the device using WebPA/Snmp Command and reactivate");
	    LOGGER.info("STEP " + stepNumber
		    + " ACTION : Execute WebPa Command with parameter  Device.X_CISCO_COM_DeviceControl.FactoryReset to set to value\":\"Router,Wifi,VoIP,Dect,MoCA\"");
	    LOGGER.info(
		    "STEP " + stepNumber + " EXPECTED : Factory reset should be successful and device should be Up ");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.performFactoryResetWebPaByPassingTriggerTime(tapEnv, device,
		    BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS);
	    if (status) {
		isfactoryReset = status;
		LOGGER.info("STEP " + stepNumber + " ACTUAL : successfully factory reset device and reactivated");
	    } else {
		LOGGER.error("STEP " + stepNumber + " ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
	    /**
	     * Step 21-29 : ManageServer parameter verification after factoryreset
	     */
	    managementServerParametersAfterReboot(tapEnv, device, stepNumber, testCaseId, webpaResponseList,
		    messageList[1]);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, false);
	} finally {
	    if (isfactoryReset) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		/**
		 * Post condition 1 : Reactivate device using snmp,Webpa
		 */
		BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, !isfactoryReset,
			BroadBandTestConstants.CONSTANT_1);
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
	LOGGER.info(" ENDING TEST CASE: TC-RDKB-WEBPA-1017");
    }

    /**
     * Method to do validate managementServerParameters After Reboot/factory reset .
     * 
     * @param tapEnv
     * @param settop
     *            {@link Settop}
     * @param stepNumber
     *            step Number
     * @param testId
     *            Test case ID
     * @param initialWebpaResponse
     *            WebParesponseList before reboot/factory reset
     * @param process
     *            Value will be either reboot/factory reset
     * @return
     * 
     */
    public static void managementServerParametersAfterReboot(AutomaticsTapApi tapEnv, Dut device, int stepNumber,
	    String testCaseId, ArrayList initialWebpaResponse, String process) throws Exception {
	String step = null;
	boolean status = false;
	String errorMessage = null;
	String webPaResponse = null;
	try {
	    /**
	     * Step :Verify whether the value of EnableCWMP with value before reboot/Factory reset
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of EnableICWMP is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether the value of EnableCWMP persist even after " + process);
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_CWMP.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : The expected value is 1 or true");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_CWMP.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse) && BroadBandCommonUtils.compareValues(
			BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			String.valueOf(initialWebpaResponse.get(BroadBandTestConstants.CONSTANT_0)), webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_CWMP.getWebpa().toString() + " is "
			+ webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step :Verify whether the value of PeriodicInformEnable with value before reboot/Factory reset
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of PeriodicInformEnable is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether the value of PeriodicInformEnable persist even after " + process);
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_PERIODIC_INFORM.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : The expected value is 1 or true");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_PERIODIC_INFORM.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse) && BroadBandCommonUtils.compareValues(
			BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			String.valueOf(initialWebpaResponse.get(BroadBandTestConstants.CONSTANT_1)), webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_ENABLE_PERIODIC_INFORM.getWebpa().toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step :Verify whether the value of PeriodicInformInterval with value before reboot/Factory reset
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of PeriodicInformInterval is  not as expected.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether the value of PeriodicInformInterval persist even after "
		    + process);
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_INTERVAL_PERIODIC_INFORM.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_INTERVAL_PERIODIC_INFORM.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse) && BroadBandCommonUtils.compareValues(
			BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			String.valueOf(initialWebpaResponse.get(BroadBandTestConstants.CONSTANT_2)), webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of  "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_INTERVAL_PERIODIC_INFORM.getWebpa().toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step :Verify the value of PeriodicInformTime with value before reboot/Factory reset
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of PeriodicInformTime is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify  the value of PeriodicInformTime persist even after " + process);
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_TIME_PERIODIC_INFORM.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : PeriodicInformTime value should be Midnight UTC in this testing. Date has not significance in the value but the time should be 00:00:00 e.g. 2013-10-18T00:00:00.000Z.");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_TIME_PERIODIC_INFORM.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse) && BroadBandCommonUtils.compareValues(
			BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			String.valueOf(initialWebpaResponse.get(BroadBandTestConstants.CONSTANT_3)), webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_TIME_PERIODIC_INFORM.getWebpa().toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step :Verify the value of DefaultActiveNotificationThrottle with value before reboot/Factory reset
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of DefaultActiveNotificationThrottle  is  not as expected.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify  the value of DefaultActiveNotificationThrottle persist even after "
		    + process);
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_ACTIVE_NOTIFICATION_THROTTLE.getWebpa()
			    .toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_ACTIVE_NOTIFICATION_THROTTLE.getWebpa()
				.toString());
		status = CommonMethods.isNotNull(webPaResponse) && BroadBandCommonUtils.compareValues(
			BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			String.valueOf(initialWebpaResponse.get(BroadBandTestConstants.CONSTANT_4)), webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_ACTIVE_NOTIFICATION_THROTTLE.getWebpa()
				.toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step :Verify the value of CWMPRetryMinimumWaitInterval with value before reboot/Factory reset
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of CWMPRetryMinimumWaitInterval  is  not as expected.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verify  the value of CWMPRetryMinimumWaitInterval persist even after " + process);
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_MIN_WAIT_INTERVAL.getWebpa()
			    .toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_MIN_WAIT_INTERVAL.getWebpa()
				.toString());
		status = CommonMethods.isNotNull(webPaResponse) && BroadBandCommonUtils.compareValues(
			BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			String.valueOf(initialWebpaResponse.get(BroadBandTestConstants.CONSTANT_5)), webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_MIN_WAIT_INTERVAL.getWebpa()
				.toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step :Verify the value of CWMPRetryIntervalMultiplie with value before reboot/Factory reset
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of CWMPRetryIntervalMultiplier is  not as expected.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION :Verify  the value of CWMPRetryIntervalMultiplie persist even after " + process);
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_INTERVAL_MULTIPLIER.getWebpa()
			    .toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_INTERVAL_MULTIPLIER.getWebpa()
				.toString());
		status = CommonMethods.isNotNull(webPaResponse) && BroadBandCommonUtils.compareValues(
			BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			String.valueOf(initialWebpaResponse.get(BroadBandTestConstants.CONSTANT_6)), webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_CWMP_RETRY_INTERVAL_MULTIPLIER.getWebpa()
				.toString()
			+ " is " + webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step :Verify the value of AliasBasedAddressing with value before reboot/Factory reset
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of AliasBasedAddressing  is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify  the value of AliasBasedAddressing persist even after " + process);
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_ALIAS_ADDRESSING.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_ALIAS_ADDRESSING.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse) && BroadBandCommonUtils.compareValues(
			BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			String.valueOf(initialWebpaResponse.get(BroadBandTestConstants.CONSTANT_7)), webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_ALIAS_ADDRESSING.getWebpa().toString() + " is "
			+ webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step :Verify the value of ParameterKey with value before reboot/Factory reset
	     *
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = "The value of ParameterKey  is  not as expected.";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify  the value of ParameterKey persist even after "
		    + process);
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute webpa on "
		    + DeviceManagementServerParams.DEVICE_MANAGEMENT_PARAMETER_KEY.getWebpa().toString());
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : A non null value is expected");
	    LOGGER.info("**********************************************************************************");
	    try {
		webPaResponse = tapEnv.executeWebPaCommand(device,
			DeviceManagementServerParams.DEVICE_MANAGEMENT_PARAMETER_KEY.getWebpa().toString());
		status = CommonMethods.isNotNull(webPaResponse) && BroadBandCommonUtils.compareValues(
			BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			String.valueOf(initialWebpaResponse.get(BroadBandTestConstants.CONSTANT_8)), webPaResponse);
	    } catch (Exception e) {
		errorMessage = errorMessage + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : The expected value of "
			+ DeviceManagementServerParams.DEVICE_MANAGEMENT_PARAMETER_KEY.getWebpa().toString() + " is "
			+ webPaResponse);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	}
    }
    
    /**
    *
    * Test Case : Verify subnet mask range in router mode
    *
    * <p>
    * STEPS:
    * </p>
    * <ol>
    * <li>PRE-CONDITION 1 : GET THE DEFAULT SUBNET MASK VALUE</li>
    * <li>PRE-CONDITION 2 : VERIFY THAT THE DEVICE IS IN ROUTER MODE</li>
    * <li>Step 1 : Set and verify the list of valid subnet mask values using webpa when device is in router mode</li>
    * <li>Step 2 : Set and verify the the list of invalid subnet mask values using webpa when device is in router mode
    * </li>
    * <li>POST-CONDITION 1 : SET THE DEFAULT SUBNET MASK VALUE</li>
    * </ol>
    * 
    * @param device
    *            {@link Dut}
    * @author Muthukumar
    * @refactor Said Hisham
    */
   @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
   @TestDetails(testUID = "TC-RDKB-SUBMSK-CHANGE-5001")
   public void testToVerifySubnetMaskValues(Dut device) {
	int stepNumber = BroadBandTestConstants.CONSTANT_1;
	String stepNum = "S" + stepNumber;
	String errorMessage = "";
	boolean status = false;
	String defaultSubnetMaskValue = null;
	String testCaseId = "TC-RDKB-SUBMSK-CHANGE-501";
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SUBMSK-CHANGE-5001");
	LOGGER.info("TEST DESCRIPTION: Verify subnetmask range in Bridge Mode");
	LOGGER.info("PRE-CONDITION 1 : GET THE DEFAULT SUBNET MASK VALUE");
	LOGGER.info("PRE-CONDITION 2 : VERIFY THAT THE DEVICE IS IN ROUTER MODE");
	LOGGER.info(
		"Step 1 : Set and verify the list of valid subnet mask values using webpa when device is in router mode");
	LOGGER.info(
		"Step 2 : Set and verify the list of invalid subnet mask values using webpa when device is in router mode");
	LOGGER.info("POST-CONDITION 1 : SET THE DEFAULT SUBNET MASK VALUE");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");

	    /**
	     * PRECONDITION 1 : GET THE DEFAULT SUBNET MASK VALUE
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : GET THE DEFAULT SUBNET MASK VALUE");
	    LOGGER.info("PRE-CONDITION 1 : ACTION : GET THE DEFAULT SUBNET MASK VALUE USING WEBPA");
	    LOGGER.info("PRE-CONDITION 1 : EXPECTED : MUST RETRIEVE THE DEFAULT SUBNET MASK VALUE ");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "FAILED TO GET DEFAULT SUBNET MASK VALUE";
	    defaultSubnetMaskValue = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_LAN_SUBNET);
	    status = CommonMethods.isNotNull(defaultSubnetMaskValue);
	    if (status) {
		LOGGER.info("PRE-CONDITION 1 : ACTUAL : DEFAULT SUBNET MASK VALUE ARE RETRIEVED SUCCESSFULLY");
	    } else {
		LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
	    }

	    /**
	     * PRECONDITION 2 : VERIFY THAT THE DEVICE IS IN ROUTER MODE
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : VERIFY THAT THE DEVICE IS IN ROUTER MODE.");
	    LOGGER.info("PRE-CONDITION 2 : ACTION : CHECK FOR THE DEVICE IS IN ROUTER MODE USING WEBPA");
	    LOGGER.info("PRE-CONDITION 2 : EXPECTED : DEVICE SHOULD BE IN 'ROUTER' MODE.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "FAILED TO VERIFY THE DEVICE IS IN ROUTER MODE";
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE,
			BroadBandTestConstants.LAN_MANAGEMENT_MODE_ROUTER);
	    } catch (TestException exception) {
		LOGGER.error(errorMessage + " : " + exception.getMessage());
	    }
	    if (!status) {
		try {
		    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
			    BroadBandTestConstants.LAN_MANAGEMENT_MODE_ROUTER,
			    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
		} catch (TestException exception) {
		    LOGGER.error(errorMessage + " : " + exception.getMessage());
		}
		if (status) {
		    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
		}
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 2 : ACTUAL : SUCCESSFULLY VERIFIED ROUTER MODE IN GATEWAY");
	    } else {
		LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	    BroadBandResultObject broadBandResultObject = null;

	    /**
	     * STEP 1 : SET AND VERIFY THE LIST OF VALID SUBNET MASK VALUES USING WEBPA WHEN DEVICE IS IN ROUTER MODE
	     */
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Set and verify the list of valid subnet mask values using webpa when device is in router mode \n"
		    + " 1. Set subnet mask value using webpa \n" + " 2. Verify the log information in PAMLog.txt.0 \n");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Execute command:  \n "
		    + " 1. Execute webpa/dmcli command: dmcli eRT setv Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanSubnetMask string <subnetmask value>  \n"
		    + " 2. cat /rdklogs/logs/PAMLog.txt.0  \n");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : Must set the valid subnet mask values");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Failed to set and verify the list of valid subnet mask values\n";
	    broadBandResultObject = BroadBandWiFiUtils.validateValidSubnetMaskValues(device, tapEnv);
	    status = broadBandResultObject.isStatus();
	    errorMessage = broadBandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP :  " + stepNumber
			+ " : ACTUAL : Successfully set and verified the valid subnet mask values");
	    } else {
		LOGGER.error("STEP :  " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Step 2 : SET AND VERIFY THE THE LIST OF INVALID SUBNET MASK VALUES USING WEBPA WHEN DEVICE IS IN ROUTER
	     * MODE
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Set and verify the  the list of invalid subnet mask values using webpa when device is in router mode\n"
		    + "1. Set subnet mask value using webpa \n" + "2. Verify the log information in PAMLog.txt.0 \n");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : 1.Execute webpa/dmcli command:  \n"
		    + "1. dmcli eRT setv Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanSubnetMask string <subnetmaskvalue> \n"
		    + "2. cat /rdklogs/logs/PAMLog.txt.0 \n");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : Should not set the invalid subnet mask values");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Failed to set and verify the list of invalid subnet mask values\n";
	    broadBandResultObject = BroadBandWiFiUtils.validateInValidSubnetMaskValues(device,
		    tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_LAN_SUBNET));
	    status = broadBandResultObject.isStatus();
	    errorMessage = broadBandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL :  Successfully verified the invalid subnet mask configuration");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    /**
	     * POST-CONDITION 1 : SET THE DEFAULT SUBNET MASK VALUE
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : SET THE DEFAULT SUBNET MASK VALUE");
	    LOGGER.info("POST-CONDITION 1 : ACTION : SET THE DEFAULT SUBNET MASK VALUE USING WEBPA");
	    LOGGER.info("POST-CONDITION 1 : EXPECTED : MUST SET THE DEFAULT SUBNET MASK VALUE ");
	    LOGGER.info("#######################################################################################");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_LAN_SUBNET, BroadBandTestConstants.CONSTANT_0,
		    defaultSubnetMaskValue, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : SUCCESSFULLY SET THE DEFAULT SUBNET MASK VALUE.");
	    } else {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SUBMSK-CHANGE-5001");
   }

}
