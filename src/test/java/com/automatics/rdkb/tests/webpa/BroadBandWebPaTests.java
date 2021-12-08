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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWebPaTests extends AutomaticsTestBase {
    /**
     * 
     * This method verifies that webpa request to get value of Webpa.version parameter gives the value of WebPA version
     * and and configparamgen version and it's compatibility with Firmware version
     * 
     * <ol>
     * <li>Step 1 : Verify retrieval of WebPA version in TR181 parameter</li>
     * <li>Step 2 :Verify the configparamgen version running in gateway</li>
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
	String configparamgenVersion = null;
	String currentBuild = null;
	// Variables declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1003");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the retrieval of webpa version from tr181 parameter and configparamgen version and it's compatibility with Firmware version");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify WebPA version obtained using WebPA request.");
	LOGGER.info("2. Verify the configparamgen version running in gateway");
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

	    stepNum = "S2";
	    status = false;
	    errorMessage = "Failed to get the configparamgen or current build details from the Gateway";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2 : DESCRIPTION : Verify the configparamgen version running in gateway");
	    LOGGER.info("STEP 2 : ACTION : Execute command: configparamgen in gateway");
	    LOGGER.info(
		    "STEP 2 : EXPECTED : Must return the configparamgen version based on build varient mentioned below :\n"
			    + " 1. Release < 4.4                      : configparamgen Version : 2.17 \n"
			    + "	2. Release > 4.4                      : configparamgen Version : 3.7 or higher \n"
			    + "	3. 4.4 initial releases until 4.4p1s9 : configparamgen Version : 3.7 or higher \n"
			    + "	4. Release 4.4p1s10 to 4.4p2s2        : configparamgen Version : 2.17 \n"
			    + "	5. Release >=4.4p3s1                  : configparamgen Version : 3.7 or higher \n"
			    + "	6. Stable2                            : configparamgen Version : 3.7 or higher \n"
			    + "	7. Sprint                             : configparamgen Version : 3.7 or higher");
	    LOGGER.info("**********************************************************************************");
	    try {
		currentBuild = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CONFIGPARAMGEN);
		LOGGER.info("Current Build in Gateway is : " + currentBuild);
		LOGGER.info("Response is : " + response);
		if (CommonUtils.isNotEmptyOrNull(response) && CommonUtils.isNotEmptyOrNull(currentBuild)) {
		    configparamgenVersion = CommonMethods.patternFinder(response,
			    BroadBandTestConstants.CONFIGPARAMGEN_VERSION_PATTERN_MATCHER);
		    LOGGER.info("Configparamgen Version obtained is : " + configparamgenVersion);
		    if (CommonUtils.isNotEmptyOrNull(configparamgenVersion)) {
			errorMessage = "configparemgen Required for build is not as expected";
			status = BroadBandCommonUtils.verifyConfigparamgenForGivenBuild(currentBuild,
				configparamgenVersion);
		    }
		}
	    } catch (Exception e) {
		LOGGER.error(errorMessage += e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Configparamgen Version is as Expected : " + configparamgenVersion);
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
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
	 * @param device {@link Dut}
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
	 * @param device {@link Dut}
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
	 * @param device {@link Dut}
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
	 * @param device {@link Dut}
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

}
