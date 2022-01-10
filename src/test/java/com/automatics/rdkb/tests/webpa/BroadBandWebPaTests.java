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
import com.automatics.rdkb.server.WhiteListServer;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandSystemUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaServerResponse;

public class BroadBandWebPaTests extends AutomaticsTestBase {
	/**
	 * 
	 * This method verifies that webpa request to get value of Webpa.version
	 * parameter gives the value of WebPA version and and configparamgen version
	 * and it's compatibility with Firmware version
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
		LOGGER.info("TEST DESCRIPTION: Verify the retrieval of webpa version from tr181 parameter and configparamgen version and it's compatibility with Firmware version");
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
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_WEBPA_VERSION);
			if (CommonMethods.isNotNull(response)
					&& response
							.contains(BroadBandTestConstants.PROCESS_NAME_WEBPA
									.toUpperCase())) {
				patternMatchedStringList = CommonMethods
						.patternFinderToReturnAllMatchedString(
								response,
								BroadBandTestConstants.WEBPA_VERSION_PATTERN_MATCHER);
				if (patternMatchedStringList.size() == BroadBandTestConstants.CONSTANT_2) {
					webpaVersion = patternMatchedStringList
							.get(BroadBandTestConstants.CONSTANT_0)
							+ "."
							+ patternMatchedStringList
									.get(BroadBandTestConstants.CONSTANT_1);
				}
			}
			status = CommonMethods.isNotNull(webpaVersion);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : WebPA request response contains WebPA version: "
						+ webpaVersion);
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status,
					errorMessage, true);

			stepNum = "S2";
			status = false;
			errorMessage = "Failed to get the configparamgen or current build details from the Gateway";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2 : DESCRIPTION : Verify the configparamgen version running in gateway");
			LOGGER.info("STEP 2 : ACTION : Execute command: configparamgen in gateway");
			LOGGER.info("STEP 2 : EXPECTED : Must return the configparamgen version based on build varient mentioned below :\n"
					+ " 1. Release < 4.4                      : configparamgen Version : 2.17 \n"
					+ "	2. Release > 4.4                      : configparamgen Version : 3.7 or higher \n"
					+ "	3. 4.4 initial releases until 4.4p1s9 : configparamgen Version : 3.7 or higher \n"
					+ "	4. Release 4.4p1s10 to 4.4p2s2        : configparamgen Version : 2.17 \n"
					+ "	5. Release >=4.4p3s1                  : configparamgen Version : 3.7 or higher \n"
					+ "	6. Stable2                            : configparamgen Version : 3.7 or higher \n"
					+ "	7. Sprint                             : configparamgen Version : 3.7 or higher");
			LOGGER.info("**********************************************************************************");
			try {
				currentBuild = FirmwareDownloadUtils
						.getCurrentFirmwareFileNameForCdl(tapEnv, device);
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommandConstants.CONFIGPARAMGEN);
				LOGGER.info("Current Build in Gateway is : " + currentBuild);
				LOGGER.info("Response is : " + response);
				if (CommonUtils.isNotEmptyOrNull(response)
						&& CommonUtils.isNotEmptyOrNull(currentBuild)) {
					configparamgenVersion = CommonMethods
							.patternFinder(
									response,
									BroadBandTestConstants.CONFIGPARAMGEN_VERSION_PATTERN_MATCHER);
					LOGGER.info("Configparamgen Version obtained is : "
							+ configparamgenVersion);
					if (CommonUtils.isNotEmptyOrNull(configparamgenVersion)) {
						errorMessage = "configparemgen Required for build is not as expected";
						status = BroadBandCommonUtils
								.verifyConfigparamgenForGivenBuild(
										currentBuild, configparamgenVersion);
					}
				}
			} catch (Exception e) {
				LOGGER.error(errorMessage += e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Configparamgen Version is as Expected : "
						+ configparamgenVersion);
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status,
					errorMessage, true);

		} catch (Exception exception) {
			LOGGER.info("Inside catch");
			errorMessage = exception.getMessage();
			LOGGER.error("Exception occured while validating webpa version using webpa request: "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId,
					stepNum, status, errorMessage, true);
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
			LOGGER.info("PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
			LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device,
					true);
			errorMessage = "WEBPA PROCESS IS NOT UP AND RUNNING";
			if (!status) {
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR
								+ errorMessage);
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
			LOGGER.info("STEP 1: ACTION :Set a new value for SSID and get the same value and Validating SSID of private 5.0 GHz network is advertised");
			LOGGER.info("STEP 1: EXPECTED: Set a new value for SSID and get the same value and SSID is advertised");

			errorMessage = "Failed to set 5 Ghz SSID name using WebPA parameter "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME;
			// Set Current 5Ghz SSID name, other than default value
			LOGGER.info("Modify Current SSID name of 5 Ghz as "
					+ BroadBandTestConstants.STRING_TEST_1);

			status = BroadBandWebPaUtils
					.setAndGetParameterValuesUsingWebPa(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
							BroadBandTestConstants.CONSTANT_0,
							BroadBandTestConstants.STRING_TEST_1);
			LOGGER.info("Is Current SSID for 5 Ghz modified - " + status);

			if (status) {
				String response = BroadBandWebPaUtils
						.getParameterValuesUsingWebPaOrDmcli(
								device,
								tapEnv,
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
				LOGGER.info("STEP 1: ACTUAL : Set new value for private 5.0 GHz SSID and Validated SSID of private 5.0 GHz network is advertised");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId,
					testStep, status, errorMessage, false);
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
			LOGGER.info("PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
			LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
			LOGGER.info("**********************************************************************************");

			LOGGER.info("*********************************************************************************");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device,
					true);
			errorMessage = "WEBPA PROCESS IS NOT UP AND RUNNING";
			if (!status) {
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR
								+ errorMessage);
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

			status = BroadBandCommonUtils
					.setDeviceInBridgeStaticModeStatusUsingWebPaCommand(tapEnv,
							device);

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Device persist in the Bridge mode after being set");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStep, status,
					errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId,
					testStep, status, errorMessage, false);
		} finally {
			int postCondition = 0;
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");

			/**
			 * POST-CONDITION 1: Revert the device back to Router mode.
			 */
			postCondition++;

			LOGGER.info("#####################################################################################");
			LOGGER.info("POST-CONDITION " + postCondition
					+ ":  DESCRIPTION :Revert the device back to Router mode.");
			LOGGER.info("POST-CONDITION "
					+ postCondition
					+ ": ACTION : Execute webpa command : Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode with value as router");
			LOGGER.info("POST-CONDITION " + postCondition
					+ ": EXPECTED: Should be able to set lanMode as router");
			LOGGER.info("#####################################################################################");

			status = BroadBandCommonUtils
					.setDeviceInRouterModeStatusUsingWebPaCommand(tapEnv,
							device);

			if (status) {
				LOGGER.info("Reverted the device back to Router mode.");
			}
			if (status) {
				LOGGER.info("POST-CONDITION " + postCondition
						+ ": ACTUAL : Reverted the device back to Router mode.");
			} else {
				LOGGER.error("POST-CONDITION " + postCondition
						+ ": ACTUAL : Post condition failed ");
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
			LOGGER.info("PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
			LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device,
					true);
			errorMessage = "WEBPA PROCESS IS NOT UP AND RUNNING";
			if (!status) {
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR
								+ errorMessage);
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
			LOGGER.info("STEP 1: ACTION :Set a new value for SSID and get the same value and Validating SSID of public 2.4 GHz network is advertised");
			LOGGER.info("STEP 1: EXPECTED: Set a new value for SSID and get the same value and SSID is advertised");
			LOGGER.info("**********************************************************************************");

			errorMessage = "Failed to set public 2.4 Ghz SSID name using WebPA parameter "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_NAME;
			// Set Current public 2.4Ghz SSID name, other than default value
			// SSID
			LOGGER.info("Modify Current SSID name of 2.4 Ghz as "
					+ BroadBandTestConstants.STRING_TEST_1);

			status = BroadBandWebPaUtils
					.setAndGetParameterValuesUsingWebPa(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PUBLIC_SSID_NAME,
							BroadBandTestConstants.CONSTANT_0,
							BroadBandTestConstants.STRING_TEST_1);
			LOGGER.info("Is Current SSID for public 2.4 Ghz modified - "
					+ status);

			if (status) {
				String response = BroadBandWebPaUtils
						.getParameterValuesUsingWebPaOrDmcli(
								device,
								tapEnv,
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
				LOGGER.info("STEP 1: ACTUAL : Set new value for public 2.4 GHz SSID and Validated SSID of public 2.4 GHz network is advertised");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId,
					testStep, status, errorMessage, false);
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
			LOGGER.info("PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
			LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device,
					true);
			errorMessage = "WEBPA PROCESS IS NOT UP AND RUNNING";
			if (!status) {
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR
								+ errorMessage);
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
			LOGGER.info("STEP 1: ACTION :Set a new value for SSID and get the same value and Validating SSID of public 5.0 GHz network is advertised");
			LOGGER.info("STEP 1: EXPECTED: Set a new value for SSID and get the same value and SSID is advertised");
			LOGGER.info("**********************************************************************************");

			errorMessage = "Failed to set public 5 Ghz SSID name using WebPA parameter "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_NAME;
			// Set Current public 5Ghz SSID name, other than default value
			LOGGER.info("Modify Current SSID name of 5 Ghz as "
					+ BroadBandTestConstants.STRING_TEST_1);

			status = BroadBandWebPaUtils
					.setAndGetParameterValuesUsingWebPa(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PUBLIC_SSID_NAME,
							BroadBandTestConstants.CONSTANT_0,
							BroadBandTestConstants.STRING_TEST_1);
			LOGGER.info("Is Current SSID for public 5 Ghz modified - " + status);

			if (status) {
				String response = BroadBandWebPaUtils
						.getParameterValuesUsingWebPaOrDmcli(
								device,
								tapEnv,
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
				LOGGER.info("STEP 1: ACTUAL : Set new value for public 5.0 GHz SSID and Validated SSID of public 5.0 GHz network is advertised");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId,
					testStep, status, errorMessage, false);
		}

	}

	/**
	 * Verify Webpa requests (GET/SET/PUT/POST/DELETE) are working fine and
	 * Webpa notifications should be sent successfully to server
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

	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = { BroadBandTestGroup.WEBPA }, dataProviderClass = AutomaticsTapApi.class)
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
			LOGGER.info("PRE-CONDITION-1: DESCRIPTION : Set and verify the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
			LOGGER.info("PRE-CONDITION-1: ACTION : Set the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
			LOGGER.info("PRE-CONDITION-1: EXPECTED : Global DNS IPv4 value should be set using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
			LOGGER.info("#######################################################################################");
			status = false;
			status = BroadBandWebPaUtils
					.setVerifyWebPAInPolledDuration(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4,
							WebPaDataTypes.STRING.getValue(),
							BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE,
							BroadBandTestConstants.THREE_MINUTES,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			// Error message
			errorMessage = "Failed to set Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'";
			LOGGER.info("PRE-CONDITION-1: ACTUAL: "
					+ (status ? "Global DNS IPv4 value sucessfully set"
							: errorMessage));

			if (!status) {
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR
								+ "PRE-CONDITION-1 FAILED : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION-2: DESCRIPTION : Set and verify the Global DNS IPv6 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
			LOGGER.info("PRE-CONDITION-2: ACTION : Set the Global DNS IPv6 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
			LOGGER.info("PRE-CONDITION-2: EXPECTED : Global DNS IPv6 value should be set using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
			LOGGER.info("#######################################################################################");
			status = false;
			status = BroadBandWebPaUtils
					.setVerifyWebPAInPolledDuration(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6,
							WebPaDataTypes.STRING.getValue(),
							BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV6_VALUE,
							BroadBandTestConstants.THREE_MINUTES,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			// Error message
			errorMessage = "Failed to set Global DNS IPv6 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'";
			LOGGER.info("PRE-CONDITION-2: ACTUAL: "
					+ (status ? "Global DNS IPv6 value sucessfully set"
							: errorMessage));
			if (!status) {
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR
								+ "PRE-CONDITION-2 FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1013");
			LOGGER.info("TEST DESCRIPTION: Verify Webpa requests (GET/SET/PUT/POST/DELETE) are working fine and Webpa notifications should be sent successfully to server");
			LOGGER.info("#######################################################################################");

			/**
			 * STEP 1:Verify the WebPA GET command can be executed successfully
			 * 
			 */
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 1:DESCRIPTION: Verify the WebPA GET command can be executed successfully");
			LOGGER.info("STEP 1:ACTION: Execute the TR-181 parameter-Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS to retrieve status of XDNS by WebPA");
			LOGGER.info("STEP 1:EXPECTED: WEBPA request should respond with success message and status code 200");
			LOGGER.info("#####################################################################################");
			currentDeviceTimeStamp = BroadBandCommonUtils
					.getCurrentTimeStampOnDevice(tapEnv, device);
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			errorMessage = "Unable to retrieve the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' using WebPA command.";
			String xDNSStatus = tapEnv
					.executeWebPaCommand(
							device,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS);
			LOGGER.info("Xdns status retrieved using WebPa = " + xDNSStatus);
			status = CommonMethods.isNotNull(xDNSStatus);
			if (status) {
				LOGGER.info("S1 ACTUAL: Successfully retrieved  the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' using WebPA command.");
			} else {
				LOGGER.error("S1 ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

			/**
			 * Step 2: Verify WEBPA GET request log message is present in
			 * WEBPAlog.txt.0
			 *
			 */
			testStepNumber = "s2";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 2: DESCRIPTION : Verify WEBPA GET request log message is present in WEBPAlog.txt.0 ");
			LOGGER.info("STEP 2: ACTION : Execute command: 1. grep -i \"WDMP-C: Request\" /rdklogs/logs/Consolelog.txt.0");
			LOGGER.info("STEP 2: EXPECTED : Webpa Log messages should present in WEBPAlog.txt.0");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to get the  log message under /rdklogs/logs/WEBPAlog.txt.0 ";
			status = BroadBandSystemUtils
					.verifyWebpaNotificationForPollingTime(tapEnv, device,
							BroadBandTraceConstants.WEBPA_GET_NOTIFICATION,
							currentDeviceTimeStamp);
			if (status) {
				LOGGER.info("S2 ACTUAL: WEBPA GET request log message is present in WEBPAlog.txt.0");
			} else {
				LOGGER.error("S2 ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

			/**
			 * ` STEP 3:Verify the WebPA SET command can be executed
			 * successfully
			 * 
			 */
			testStepNumber = "s3";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 3:DESCRIPTION: Verify the WebPA SET command can be executed successfully");
			LOGGER.info("STEP 3:ACTION: Execute the TR-181 parameter-Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS to set the  status of XDNS as 'true' by WebPA");
			LOGGER.info("STEP 3:EXPECTED: WEBPA request should respond with success message and status code 200");
			LOGGER.info("#####################################################################################");
			errorMessage = "Unable to set the status of XDNS 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' as 'true'using WebPA command..";
			currentDeviceTimeStamp = BroadBandCommonUtils
					.getCurrentTimeStampOnDevice(tapEnv, device);
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			webPaServerResponse = BroadBandWebPaUtils
					.setWebPaParamAndReturnResp(
							tapEnv,
							device,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS,
							BroadBandTestConstants.TRUE,
							BroadBandTestConstants.CONSTANT_3);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(
					BroadBandTestConstants.SUCCESS_TXT);
			if (status) {
				LOGGER.info("S3 ACTUAL: Successfully able to set the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS' as 'true' using WebPA command.");
			} else {
				LOGGER.error("S3 ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

			/**
			 * Step 4: Verify WEBPA SET request log message is present in
			 * WEBPAlog.txt.0
			 *
			 */
			testStepNumber = "s4";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 4: DESCRIPTION : Verify WEBPA SET request log message is present in WEBPAlog.txt.0 ");
			LOGGER.info("STEP 4: ACTION : Execute command: 1. grep -i \"WDMP-C: SET Request\" /rdklogs/logs/Consolelog.txt.0");
			LOGGER.info("STEP 4: EXPECTED : Log message should present in WEBPAlog.txt.0");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to get the  log message under /rdklogs/logs/WEBPAlog.txt.0 ";
			status = BroadBandSystemUtils
					.verifyWebpaNotificationForPollingTime(tapEnv, device,
							BroadBandTraceConstants.WEBPA_SET_NOTIFICATION,
							currentDeviceTimeStamp);
			if (status) {
				LOGGER.info("S4 ACTUAL: WEBPA SET request log message is present in WEBPAlog.txt.0");
			} else {
				LOGGER.error("S4 ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

			/**
			 * STEP 5:Verify the WEBPA PUT command can be executed successfully
			 * 
			 * 
			 */
			testStepNumber = "s5";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 5:DESCRIPTION: Verify the WEBPA PUT command can be executed successfully");
			LOGGER.info("STEP 5:ACTION: Execute the TR-181 parameter-Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS to put the table entry in XDNS by WebPA");
			LOGGER.info("STEP 5:EXPECTED: should be able to put the table entry in XDNS table by WebPA");
			LOGGER.info("#####################################################################################");
			// Instance to store webPaServerResponse
			currentDeviceTimeStamp = BroadBandCommonUtils
					.getCurrentTimeStampOnDevice(tapEnv, device);
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			webPaServerResponse = BroadBandWebPaUtils.invokeRestCallToXDNS(
					tapEnv, device, BroadBandTestConstants.STRING_PUT);
			if (webPaServerResponse != null) {
				status = webPaServerResponse.getMessage().equalsIgnoreCase(
						BroadBandTestConstants.SUCCESS_TXT);
			}
			errorMessage = "Unable to put the  table entry using the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'  using WebPA command.";
			if (status) {
				LOGGER.info("S5 ACTUAL: Successfully able to put the table entry using the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'' using WebPA command.");
			} else {
				LOGGER.error("S5 ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

			/**
			 * Step 6: Verify WEBPA PUT request log message is present in
			 * WEBPAlog.txt.0
			 *
			 */
			testStepNumber = "s6";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 6: DESCRIPTION : Verify WEBPA PUT request log message is present in WEBPAlog.txt.0 ");
			LOGGER.info("STEP 6: ACTION : Execute command: 1. grep -i \"WDMP-C: REPLACE_ROWS Request\" /rdklogs/logs/Consolelog.txt.0");
			LOGGER.info("STEP 6: EXPECTED : Log message should present in WEBPAlog.txt.0");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to get the  log message under /rdklogs/logs/WEBPAlog.txt.0 ";
			status = BroadBandSystemUtils
					.verifyWebpaNotificationForPollingTime(tapEnv, device,
							BroadBandTraceConstants.WEBPA_PUT_NOTIFICATION,
							currentDeviceTimeStamp);
			if (status) {
				LOGGER.info("S6 ACTUAL: WEBPA PUT Log message should present in WEBPAlog.txt.0");
			} else {
				LOGGER.error("S6 ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

			/**
			 * STEP 7:Verify the WEBPA POST command can be executed successfully
			 * 
			 * 
			 */
			testStepNumber = "s7";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 7:DESCRIPTION :Verify the WEBPA POST command can be executed successfully");
			LOGGER.info("STEP 7:ACTION: Execute the TR-181 parameter-Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS to post the table entry in XDNS by WebPA");
			LOGGER.info("STEP 7:EXPECTED: should be able to post the table entry in XDNS table by WebPA");
			LOGGER.info("#####################################################################################");
			currentDeviceTimeStamp = BroadBandCommonUtils
					.getCurrentTimeStampOnDevice(tapEnv, device);
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			webPaServerResponse = BroadBandWebPaUtils.invokeRestCallToXDNS(
					tapEnv, device, BroadBandTestConstants.STRING_POST);
			LOGGER.info("webPaServerResponse is"
					+ webPaServerResponse.getMessage());
			String tableRowNumber = webPaServerResponse.getRow();
			LOGGER.info("tableRowNumber is" + tableRowNumber);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(
					BroadBandTestConstants.SUCCESS_TXT);
			errorMessage = "Unable to post the  the table entry using the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'' using WebPA command.";
			if (status) {
				LOGGER.info("S7 ACTUAL: Successfully able to post the table entry using the webpa parameter  'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'' using WebPA command.");
			} else {
				LOGGER.error("S7 ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

			/**
			 * Step 8: Verify WEBPA POST request log message is present in
			 * WEBPAlog.txt.0
			 *
			 */
			testStepNumber = "s8";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 8: DESCRIPTION : Verify WEBPA POST request log message is present in WEBPAlog.txt.0 ");
			LOGGER.info("STEP 8: ACTION : Execute command: 1. grep -i \"WDMP-C: ADD_ROW Request\" /rdklogs/logs/Consolelog.txt.0");
			LOGGER.info("STEP 8: EXPECTED : Log message should present in WEBPAlog.txt.0");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to get the  log message under /rdklogs/logs/WEBPAlog.txt.0 ";
			status = BroadBandSystemUtils
					.verifyWebpaNotificationForPollingTime(tapEnv, device,
							BroadBandTraceConstants.WEBPA_POST_NOTIFICATION,
							currentDeviceTimeStamp);
			if (status) {
				LOGGER.info("S8 ACTUAL: WEBPA POST Log message should present in WEBPAlog.txt.0");
			} else {
				LOGGER.error("S8 ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, false);

			/**
			 * Step 9: Verify the WEBPA DELETE command can be executed
			 * successfully
			 *
			 */
			testStepNumber = "s9";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 9: DESCRIPTION: Verify the WEBPA DELETE command can be executed successfully");
			LOGGER.info("STEP 9:ACTION: Execute the TR-181 parameter-Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS to delete the table entry in XDNS by WebPA");
			LOGGER.info("STEP 9: EXPECTED: should be able to delete the table entry in XDNS table by WebPA");
			LOGGER.info("#####################################################################################");
			currentDeviceTimeStamp = BroadBandCommonUtils
					.getCurrentTimeStampOnDevice(tapEnv, device);
			tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			errorMessage = "Null response obtained for using delete by webpa";
			WebPaServerResponse deleteResponse = tapEnv
					.deleteTableRowUsingRestApi(device, tableRowNumber);
			if (CommonMethods.isNotNull(deleteResponse.getMessage())) {
				status = deleteResponse.getMessage().equalsIgnoreCase(
						BroadBandTestConstants.SUCCESS_TXT);
				errorMessage = "Unable to delete the table row using webpa";
			}
			if (status) {
				LOGGER.info("S9 ACTUAL: table row deleted succesfully using webpa");
			} else {
				LOGGER.error("S9 ACTUAL: " + errorMessage
						+ " ACTUAL RESPONSE: " + deleteResponse.getMessage());
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, true);

			/**
			 * Step 10: Verify WEBPA DELETE request log message is present in
			 * WEBPAlog.txt.0
			 *
			 */
			testStepNumber = "s10";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 10: DESCRIPTION : Verify WEBPA DELETE request log message is present in WEBPAlog.txt.0 ");
			LOGGER.info("STEP 10: ACTION : Execute command: 1. grep -i \"WDMP-C: DELETE_ROW Request\" /rdklogs/logs/Consolelog.txt.0");
			LOGGER.info("STEP 10: EXPECTED : Log message should present in WEBPAlog.txt.0");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to get the  log message under /rdklogs/logs/WEBPAlog.txt.0 ";
			status = BroadBandSystemUtils
					.verifyWebpaNotificationForPollingTime(tapEnv, device,
							BroadBandTraceConstants.WEBPA_DELETE_NOTIFICATION,
							currentDeviceTimeStamp);
			if (status) {
				LOGGER.info("S10 ACTUAL: WEBPA DELETE Log message should present in WEBPAlog.txt.0");
			} else {
				LOGGER.error("S10 ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber,
					status, errorMessage, true);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception occured during execution !!!!"
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId,
					testStepNumber, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("#####################################################################################");
			LOGGER.info("POST-CONDITION 1:DESCRIPTION :Verify the WEBPA Set command is executed for default value.");
			LOGGER.info("POST-CONDITION 1:ACTION : Verify the WEBPA Set command is executed for default value");
			LOGGER.info("POST-CONDITION 1:EXPECTED: Should be able to WEBPA Set the default value");
			LOGGER.info("#####################################################################################");
			webPaServerResponse = BroadBandWebPaUtils
					.setWebPaParamAndReturnResp(
							tapEnv,
							device,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS,
							BroadBandTestConstants.FALSE,
							BroadBandTestConstants.CONSTANT_3);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(
					BroadBandTestConstants.SUCCESS_TXT);
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
	 * <li>3:Set DiagnosticState to 'Requested' using webpa wait for 2 minute
	 * and verify with 'Complete' webpa response</li>
	 * <li>4:Get Traceroute hops using Webpa command and validate hostaddress as
	 * IPv4 address</li>
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
			LOGGER.info("3.Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Complete' webpa response");
			LOGGER.info("4.Get Traceroute hops using Webpa command and validate hostaddress as IPv4 address");
			LOGGER.info("POST-CONDITION 1: Set Traceroute Host Address To Null");
			LOGGER.info("#####################################################################################");

			/**
			 * STEP 1:Retrieve IPv4 address with nslookup and save
			 */
			status = false;
			String response = null;
			testStep = "s1";
			LOGGER.info("*********************************************************************************************");
			LOGGER.info("STEP 1 : DESCRIPTION :Retrieve IPv4 address with nslookup and save");
			LOGGER.info("STEP 1 : ACTION :Execute command nslookup to  facebook.com");
			LOGGER.info("STEP 1 : EXPECTED:IPv4 address should be retrieved successfully and saved");
			LOGGER.info("*********************************************************************************************");
			errorMessage = "Unable to retrieve IPv4 address from nslookup response";
			try {
				response = tapEnv
						.executeCommandUsingSshConnection(
								WhiteListServer.getInstance(tapEnv,
										BroadbandPropertyFileHandler
												.getReverseSshJumpServer()),
								BroadBandCommonUtils
										.concatStringUsingStringBuffer(
												BroadBandCommandConstants.CMD_NSLOOKUP_WITH_PATH_FOR_IPV4_ADDRESS,
												BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK));
				if (CommonMethods.isNotNull(response)) {
					nslookupIPv4Addr = BroadBandCommonUtils
							.patternFinderForMultipleMatches(
									response,
									BroadBandTestConstants.PATTERN_TO_RETRIEVE_IPV4_ADDRESS_FROM_NSLOOKUP_FACEBOOK,
									BroadBandTestConstants.CONSTANT_1).get(0);
					status = CommonMethods.isNotNull(nslookupIPv4Addr)
							&& CommonMethods.isIpv4Address(nslookupIPv4Addr);
				}
			} catch (Exception e) {
				LOGGER.error("EXCEPTION OCCURED WHILE RETRIEVING IPV4 ADDRESS FROM NS LOOKUP :\n"
						+ e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP 1:ACTUAL :IPv4 Address are retrieved successfully from nslookup response");
			} else {
				LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
			}
			LOGGER.info("********************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status,
					errorMessage, true);

			/**
			 * STEP 2:Set Host address as IPv4 address using Webpa
			 */
			status = false;
			testStep = "s2";
			LOGGER.info("********************************************************************************************");
			LOGGER.info("STEP 2 : DESCRIPTION :Set Host address as IPv4 address retrieved from nslookup using Webpa");
			LOGGER.info("STEP 2 : ACTION :Execute Webpa SET command Device.IP.Diagnostics.TraceRoute.Host IPv4 address as value");
			LOGGER.info("STEP 2 : EXPECTED:Webpa should be success with Success 200 status code");
			LOGGER.info("*********************************************************************************************");
			errorMessage = "Unable to set IP address to traceroute host using webpa";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(
					device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_HOST,
					BroadBandTestConstants.CONSTANT_0, nslookupIPv4Addr);
			if (status) {
				LOGGER.info("STEP 2:ACTUAL :Webpa Set for Host address is successful");
			} else {
				LOGGER.error("STEP 2:ACTUAL :" + errorMessage);
			}
			LOGGER.info("********************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status,
					errorMessage, true);

			/**
			 * STEP 3:Set DiagnosticState to 'Requested' using webpa wait for 2
			 * minute and verify with 'Complete' webpa response
			 */
			status = false;
			testStep = "s3";
			LOGGER.info("*********************************************************************************************");
			LOGGER.info("STEP 3 : DESCRIPTION :Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Complete' webpa response");
			LOGGER.info("STEP 3 : ACTION :Execute Webpa get command Device.IP.Diagnostics.TraceRoute.DiagnosticsState Value:Requested");
			LOGGER.info("STEP 3 : EXPECTED:Webpa should be success with Success 200 status code");
			LOGGER.info("*********************************************************************************************");
			errorMessage = "Unable to set and Validate webpa request for traceroute diagnosticState";
			status = BroadBandWebPaUtils
					.setVerifyWebPAInPolledDurationForPassedValue(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_DIAGNOSTIC_STATE,
							BroadBandTestConstants.CONSTANT_0,
							BroadBandTestConstants.STRING_REQUESTED,
							BroadBandTestConstants.STRING_COMPLETE,
							BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
							BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 3:ACTUAL :Diagnostic state set sucessfully and verified with 'Complete' response");
			} else {
				LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status,
					errorMessage, true);
			/**
			 * STEP 4:Get Trace route hops using Webpa command and validate host
			 * address as IPv4 address
			 */
			status = false;
			testStep = "s4";
			LOGGER.info("**********************************************************************************************");
			LOGGER.info("STEP 4 : DESCRIPTION :Get Traceroute hops using Webpa command and validate hostaddress as IPv4 address");
			LOGGER.info("STEP 4 : ACTION :Execute Webpa GET command:Device.IP.Diagnostics.TraceRoute.RouteHops.");
			LOGGER.info("STEP 4 : EXPECTED:Webpa get request should be successful");
			LOGGER.info("*********************************************************************************************");
			errorMessage = "Unable to Validate traceroute from webpa request";
			bandResultObject = BroadBandWebPaUtils.validateTraceRouteOutput(
					device, tapEnv, false);
			errorMessage += bandResultObject.getErrorMessage();
			status = bandResultObject.isStatus();
			if (status) {
				LOGGER.info("STEP 4:ACTUAL :Traceroute hops validated successfully with Ipv4 address");
			} else {
				LOGGER.error("STEP 4:ACTUAL :" + errorMessage);
			}
			LOGGER.info("*********************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status,
					errorMessage, true);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Execution error in verifying traceroute "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId,
					testStep, status, errorMessage, true);
		} finally {
			errorMessage = "UNABLE TO RESTORE THE TRACEROUTE HOST ADDRESS";
			status = false;
			LOGGER.info("###############################STARTING POST-CONFIGURATIONS###############################");
			LOGGER.info("POST-CONDITION 1: DESCRIPTION:SET TRACEROUTE HOST ADDRESS TO NULL");
			LOGGER.info("POST CONDITION 1: ACTION:EXECUTE WEBPA COMMAND PARAMETER:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_HOST
					+ " with Value '"
					+ BroadBandTestConstants.STRING_NULL
					+ "'");
			LOGGER.info("POST-CONDITION 1: EXPECTED:TRACEROUTE HOST ADDRESS SHOULD BE RESTORED SUCCESSFULLY");
			LOGGER.info("##########################################################################");
			try {
				status = BroadBandWebPaUtils
						.setAndGetParameterValuesUsingWebPa(
								device,
								tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_HOST,
								BroadBandTestConstants.CONSTANT_0,
								BroadBandTestConstants.STRING_NULL);
				if (status) {
					LOGGER.info("POST-CONDITION 1 : ACTUAL : SUCCESSFULLY RESTORED THE TRACEROUTE HOST ADDRESS");
				} else {
					LOGGER.error("POST-CONDITION 1 : ACTUAL :" + errorMessage);
				}
			} catch (Exception e) {
				LOGGER.error("EXCEPTION OCCURED WHILE CONFIGURING POST CONDITION 1:\n"
						+ e.getMessage());
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
	 * <li>3:Set DiagnosticState to 'Requested' using webpa wait for 2 minute
	 * and verify with 'Completed' webpa response</li>
	 * <li>4:Get Traceroute hops using Webpa command and validate hostaddress as
	 * IPv6 address</li>
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
			LOGGER.info("3.Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Completed' webpa response");
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
			nslookupIPv6Addr = BroadBandCommonUtils
					.executeCommandOnJumpServerAndRetrievIPAddressWithPatternSearch(
							tapEnv,
							CommonUtils
									.concatStringUsingStringBuffer(
											BroadBandCommandConstants.CMD_NSLOOKUP_WITH_PATH_FOR_IPV6_ADDRESS,
											BroadBandTestConstants.SSID_NAME_WITH_WHITSPACE_ONLY,
											BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK),
							true,
							BroadBandTestConstants.PATTERN_TO_RETRIVE_IPV6_ADDRESS_FROM_NSLOOKUP_FACEBOOK,
							BroadBandTestConstants.CONSTANT_1);

			if (CommonMethods.isNotNull(nslookupIPv6Addr)
					&& CommonMethods.isIpv6Address(nslookupIPv6Addr)) {
				status = true;
				LOGGER.info("STEP 1:ACTUAL :IPv6 Address is retrieved successfully from nslookup response");
			} else {
				LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status,
					errorMessage, true);

			/**
			 * STEP 2:Set Host address as IPv6 address using Webpa
			 */
			status = false;
			testStep = "s2";
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 2 : DESCRIPTION :Set Host address as IPv6 address retrieved from nslookup using Webpa");
			LOGGER.info("STEP 2 : ACTION :Execute webpa command Device.IP.Diagnostics.TraceRoute.DiagnosticsState value Ipv6 address");
			LOGGER.info("STEP 2 : EXPECTED:Webpa should be success with Success 200 status code");
			errorMessage = "Unable to set IP address to traceroute host using webpa";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(
					device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_HOST,
					BroadBandTestConstants.CONSTANT_0, nslookupIPv6Addr);
			if (status) {
				LOGGER.info("STEP 2:ACTUAL :Webpa set IPv6 address to Host address is successful");
			} else {
				LOGGER.error("STEP 2:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status,
					errorMessage, true);
			/**
			 * STEP 3:Set DiagnosticState to 'Requested' using webpa wait for 2
			 * minute and verify with 'Completed' webpa response
			 */
			status = false;
			testStep = "s3";
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 3 : DESCRIPTION :Set DiagnosticState to 'Requested' using webpa wait for 2 minute and verify with 'Completed' webpa response");
			LOGGER.info("STEP 3 : ACTION :Execute Webpa SET and GET:Device.IP.Diagnostics.TraceRoute.DiagnosticsState with value:Requested");
			LOGGER.info("STEP 3 : EXPECTED:Webpa should be success with Success 200 status code and Get request should be validated successfully");
			errorMessage = "Unable to set and Validate webpa request for traceroute diagnosticState";
			status = BroadBandWebPaUtils
					.setVerifyWebPAInPolledDurationForPassedValue(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_DIAGNOSTIC_STATE,
							BroadBandTestConstants.CONSTANT_0,
							BroadBandTestConstants.STRING_REQUESTED,
							BroadBandTestConstants.STRING_COMPLETE,
							BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
							BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 3:ACTUAL :Diagnostic state is set and validated successfully");
			} else {
				LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status,
					errorMessage, true);
			/**
			 * STEP 4:Get Traceroute hops using Webpa command and validate
			 * hostaddress as IPv6 address
			 */
			status = false;
			testStep = "s4";
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 4 : DESCRIPTION :Get Traceroute hops using Webpa command and validate hostaddress as IPv6 address");
			LOGGER.info("STEP 4 : ACTION :Execute Webpa get command:evice.IP.Diagnostics.TraceRoute.RouteHops.");
			LOGGER.info("STEP 4 : EXPECTED:Webpa set request should be successful");
			errorMessage = "Unable to Validate traceroute from webpa request";
			bandResultObject = BroadBandWebPaUtils.validateTraceRouteOutput(
					device, tapEnv, true);
			if (bandResultObject.isStatus()) {
				status = true;
				LOGGER.info("STEP 4:ACTUAL :Traceroute hops is validated with IPv6 address");
			} else {
				LOGGER.error("STEP 4:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status,
					errorMessage, true);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Execution error in verifying traceroute "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId,
					testStep, status, errorMessage, true);
		} finally {
			LOGGER.info("###############################STARTING POST-CONFIGURATIONS###############################");
			LOGGER.info("POST-CONDITION 1: DESCRIPTION:SET TRACEROUTE HOST ADDRESS TO NULL");
			LOGGER.info("POST CONDITION 1: ACTION:EXECUTE WEBPA COMMAND PARAMETER:Device.IP.Diagnostics.TraceRoute.Host with Value 'null'");
			LOGGER.info("POST-CONDITION 1: EXPECTED:TRACEROUTE HOST ADDRESS SHOULD BE RESTORED SUCCESSFULLY");
			LOGGER.info("##########################################################################");
			status = false;
			try {
				status = BroadBandWebPaUtils
						.setAndGetParameterValuesUsingWebPa(
								device,
								tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_TRACEROUTE_HOST,
								BroadBandTestConstants.CONSTANT_0,
								BroadBandTestConstants.STRING_NULL);
				LOGGER.info("POST CONDITION 1:ACTUAL:TRACEROUTE RESTORATION STATUS: "
						+ status);
			} catch (Exception e) {
				LOGGER.error("EXCEPTION OCCURED WHILE CONFIGURING POST CONDITION 1:\n"
						+ e.getMessage());
			}

		}

	}

	/**
	 * Verify all reports for Harvester and LMLite are enabled and Verify
	 * CcspWebpaadapter responds to WEBPA GET, SET requests.
	 * <ol>
	 * <li>PRE CONDITION 1:Reboot the device</li>
	 * <li>PRE CONDITION 2:Verify whether WebPA is Up and Running in the Device.
	 * </li>
	 * <li>Verify WEBPAlog.txt.0 Logs for "Component caching is completed"
	 * message in the Atom Console.</li>
	 * <li>Verify parodus process is running on Arm side.</li>
	 * <li>Verify the Current Value for Network Device Status via WEB Pa
	 * Commands.</li>
	 * <li>Verify the Current Value for Network Device Traffic via WEB Pa
	 * Commands.</li>
	 * <li>Verify retrieving current value for the Interface Devices Wifi Via
	 * WEBPA Command.</li>
	 * <li>Verify Enabling the Network Device Status to True Via WEBPA Command.</li>
	 * <li>Verify Enabling the Network Device Traffic to True Via WEBPA Command.
	 * </li>
	 * <li>Verify enabling the
	 * "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled" to true using
	 * webpa command</li>
	 * <li>Verify the Current Value for
	 * "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled" via WEB Pa
	 * Commands.</li>
	 * <li>Set value to parameter
	 * "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod" using WEBpa</li>
	 * <li>Get value to parameter
	 * "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod" using WEBpa</li>
	 * <li>Set value to parameter
	 * "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod" using
	 * WEBpa</li>
	 * <li>Get value to parameter
	 * "Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod" using
	 * WEBpa</li>
	 * <li>Verify the Process harvestor.txt is running in atom and arm console.</li>
	 * <li>Verify Harvester.txt log contains the required string.</li>
	 * <li>Verify Enabling the Interface Devices Wifi to True Via WEBPA Command.
	 * </li>
	 * <li>Verify the Process lmlite.txt is running in atom and arm console.</li>
	 * <li>POST CONDITION 1: Revert the Network Device Status,Network Device
	 * Traffic and Interface Device WiFi to its default Values.</li>
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
		LOGGER.info("TEST DESCRIPTION: Verify all reports for Harvester and LMLite are enabled and Verify CcspWebpaadapter responds to WEBPA GET, SET requests.");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE CONDITION 1. Reboot the device");
		LOGGER.info("PRE CONDITION 2. Verify whether WebPA is Up and Running in the Device.");
		LOGGER.info("1. Verify WEBPAlog.txt.0 Logs for \"Component caching is completed\" message in the Atom and Arm Console.");
		LOGGER.info("2. Verify parodus process is running on ARM side.");
		LOGGER.info("3. Verify the Current Value for Network Device Status via WEB Pa Commands.");
		LOGGER.info("4. Verify the Current Value for Netwrok Device Traffic via WEB Pa Commands.");
		LOGGER.info("5. Verify retrieving current value for  the Interface Devices Wifi Via WEBPA Command.");
		LOGGER.info("6. Verify Enabling the Network Device Status to True Via WEBPA Command.");
		LOGGER.info("7. Verify Enabling the Network Device Traffic to True Via WEBPA Command.");
		LOGGER.info("8. Verify enabling the \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" to true using webpa command");
		LOGGER.info("9. Verify the Current Value for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" via WEB Pa Commands.");
		LOGGER.info("10. Set value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\" using WEBpa");
		LOGGER.info("11. Get value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\" using WEBpa");
		LOGGER.info("12. Set value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\" using WEBpa");
		LOGGER.info("13. Get value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\" using WEBpa");
		LOGGER.info("14. Verify the Process harvestor.txt is running in atom and arm console.");
		LOGGER.info("15. Verify Harvester.txt log conatins the required string.");
		LOGGER.info("16. Verify Enabling the Interface Devices Wifi to True Via WEBPA Command");
		LOGGER.info("17. Verify the Process lmlite.txt is running in atom and arm console.");
		LOGGER.info("POST CONDITION 1. Revert the Network Device Status,Network Device Traffic,NeighboringAP and Interface Device WiFi to its default Values.");

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
				status = CommonMethods.rebootAndWaitForIpAccusition(device,
						tapEnv);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : Device Rebooted successfully.");
			} else {
				LOGGER.error("PRE-CONDITION 1: ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR
								+ "PRE-CONDITION 1 : FAILED : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : Verify whether WebPA is Up and Running in the Device.");
			LOGGER.info("PRE-CONDITION 2 : ACTION : Verifying Successful webpa Get response ,in case of failure rechecking for 8 minutes.");
			LOGGER.info("PRE-CONDITION 2 : EXPECTED : WebPA should be Up and Running in the Device.");
			errorMessage = "WebPA is not Up and not Running in Device.";
			try {
				status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv,
						device, true);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION 2 : ACTUAL :  WebPA is Up and Running in the Device.");
			} else {
				LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR
								+ "PRE-CONDITION 2 : FAILED : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");

			errorMessage = "WEBPAlog.txt.0 log file doesnï¿½t contain the required String : \"Component caching is completed\"";

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION :Verify WEBPAlog.txt.0 Logs for \"Component caching is completed\" message in the Atom/Arm Console.");
			LOGGER.info("STEP 1: ACTION : Execute command: ssh <atom ip> and then executetail -f rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP 1: EXPECTED : Webpa should wait for system ready signal to process component caching.  webpa should proceed with caching within 7 mins after getting system ready signal.  \"Checked CR - System is ready, proceed with component caching\" & \"Component caching is completed. Hence setting cachingStatus to active\"should be present in the WEBPAlog.txt.0 log file");
			LOGGER.info("**********************************************************************************");

			response = BroadBandCommonUtils
					.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
							BroadBandTraceConstants.LOG_MESSAGE_COMP_CACHING,
							BroadBandCommandConstants.LOG_FILE_WEBPA,
							BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
							BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS);
			status = CommonMethods.isNotNull(response);
			if (!status) {
				try {
					status = CommonUtils
							.validateTraceLog(
									tapEnv,
									device,
									BroadBandTraceConstants.WEBPA_LOG_MESSAGE_COMP_CACHING_TRACE,
									BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
									true);
				} catch (Exception e) {
					LOGGER.error("Exception occured while validating device trace "
							+ e.getMessage());
				}
			}
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : WEBPAlog.txt.0 log file contains the required String: \"Component caching is completed\"");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "S2";
			errorMessage = "Parodus Process details is not displayed as (/usr/bin/parodus). ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify parodus process is running on ARM console.");
			LOGGER.info("STEP 2: ACTION : Execute the below command in ARM console: Command: root@Docsis-Gateway:/rdklogs/logs # pidof process ");
			LOGGER.info("STEP 2: EXPECTED : Parodus process details should be displayed");
			LOGGER.info("**********************************************************************************");

			response = CommonMethods.getPidOfProcess(device, tapEnv,
					BroadBandTestConstants.PROCESS_NAME_PARODUS);
			status = CommonMethods.isNotNull(response);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : parodus process is running on ARM side.");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "S3";
			errorMessage = "Unable to retrieve the status of 'Device.X_RDKCENTRAL-COM_Report.NetworkDevicesStatus.Enabled' using WebPA command.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify the Current Value for Network Device Status via WEB Pa Commands.");
			LOGGER.info("STEP 3: ACTION : Execute the Below commands: curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NetworkDevicesStatus.Enabled ");
			LOGGER.info("STEP 3: EXPECTED : Current value for Network Device Status should be retieved successfully using the WEB PA command. ");
			LOGGER.info("**********************************************************************************");
			String networkDevicesStatus = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_WIFI_REPORT);
			LOGGER.info("Network Devices Status retrieved using WebPa = "
					+ networkDevicesStatus);
			status = CommonMethods.isNotNull(networkDevicesStatus);
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Current value for the Network Device Status retrieved successfully");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "S4";
			errorMessage = "Unable to retrieve the Current value for the Network Device Traffic using the WEBPA parameter  Device.X_RDKCENTRAL-COM_Report.NetworkDevicesTraffic.Enabled ";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify the Current Value for Netwrok Device Traffic via WEB Pa Commands.");
			LOGGER.info("STEP 4: ACTION : Execute the Below commands: curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NetworkDevicesTraffic.Enabled ");
			LOGGER.info("STEP 4: EXPECTED : Current value for Network Device Traffic should be retieved successfully using the WEB PA command. ");
			LOGGER.info("**********************************************************************************");
			String networkDevicesTraffic = tapEnv
					.executeWebPaCommand(
							device,
							BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_TRAFFIC_WIFI_REPORT);
			LOGGER.info("Network Devices Traffic retrieved using WebPa = "
					+ networkDevicesTraffic);
			status = CommonMethods.isNotNull(networkDevicesTraffic);
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Current value for the Network Device Traffic retrieved successfully");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "S5";
			errorMessage = "Unable to retrieve the Current value for the Interface Device WiFi using the WEBPA parameter  Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify retrieving current value for  the Interface Devices Wifi Via WEBPA Command.");
			LOGGER.info("STEP 5: ACTION : Execute the below command: curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled ");
			LOGGER.info("STEP 5: EXPECTED : Current value for Interface Devices Wifi should be retieved successfully using the WEB PA command. ");
			LOGGER.info("**********************************************************************************");

			String interfaceDevicesWifi = tapEnv
					.executeWebPaCommand(
							device,
							BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
			LOGGER.info("Network Devices Traffic retrieved using WebPa = "
					+ interfaceDevicesWifi);
			status = CommonMethods.isNotNull(interfaceDevicesWifi);
			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Current value for the Interface Device WiFi retrieved successfully");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "S6";
			errorMessage = "Unable to enable the Network Device Status (Device.X_RDKCENTRAL-COM_Report.NetworkDevicesStatus.Enabled bool true) Parameter to true via WEBPA.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify Enabling the Network Device Status to True Via WEBPA Command.");
			LOGGER.info("STEP 6: ACTION : Execute the Below commands:                            curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NetworkDevicesStatus.Enabled bool true");
			LOGGER.info("STEP 6: EXPECTED : Enabling Network Device Status via WEBPA should be successful.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(
					device, tapEnv,
					BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_WIFI_REPORT,
					BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Network Device Status enabled to true successfully");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, true);
			LOGGER.info("**********************************************************************************");

			stepNum = "S7";
			errorMessage = "Unable to enable the Network Device Trafic (Device.X_RDKCENTRAL-COM_Report.NetworkDevicesTraffic.Enabled bool true) Parameter to true via WEBPA.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify Enabling the Network Device Traffic to True Via WEBPA Command.");
			LOGGER.info("STEP 7: ACTION : Execute the Below commands:                            curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NetworkDevicesTraffic.Enabled bool true");
			LOGGER.info("STEP 7: EXPECTED : Enabling Network Device Traffic via WEBPA should be successful.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils
					.setAndGetParameterValuesUsingWebPa(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_TRAFFIC_WIFI_REPORT,
							BroadBandTestConstants.CONSTANT_3,
							BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Network Device Trafic enabled to true successfully");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, true);
			LOGGER.info("**********************************************************************************");

			stepNum = "s8";
			errorMessage = "Unable to set value as true for the parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\"";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify enabling the \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" to true using webpa command");
			LOGGER.info("STEP 8: ACTION : Execute the below command: curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled \"");
			LOGGER.info("STEP 8: EXPECTED : \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" should get enabled to true");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils
					.setAndGetParameterValuesUsingWebPa(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_ENABLED_REPORT,
							BroadBandTestConstants.CONSTANT_3,
							BroadBandTestConstants.TRUE);

			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled parameter got enabled successfully");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "s9";
			errorMessage = "\"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" value is not returned as true";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify the Current Value for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" via WEB Pa Commands.");
			LOGGER.info("STEP 9: ACTION : Execute the Below command: curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=\"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\"");
			LOGGER.info("STEP 9: EXPECTED : \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled\" should return value as 1");
			LOGGER.info("**********************************************************************************");

			String neighboringAP = tapEnv
					.executeWebPaCommand(
							device,
							BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_ENABLED_REPORT);
			LOGGER.info("NeighboringAP retrieved using WebPa = "
					+ neighboringAP);
			status = CommonMethods.isNotNull(neighboringAP);

			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Current value for NeighboringAP value retrieved sucessfully");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "s10";
			errorMessage = "Value didnï¿½t get set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\"using the WEB PA command";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Set value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\" using WEBpa");
			LOGGER.info("STEP 10: ACTION : Execute the below command: curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod \"");
			LOGGER.info("STEP 10: EXPECTED : Value should be set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\"using the WEB PA command");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils
					.setAndGetParameterValuesUsingWebPa(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_POLLING_PERIOD,
							BroadBandTestConstants.CONSTANT_2,
							BroadBandTestConstants.STRING_CONSTANT_3600);

			if (status) {
				LOGGER.info("STEP 10: ACTUAL : Value got set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\"");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "s11";
			errorMessage = "Value retrieved is not expected for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\"using the WEB PA command";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Get value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\" using WEBpa");
			LOGGER.info("STEP 11: ACTION : Execute the below command: curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod \"");
			LOGGER.info("STEP 11: EXPECTED : Value should be retrieved successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.PollingPeriod\"using the WEB PA command");
			LOGGER.info("**********************************************************************************");

			String neighboringAPPollingPeriod = tapEnv
					.executeWebPaCommand(
							device,
							BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_POLLING_PERIOD);
			LOGGER.info("NeighboringAP retrieved using WebPa = "
					+ neighboringAPPollingPeriod);
			status = CommonMethods.isNotNull(neighboringAPPollingPeriod);

			if (status) {
				LOGGER.info("STEP 11: ACTUAL : Current value for NeighboringAP Polling period value retrieved sucessfully");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "s12";
			errorMessage = "Value didnï¿½t get set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"using the WEB PA command";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Set value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\" using WEBpa");
			LOGGER.info("STEP 12: ACTION : Execute the below command: curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"");
			LOGGER.info("STEP 12: EXPECTED : Value should be set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"using the WEB PA command");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils
					.setAndGetParameterValuesUsingWebPa(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_REPORTING_PERIOD,
							BroadBandTestConstants.CONSTANT_2,
							BroadBandTestConstants.STRING_CONSTANT_3600);

			if (status) {
				LOGGER.info("STEP 12: ACTUAL : Value got set successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "s13";
			errorMessage = "Value retrieved is not expected for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"using the WEB PA command";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Get value to parameter \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\" using WEBpa");
			LOGGER.info("STEP 13: ACTION : Execute the below command: curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"");
			LOGGER.info("STEP 13: EXPECTED : Value should be retrieved successfully for \"Device.X_RDKCENTRAL-COM_Report.NeighboringAP.ReportingPeriod\"using the WEB PA command");
			LOGGER.info("**********************************************************************************");

			String neighboringAPReportingPeriod = tapEnv
					.executeWebPaCommand(
							device,
							BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_REPORTING_PERIOD);
			LOGGER.info("NeighboringAP retrieved using WebPa = "
					+ neighboringAPReportingPeriod);
			status = CommonMethods.isNotNull(neighboringAPReportingPeriod);

			if (status) {
				LOGGER.info("STEP 13: ACTUAL : Current value for NeighboringAP Reporting period value retrieved sucessfully ");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "s14";
			errorMessage = "Harvestor process is not running in console";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 14: DESCRIPTION :  Verify the Process harvestor.txt is running in atom or arm console");
			LOGGER.info("STEP 14: ACTION : Execute pidof harvester");
			LOGGER.info("STEP 14: EXPECTED : Harvester process details should be displayed successfully");
			LOGGER.info("**********************************************************************************");
			isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device,
					tapEnv);
			response = CommonMethods.isAtomSyncAvailable(device, tapEnv) ? BroadBandCommonUtils
					.getPidOfProcessFromAtomConsole(device, tapEnv,
							BroadBandTestConstants.PROCESS_NAME_HARVESTER)
					: CommonMethods.getPidOfProcess(device, tapEnv,
							BroadBandTestConstants.PROCESS_NAME_HARVESTER);
			status = CommonMethods.isNotNull(response);

			if (status) {
				LOGGER.info("STEP 14: ACTUAL : Harvestor process is running");
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "S15";
			errorMessage = "Harvesterlog.txt.0 Log doesnï¿½t conatin the required String.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 15: DESCRIPTION : Verify Harvester.txt log contains the required string.");
			LOGGER.info("STEP 15: ACTION : Execute following commands: root@Docsis-Gateway:/rdklogs/logs# cat Harvesterlog.txt.0");
			LOGGER.info("STEP 15: EXPECTED : Harvestor Log should contain \"Init for parodus Success\"  and \"Sent message successfully to parodus\" should be displayed.\"");
			LOGGER.info("***********************************************************************************");

			if (CommonMethods.isNotNull(BroadBandCommonUtils
					.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
							BroadBandTraceConstants.LOG_MESSAGE_PARODUS_INIT,
							BroadBandCommandConstants.FILE_HARVESTER_LOG,
							BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
				errorMessage = "Harvesterlog.txt.0 Log doesnï¿½t conatin the init for parodus string";
				response = BroadBandCommonUtils
						.searchLogFilesInAtomOrArmConsoleByPolling(
								device,
								tapEnv,
								BroadBandTraceConstants.LOG_MESSAGE_SUCCCESS_TO_PARADOUS_LMLITE,
								BroadBandCommandConstants.FILE_HARVESTER_LOG,
								BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				status = CommonMethods.isNotNull(response);
			}
			if (status) {
				LOGGER.info("STEP 15: ACTUAL : Harvesterlog.txt.0 Log contains the required string");
			} else {
				LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "S16";
			errorMessage = "Unable to enable the  Interface Devices Wifi (Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled bool true) Parameter to true via WEBPA.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 16: DESCRIPTION : Verify Enabling the Interface Devices Wifi to True Via WEBPA Command.");
			LOGGER.info("STEP 16: ACTION : Execute the below command: curl -X SET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled ");
			LOGGER.info("STEP 16: EXPECTED : Enabling Interface Devices Wifi via WEBPA should be successful.");
			LOGGER.info("***********************************************************************************");

			status = BroadBandWebPaUtils
					.setAndGetParameterValuesUsingWebPa(
							device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT,
							BroadBandTestConstants.CONSTANT_3,
							BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 16: ACTUAL : Interface Devices Wifi enabled to true successfully");
			} else {
				LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "S17";
			errorMessage = " lmlite.txt process is not up and running.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 17: DESCRIPTION : Verify the Process lmlite.txt is running in arm console.");
			LOGGER.info("STEP 17: ACTION : Execute following command: /rdklogs/logs# ps | grep lmlite");
			LOGGER.info("STEP 17: EXPECTED : lmlite.txt process details should be displayed successfully. (usr/bin/lmlite). ");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device,
					BroadBandCommandConstants.CMD_PS_GREP_CCSP);
			status = CommonMethods.patternMatcher(response,
					BroadBandTestConstants.LMLITE_PROCESS_OUTPUT);
			status = CommonMethods.isNotNull(response);

			if (status) {
				LOGGER.info("STEP 17: ACTUAL : lmlite.txt process is up and running");
			} else {
				LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
					errorMessage, false);
			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device,
					testCaseId, stepNum, false, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Revert the Network Device Status,Network Device Traffic,NeighboringAP,and Interface Device WiFi to its default Values.");
			LOGGER.info("POST-CONDITION 1 : ACTION : Execute the below Command : Device.X_RDKCENTRAL-COM_Report.NetworkDeviceStatus.Enabled,Device.X_RDKCENTRAL-COM_Report.NetworkDeviceTraffic.Enabled,Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled,Device.X_RDKCENTRAL-COM_Report.NeighboringAP.Enabled");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : Reverting Back the Network Device Status,Network Device Traffic and Interface Device WiFi to its default Values should be succcessful.");
			HashMap<String, String> privateValueMap = new HashMap<String, String>();
			privateValueMap.put(
					BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_WIFI_REPORT,
					BroadBandTestConstants.FALSE);
			privateValueMap
					.put(BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_TRAFFIC_WIFI_REPORT,
							BroadBandTestConstants.FALSE);

			privateValueMap
					.put(BroadBandWebPaConstants.WEBPA_PARAM_NEIGHBORINGAP_ENABLED_REPORT,
							BroadBandTestConstants.FALSE);
			privateValueMap
					.put(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT,
							BroadBandTestConstants.FALSE);

			status = BroadBandWebPaUtils.executeMultipleWebpaParametersSet(
					device, tapEnv, privateValueMap,
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

}
