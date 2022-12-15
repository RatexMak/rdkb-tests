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
package com.automatics.rdkb.tests.wifi.connectedclients;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.enums.StbProcess;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTelemetryConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants.sshTelnetCommandExpectedOutput;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.enums.BroadBandWhixEnumConstants.WEBPA_AP_INDEXES;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.BroadBandBandSteeringUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandMeshUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetry2Utils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetryUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWifiWhixUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils.WifiOperatingStandard;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.webpa.WebPaServerResponse;
import com.automatics.utils.AutomaticsPropertyUtility;

public class BroadBandWiFiConnectedClientTests extends AutomaticsTestBase {

	/**
	 * Test to verify that Wi-Fi connectivity of 2.4GHz frequency SSID is not
	 * affected when 5GHz frequency SSID is disabled
	 * <ol>
	 * <li>STEP 1:Disable Private 2.4 GHz SSID via WebPA</li>
	 * <li>STEP 2:Verify the Private 2.4 GHz SSID enabled status via WebPA</li>
	 * <li>STEP 3:Verify the Private 5 GHz SSID enabled status via WebPA</li>
	 * <li>STEP 4: Connect the device to 5 GHz SSID and verify connection
	 * status</li>
	 * <li>STEP 5:Verify whether interface got the correct IPv4 address.</li>
	 * <li>STEP 6:Verify whether interface got the correct IPv6 address.</li>
	 * <li>STEP 7: Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * <li>STEP 8: Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * <li>STEP 9:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error
	 * like \"Failed to get parameter value of\"</li>
	 * <li>STEP 10:Verify dmcli eRT getv Device.WiFi. - has to give more than 23
	 * params.</li>
	 * <li>Postcondition1:Revert WIFI SSID to its original state</li>
	 * </ol>
	 * 
	 * @param Dut {@link device}
	 * @author anandam.s
	 * @refactor Athira
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-4001")
	public void testVerifyConnectivityOfClientDeviceWhichConnectedWith5GhzOnlyEnabledRdkbGateway(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-401";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;
		String deviceDateTime = null;
		try {

			LOGGER.info("STARTING TEST CASE: " + testId);

			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"TEST DESCRIPTION: Test to Verify that LAN side 5GHz wireless CPE gets an IP address when only 5 GHZ radio is enabled in Cable modem");
			LOGGER.info("STEP 1:Disable Private 2.4 GHz SSID via WebPA");
			LOGGER.info("STEP 2:Verify the Private 2.4 GHz SSID enabled status via WebPA ");
			LOGGER.info("STEP 3:Verify the Private 5 GHz SSID enabled status via WebPA ");
			LOGGER.info("STEP 4: Connect the device to 5 GHz SSID and verify connection status");
			LOGGER.info("STEP 5:Verify whether interface got the correct IPv4  address.");
			LOGGER.info("STEP 6:Verify whether interface got the correct IPv6  address.");
			LOGGER.info("STEP 7: Verify whether you have connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP 8: Verify whether you have connectivity using that particular interface using IPV6 ");
			LOGGER.info(
					"STEP 9:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
			LOGGER.info("STEP 10:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
			LOGGER.info("Postcondition1:Revert WIFI SSID to its original state");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1 Disable Private 2.4 GHz SSID via WebPA
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: Description :Disable Private 2.4 GHz SSID via WebPA");
			LOGGER.info("STEP 1: Action: Execute command Private 2.4 GHz SSID via WebPA "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS);
			LOGGER.info(
					"STEP 1:EXPECTED: Device should disable the Private 2.4 GHZ SSID and WebPA command return success response.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Private 2.4 GHZ SSID is not disabled ";
			// Disable 2.4ghz radio
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)
					&& BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
							WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, false);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successsfully Disabled Private 2.4 GHz SSID via WebPA");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 2 Verify the 2.4 GHz SSID enabled status via WebPA
			 */
			testStepNumber = "s2";
			status = false;
			errorMessage = "Private 2.4 GHZ SSID is not disabled ";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2:DESCRIPTION:Verify the 2.4 GHz SSID enabled status via WebPA ");
			LOGGER.info("STEP 2: ACTION: Execute WebPA command to verify Private 2.4 GHz SSID enabled status");
			LOGGER.info("STEP 2 :EXPECTED: Device should return the  Private 2.4 GHz SSID enabled status as \"Down\"");
			LOGGER.info("**********************************************************************************");
			// verify 2.4ghz radio status
			Map<String, String> radioSatusForAllBands = BroadBandConnectedClientUtils.getAllRadioStatus(tapEnv, device);
			String radioStatus = radioSatusForAllBands
					.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID);
			if (null != radioStatus) {
				errorMessage = "Verification of 2.4Ghz Private SSID status failed. EXPECTED ssid status is : "
						+ BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN + ", but we retrieved " + radioStatus
						+ " value from WebPa command.";
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN.equalsIgnoreCase(radioStatus);
			} else {
				errorMessage = "Unable to fetch the 2.4Ghz Private SSID status using Webpa paramete"
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID;
			}
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successsfully Verified the 2.4 GHz SSID enabled status via WebPA");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 3 Verify the 5 GHz SSID enabled status via WebPA
			 */
			testStepNumber = "s3";
			status = false;
			radioStatus = null;
			errorMessage = "Private 5 GHZ SSID is not enabled ";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3:DESCRIPTION:Verify the 5 GHz SSID enabled status via WebPA ");
			LOGGER.info("STEP 3: ACTION: Execute WebPA command to verify Private 5 GHz SSID enabled status");
			LOGGER.info("STEP 3 :EXPECTED: Device should return the  Private 5 GHz SSID enabled status as \"Up\"");
			LOGGER.info("**********************************************************************************");
			// verify 5ghz radio status
			radioStatus = radioSatusForAllBands
					.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID);
			if (null != radioStatus) {
				errorMessage = "Verification of 5Ghz Private SSID status failed. EXPECTED ssid status is : "
						+ BroadBandConnectedClientTestConstants.RADIO_STATUS_UP + ", but we retrieved " + radioStatus
						+ " value from WebPa command.";
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_UP.equalsIgnoreCase(radioStatus);
			} else {
				errorMessage = "Unable to fetch the 5Ghz Private SSID status using Webpa paramete"
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
			}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Successsfully Verified the 5 GHz SSID enabled status via WebPA");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 4 Connect the device to 5 GHz SSID and verify connection status
			 */
			testStepNumber = "s4";
			status = false;
			errorMessage = "Connection to 5Ghz device failed";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: Description : Connect the device to 5 GHz SSID and verify connection status");
			LOGGER.info("STEP 4: Action : Connect the client to 5 GHz SSID");
			LOGGER.info("STEP 4: EXPECTED: Device should be connected with 5 GHz wifi network");
			LOGGER.info("**********************************************************************************");
			try {
				// get all connected devices and connect a device to 5Ghz
				connectedDeviceActivated = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
				status = null != connectedDeviceActivated;
			} catch (Exception exception) {
				errorMessage = errorMessage + "  Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Device should be connected with 5 GHz wifi network");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Steps 5 to 8
			 */
			BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, connectedDeviceActivated,
					testId, new String[] { "s5", "s6", "s7", "s8" });
			/**
			 * Steps 9 to 10
			 */
			wifiDmcliParseCheck(device, tapEnv, testId, deviceDateTime, BroadBandTestConstants.CONSTANT_8);

		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			LOGGER.info("**********************************************************************************");
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		} finally {
			BroadBandConnectedClientUtils.resetAllRadios(device, tapEnv);

		}

		LOGGER.info("ENDING TESTCASE : " + testId);

	}

	/**
	 * Test to verify that Wi-Fi connectivity of 2.4GHz frequency SSID is not
	 * affected when 5GHz frequency SSID is disabled
	 * <ol>
	 * <li>STEP 1:Disable Private 5 GHz SSID via WebPA</li>
	 * <li>STEP 2:Verify the Private 5 GHz SSID enabled status via WebPA</li>
	 * <li>STEP 3:Verify the Private 2.4 GHz SSID enabled status via WebPA</li>
	 * <li>STEP 4: Connect the device to 2.4 GHz SSID and verify connection
	 * status</li>
	 * <li>STEP 5:Verify whether interface got the correct IPv4 address.</li>
	 * <li>STEP 6:Verify whether interface got the correct IPv6 address.</li>
	 * <li>STEP 7: Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * <li>STEP 8: Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * <li>STEP 9:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error
	 * like \"Failed to get parameter value of\"</li>
	 * <li>STEP 10:Verify dmcli eRT getv Device.WiFi. - has to give more than 23
	 * params.</li>
	 * <li>Postcondition1:Revert WIFI SSID to its original state</li>
	 * </ol>
	 * 
	 * @author anandam.s
	 * @refactor Athira
	 * @param device instance of {@link Dut}
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-4002")
	public void testToVerifyIPAddressWith5GHZDisabled(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-402";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		String deviceDateTime = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;

		try {

			LOGGER.info("STARTING TEST CASE: " + testId);

			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"TEST DESCRIPTION: Test toVerify that Wi-Fi connectivity of 2.4GHz frequency SSID is not affected when 5GHz frequency SSID is disabled");
			LOGGER.info("STEP 1:Disable Private 5 GHz SSID via WebPA");
			LOGGER.info("STEP 2:Verify the Private 5 GHz SSID enabled status via WebPA ");
			LOGGER.info("STEP 3:Verify the Private 2.4 GHz SSID enabled status via WebPA ");
			LOGGER.info("STEP 4: Connect the device to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP 5:Verify whether interface got the correct IPv4  address.");
			LOGGER.info("STEP 6:Verify whether interface got the correct IPv6  address.");
			LOGGER.info("STEP 7: Verify whether you have connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP 8: Verify whether you have connectivity using that particular interface using IPV6 ");
			LOGGER.info(
					"STEP 9:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
			LOGGER.info("STEP 10:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
			LOGGER.info("Postcondition1:Revert WIFI SSID to its original state");
			LOGGER.info("#######################################################################################");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1:Description:Disable Private 5 GHz SSID via WebPA");
			LOGGER.info("STEP 1:Action:Execute command Private 5 GHz SSID via WebPA "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS);
			LOGGER.info(
					"STEP 1: EXPECTED: Device should disable the Private 5 GHZ SSID and WebPA command return success response.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Private 5GHZ SSID is not disabled ";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			// Disable 5ghz radio
			status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
					WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, false);
			if (status) {
				LOGGER.info("STEP 1 : ACTUAL : Private 5 GHZ SSID disabled and WebPA command return success response");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			// wait for 1.5 min
			LOGGER.info("Waiting for 1.5 minutes");
			LOGGER.info("**********************************************************************************");
			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2:DESCRIPTION:Verify the Private 5 GHz SSID Disabled status via WebPA ");
			LOGGER.info("STEP 2: ACTION: Execute WebPA command to verify Private 5GHz SSID disable status");
			LOGGER.info("STEP 2 :EXPECTED: Device should return the  Private 5 GHz SSID enabled status as \"Down\"");
			LOGGER.info("**********************************************************************************");

			testStepNumber = "s2";
			status = false;
			errorMessage = "Private 5 GHZ SSID is not disabled ";
			// verify 5ghz radio status
			Map<String, String> radioSatusForAllBands = BroadBandConnectedClientUtils.getAllRadioStatus(tapEnv, device);
			String radioStatus = radioSatusForAllBands
					.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID);
			if (null != radioStatus) {
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN.equalsIgnoreCase(radioStatus);
				if (status) {
					LOGGER.info("STEP 2 : ACTUAL : Device returned the  Private 5 GHz SSID enabled status as Down");
				} else {
					errorMessage = "Verification of 5Ghz Private SSID status failed. EXPECTED ssid status is : "
							+ BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN + ", but we retrieved "
							+ radioStatus + " value from WebPa command.";
					LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
				}
			} else {
				errorMessage = "Unable to fetch the 5Ghz Private SSID status using Webpa paramete"
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3:DESCRIPTION:Verify the Private 2.4 GHz SSID enabled status via WebPA ");
			LOGGER.info("STEP 3: ACTION: Execute WebPA command to verify Private 2.4GHz SSID enabled status");
			LOGGER.info("STEP 3 :EXPECTED: Device should return the  Private 2.4 GHz SSID enabled status as \"Up\"");
			LOGGER.info("**********************************************************************************");

			testStepNumber = "s3";
			status = false;
			errorMessage = "Private 2.4 GHZ SSID is not enabled ";
			// verify 2.4ghz radio status
			radioStatus = radioSatusForAllBands
					.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID);
			if (null != radioStatus) {
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_UP.equalsIgnoreCase(radioStatus);
				if (status) {
					LOGGER.info("STEP 3 : ACTUAL : Device returned the  Private 2.4 GHz SSID enabled status as Up");
				} else {

					errorMessage = "Verification of 2.4Ghz Private SSID status failed. EXPECTED ssid status is : "
							+ BroadBandConnectedClientTestConstants.RADIO_STATUS_UP + ", but we retrieved "
							+ radioStatus + " value from WebPa command.";
					LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
				}
			} else {
				errorMessage = "Unable to fetch the 2.4Ghz Private SSID status using Webpa paramete"
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID;
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: Description : Connect the device to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP 4: Action : Connect the client to 2.4 GHz SSID");
			LOGGER.info("STEP 4: EXPECTED: Device should be connected with 2.4 GHz wifi network");
			LOGGER.info("**********************************************************************************");

			testStepNumber = "s4";
			status = false;
			errorMessage = "Connection to 2.4Ghz device failed";

			try {
				// get the 2.4ghz SSId and password of device device
				connectedDeviceActivated = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
				if (null != connectedDeviceActivated) {
					status = true;
				} else {
					errorMessage = "Unable to connect to 2.4GHz private SSID when 5GHZ Private SSID disabled mode";
				}
			} catch (Exception exception) {
				errorMessage = errorMessage + "  Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/** Steps s5 to s8 */
			BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, connectedDeviceActivated,
					testId, new String[] { "s5", "s6", "s7", "s8" });
			/**
			 * Steps 9 to 10
			 */
			wifiDmcliParseCheck(device, tapEnv, testId, deviceDateTime, BroadBandTestConstants.CONSTANT_8);

		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			LOGGER.info("**********************************************************************************");
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		} finally {
			BroadBandConnectedClientUtils.resetAllRadios(device, tapEnv);

		}

		LOGGER.info("ENDING TEST CASE: " + testId);

	}

	/**
	 * Test to verify that LAN side wireless CPE gets an IP address when dual radio
	 * bands(2.4GHZ and 5 GHZ) are enabled in CM
	 * 
	 * @param device instance of {@link Dut}
	 * @author Anandam S
	 * @Refactor Sruthi Santhosh
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-4003")
	public void testVerifyConnectivityOfClientDeviceWithGatewayEnabledWithBothPrivateSsid(Dut device) {
		boolean status = false;
		String testId = "TC-RDKB-WIFI-403";
		int stepNumber = 1;
		String testStepNumber = "S" + stepNumber;
		String errorMessage = null;
		Dut connectedDeviceActivated = null;
		try {
			LOGGER.info("###################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-4003");
			LOGGER.info(
					"TEST DESCRIPTION: Verify that LAN side wireless CPE gets an IP address when dual radio bands(2.4GHZ and 5 GHZ) are enabled in CM");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("Step 1: Verify the Private 2.4 GHz SSID enabled status via WebPA");
			LOGGER.info("Step 2: Verify the Private 5 GHz SSID enabled status via WebPA");
			LOGGER.info("Step 3: Connect the device to 2.4 GHz SSID and verify connection status");
			LOGGER.info("Step 4: Verify whether interface got the correct IPv4 address.");
			LOGGER.info("Step 5: Verify whether interface got the correct IPv6 address.");
			LOGGER.info("Step 6: Verify whether you have connectivity using that particular interface using IPV4");
			LOGGER.info("Step 7: Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info("Step 8: Connect the device to 5 GHz SSID and verify connection status");
			LOGGER.info("Step 9: Verify whether interface got the correct IPv4 address.");
			LOGGER.info("Step 10: Verify whether interface got the correct IPv6 address.");
			LOGGER.info("Step 11: Verify whether you have connectivity using that particular interface using IPV4");
			LOGGER.info("Step 12: Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info("###################################################################################");

			/**
			 * STEP 1 : VERIFY THE PRIVATE 2.4 GHZ SSID ENABLED STATUS
			 */
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : Verify the Private 2.4 GHz SSID enabled status");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : Verify the Private 2.4 GHz SSID enabled status using webpa.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPTECTED : Device should return the  Private 2.4 GHz SSID enabled status as \"Up\"");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Private 2.4 GHZ SSID status is not UP ";
			// verify 2.4 ghz radio status
			Map<String, String> radioSatusForAllBands = BroadBandConnectedClientUtils.getAllRadioStatus(tapEnv, device);
			String radioStatus = radioSatusForAllBands
					.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID);
			if (null != radioStatus) {
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_UP.equalsIgnoreCase(radioStatus);
				if (!status) {
					errorMessage = "Verification of 2.4GHz Private SSID status failed. EXPECTED ssid status is : "
							+ BroadBandConnectedClientTestConstants.RADIO_STATUS_UP + ", but we retrieved "
							+ radioStatus + " value from WebPa command.";
				}
			} else {
				errorMessage = "Unable to fetch the 2.4GHz Private SSID status using Webpa parameter "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID;
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL :  Successfully Verified the 2.4 GHz ssid status and its enabled");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2 : VERIFY THE PRIVATE 5 GHZ SSID ENABLED STATUS
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : Verify the Private 5 GHz SSID enabled status");
			LOGGER.info(
					"STEP :  " + stepNumber + " : ACTION : Verify the Private 5 GHz SSID enabled status using webpa.");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPTECTED : Device should return the  Private 5 GHz SSID enabled status as \"Up\"");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Private 5 GHZ SSID status is not UP ";
			// verify 5ghz radio status
			radioStatus = radioSatusForAllBands
					.get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID);
			if (null != radioStatus) {
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_UP.equalsIgnoreCase(radioStatus);
				if (!status) {
					errorMessage = "Verification of 5GHz Private SSID status failed. EXPECTED ssid status is : "
							+ BroadBandConnectedClientTestConstants.RADIO_STATUS_UP + ", but we retrieved "
							+ radioStatus + " value from WebPa command.";
				}
			} else {
				errorMessage = "Unable to fetch the 5GHz Private SSID status using Webpa parameter "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL :  Successfully Verified the 5 GHz ssid status and its enabled");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3 : CONNECT THE CLIENT TO 2.4 GHZ
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			connectedDeviceActivated = null;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : Verify connecting the wi-fi client in the device to 2.4 GHz ssid");
			LOGGER.info(
					"STEP :  " + stepNumber + " : ACTION : Connect the wi-fi client with 2.4 GHz ssid and password");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED : the connection must be successful");
			LOGGER.info("***************************************************************************************");
			errorMessage = "Failed to connected the wifi client to 2.4 GHz ssid";
			try {
				connectedDeviceActivated = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (connectedDeviceActivated != null);
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: Connected the wifi client to 2.4 GHz ssid successfully");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 4-7 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED WITH
			 * PRIVATE WIFI 5 GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, connectedDeviceActivated,
					stepNumber);

			/**
			 * STEP 8 : CONNECT THE CLIENT TO 5 GHZ
			 */
			stepNumber = 8;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			connectedDeviceActivated = null;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : Verify connecting the wi-fi client in the device to 5ghz ssid");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : Connect the wi-fi client with 5ghz ssid and password");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED : the connection must be successful");
			LOGGER.info("***************************************************************************************");
			errorMessage = "Failed to connected the wifi client to 5ghz ssid";
			try {
				connectedDeviceActivated = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (connectedDeviceActivated != null);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 5GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 9-12 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH PRIVATE WIFI 5 GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6
			 * INTERFACE.
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, connectedDeviceActivated,
					stepNumber);
		} catch (Exception exception) {
			errorMessage = errorMessage + exception.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		} finally {
			BroadBandPostConditionUtils.executePostConditionToVerifyDefaultRadioStatus(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
		}
		LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-4003");
	}

	/**
	 * Verify the enable and disable functionality of both radios(2.4 GHz and 5 GHz)
	 * is working fine
	 * <ol>
	 * <li>STEP 1:Disable Private 2.4 GHz SSID via WebPA</li>
	 * <li>STEP 2:Verify the Private 2.4 GHz SSID Disabled status via WebPA</li>
	 * <li>STEP 3:Scan the broadcasting WI-Fi network from client device and check
	 * the connectivity status</li>
	 * <li>STEP 4:Disable Private 5 GHz SSID via WebPA</li>
	 * <li>STEP 5:Verify the Private 5 GHz SSID enabled status via WebPA</li>
	 * <li>STEP 6:Scan the broadcasting WI-Fi network from client device and check
	 * the connectivity status</li>
	 * <li>STEP 7:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error
	 * like \"Failed to get parameter value of\"</li>
	 * <li>STEP 8:Verify dmcli eRT getv Device.WiFi. - has to give more than 23
	 * params.</li>
	 * <li>STEP 9:Enable the Private 2.4 GHz SSID via WebPA</li>
	 * <li>STEP 10:Verify the Private 2.4 GHz SSID Disabled status via WebPA</li>
	 * <li>STEP 11: Connect the device to 2.4 GHz SSID and verify connection
	 * status</li>
	 * <li>STEP 12:Verify whether interface got the correct IPv4 address.</li>
	 * <li>STEP 13:Verify whether interface got the correct IPv6 address.</li>
	 * <li>STEP 14:Enable the Private 5 GHz SSID via WebPA</li>
	 * <li>STEP 15:Verify the Private 5 GHz SSID Disabled status via WebPA</li>
	 * <li>STEP 16: Connect the device to 5 GHz SSID and verify connection
	 * status</li>
	 * <li>STEP 17:Verify whether interface got the correct IPv4 address.</li>
	 * <li>STEP 18:Verify whether interface got the correct IPv6 address.</li>
	 * <li>STEP 19:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error
	 * like \"Failed to get parameter value of\"</li>
	 * <li>STEP 20:Verify dmcli eRT getv Device.WiFi. - has to give more than 23
	 * params.</li> *
	 * <li>Postcondition1:Revert WIFI SSID to its original state</li>
	 * </ol>
	 * 
	 * @author anandam.s
	 * 
	 * @param device {@link Device}
	 * @Refactor Sruthi Santhosh
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-4004")
	public void testVerifyConnectivityOfClientDeviceWhenEnableOrDisablingSsid(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-404";
		// Test step number
		String radioStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		String deviceDateTime = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;
		try {
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-4004");
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"TEST DESCRIPTION: Test toVerify that Wi-Fi connectivity of 2.4GHz frequency SSID is not affected when 5GHz frequency SSID is disabled");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("STEP 1:Disable Private 2.4 GHz SSID via WebPA");
			LOGGER.info("STEP 2:Verify the Private 2.4 GHz SSID Disabled status via WebPA ");
			LOGGER.info(
					"STEP 3:Scan the broadcasting WI-Fi network from client device and check the connectivity status");
			LOGGER.info("STEP 4:Disable Private 5 GHz SSID via WebPA");
			LOGGER.info("STEP 5:Verify the Private 5 GHz SSID enabled status via WebPA ");
			LOGGER.info(
					"STEP 6:Scan the broadcasting WI-Fi network from client device and check the connectivity status");
			LOGGER.info(
					"STEP 7:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
			LOGGER.info("STEP 8:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
			LOGGER.info("STEP 9:Enable the Private 2.4 GHz SSID via WebPA");
			LOGGER.info("STEP 10:Verify the Private 2.4 GHz SSID Disabled status via WebPA ");
			LOGGER.info("STEP 11: Connect the device to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP 12:Verify whether interface got the correct IPv4  address.");
			LOGGER.info("STEP 13:Verify whether interface got the correct IPv6  address.");
			LOGGER.info("STEP 14:Enable the Private 5 GHz SSID via WebPA");
			LOGGER.info("STEP 15:Verify the Private 5 GHz SSID Disabled status via WebPA ");
			LOGGER.info("STEP 16: Connect the device to 5 GHz SSID and verify connection status");
			LOGGER.info("STEP 17:Verify whether interface got the correct IPv4  address.");
			LOGGER.info("STEP 18:Verify whether interface got the correct IPv6  address.");
			LOGGER.info(
					"STEP 19:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
			LOGGER.info("STEP 20:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
			LOGGER.info("#######################################################################################");

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION:Disable Private 2.4 GHz SSID via WebPA");
			LOGGER.info("STEP 1: ACTION: Execute WebPA command to disable Private 2.4 GHz SSID");
			LOGGER.info(
					"STEP 1: EXPECTED: Device should disable the Private 2.4 GHZ SSID and WebPA command return success response.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Private 2.4GHZ SSID is not disabled ";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			// Disable 2.4ghz radio
			status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
					WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, false);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Private 2.4 GHz SSID is disabled via WebPA");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

			radioStepNumber = "s2";
			status = false;
			errorMessage = "Private 2.4 GHZ SSID is not disabled ";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION: Verify the Private 2.4 GHz SSID Disabled status via WebPA ");
			LOGGER.info("STEP 2: ACTION: Execute WebPA command to verify Private 2.4 GHz SSID disable status");
			LOGGER.info("STEP 2: EXPECTED: Device should return the  Private 2.4 GHz SSID enabled status as \"Down\"");
			LOGGER.info("**********************************************************************************");
			// verify 5ghz radio status
			String radioStatus = BroadBandConnectedClientUtils.getRadioStatus(WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv,
					device);
			if (null != radioStatus) {
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN.equalsIgnoreCase(radioStatus);
				if (!status) {
					errorMessage = "Verification of 2GHz Private SSID status failed. EXPECTED ssid status is : "
							+ BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN + ", but we retrieved "
							+ radioStatus + " value from WebPa command.";
				}
			} else {
				errorMessage = "Unable to fetch the 2GHz Private SSID status using Webpa parameter "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID;
			}
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Private 2.4 GHz SSID is in disable status");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

			/** Wait for 4 mins for the changes to get applied */
			LOGGER.info("Waiting for 4 minutes");
			tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);

			radioStepNumber = "s3";
			status = false;
			errorMessage = "Client device(s) is listed the Private 2.4 Ghz SSID Name in available network list";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION: Scan the broadcasting WI-Fi network from client device and check the connectivity status");
			LOGGER.info("STEP 3: ACTION: verify whether the 2.4Ghz SSID is not broadcasted");
			LOGGER.info(
					"STEP 3:EXPECTED: Client device(s) should not list the Private 2.4 Ghz SSID Name in available network list.");
			LOGGER.info("**********************************************************************************");
			boolean isVisible = false;
			String connectionType = null;
			errorMessage = "Client device(s) is listed the Private 2.4 Ghz SSID Name in available network list";
			// verify whether 2.4ghz ssid is visible in client devices
			List<Dut> connectedDevices = ((Device) device).getConnectedDeviceList();
			if (null != connectedDevices) {
				for (Dut connectedDevice : connectedDevices) {
					connectionType = ((Device) connectedDevice).getConnectedDeviceInfo().getConnectionType();
					if (CommonMethods.isNotNull(connectionType) && connectionType.trim().equalsIgnoreCase(
							BroadBandConnectedClientTestConstants.STRING_CLIENT_DEVICE_CONNECTION_TYPE_WIFI)) {
						isVisible = BroadBandConnectedClientUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDevice(
								connectedDevice, BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(
										device, tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ),
								tapEnv);
					}
					if (isVisible) {
						errorMessage = "2.4GHz SSID is listed in client device  model " + connectedDevice.getModel();
						LOGGER.info(errorMessage);
						break;
					}
				}
				status = !isVisible;
				if (status) {
					LOGGER.info("STEP 3: ACTUAL : 2.4GHz SSID is not listed in client device  model");
				} else {
					LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

				/**
				 * Step 4
				 */
				radioStepNumber = "s4";
				status = false;
				errorMessage = "Private 5GHZ SSID is not disabled ";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 4:DESCRIPTION:Disable Private 5 GHz SSID via WebPA");
				LOGGER.info("STEP 4: ACTION: Execute WebPA command to disable Private 5GHz SSID");
				LOGGER.info(
						"STEP 4:EXPECTED: Device should disable the Private 5 GHZ SSID and WebPA command return success response.");
				LOGGER.info("**********************************************************************************");
				// Disable 5ghz radio
				status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
						WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, false);
				if (status) {
					LOGGER.info("STEP 4: ACTUAL : Private 5GHz SSID is disabled via WebPA");
				} else {
					LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

				/**
				 * Step 5
				 */
				radioStepNumber = "s5";
				status = false;
				errorMessage = "Private 5 GHZ SSID is not disabled ";
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 5:DESCRIPTION:Verify the Private 5 GHz SSID Disabled status via WebPA ");
				LOGGER.info("STEP 5: ACTION: Execute WebPA command to verify Private 5GHz SSID disable status");
				LOGGER.info(
						"STEP 5: :EXPECTED: Device should return the  Private 5 GHz SSID enabled status as \"Down\"");
				LOGGER.info("**********************************************************************************");
				// verify 5ghz radio status
				radioStatus = BroadBandConnectedClientUtils.getRadioStatus(WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv,
						device);
				if (null != radioStatus) {
					status = BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN.equalsIgnoreCase(radioStatus);
					if (!status) {
						errorMessage = "Verification of 5GHz Private SSID status failed. EXPECTED ssid status is : "
								+ BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN + ", but we retrieved "
								+ radioStatus + " value from WebPa command.";
					}
				} else {
					errorMessage = "Unable to fetch the 5GHz Private SSID status using Webpa parameter "
							+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
				}
				if (status) {
					LOGGER.info("STEP 5: ACTUAL : Private 5 GHz SSID is in disable status");
				} else {
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

				/** Wait for 4 mins for the changes to get applied */
				LOGGER.info("Waiting for 4 minutes");
				tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);

				/**
				 * Step 6
				 */
				radioStepNumber = "s6";
				isVisible = false;
				errorMessage = "Client device(s) is listed the Private 5 Ghz SSID Name in available network list";
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 6:DESCRIPTION:Scan the broadcasting WI-Fi network from client device and check the connectivity status");
				LOGGER.info("STEP 6: ACTION: verify whether the 5Ghz SSID is not broadcasted");
				LOGGER.info(
						"STEP 6:EXPECTED: Client device(s) should not list the Private 5 Ghz SSID Name in available network list.");
				LOGGER.info("**********************************************************************************");
				isVisible = false;
				// verify whether 5ghz ssid is visible in client devices
				for (Dut connectedDevice : connectedDevices) {
					connectionType = ((Device) connectedDevice).getConnectedDeviceInfo().getConnectionType();
					if (CommonMethods.isNotNull(connectionType) && connectionType.trim().equalsIgnoreCase(
							BroadBandConnectedClientTestConstants.STRING_CLIENT_DEVICE_CONNECTION_TYPE_WIFI)) {
						isVisible = BroadBandConnectedClientUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDevice(
								connectedDevice, BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(
										device, tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ),
								tapEnv);
					}
					if (isVisible) {
						errorMessage = "5GHz SSID is listed in client device  model " + connectedDevice.getModel();
						LOGGER.info(errorMessage);
						break;
					}
				}
				status = !isVisible;
				if (status) {
					LOGGER.info("STEP 6: ACTUAL : 5 GHz SSID is not listed in client device  model");
				} else {
					LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);
				/**
				 * Step 7-8
				 */
				wifiDmcliParseCheck(device, tapEnv, testId, deviceDateTime, BroadBandTestConstants.CONSTANT_6);
				/**
				 * Step 9-13
				 */
				enableRadioAndCheckConnectivity(device, connectedDeviceActivated, testId,
						new String[] { "9", "10", "11", "12", "13" }, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				/**
				 * Step 14-18
				 */
				enableRadioAndCheckConnectivity(device, connectedDeviceActivated, testId,
						new String[] { "14", "15", "16", "17", "18" }, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				/**
				 * Step 19-20
				 */
				wifiDmcliParseCheck(device, tapEnv, testId, deviceDateTime, BroadBandTestConstants.CONSTANT_18);
			} else {
				status = false;
				errorMessage = "Connected device list is empty";
				LOGGER.info(errorMessage);
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);
			}
		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, radioStepNumber, status, errorMessage,
					true);
		} finally {
			BroadBandConnectedClientUtils.resetAllRadios(device, tapEnv);
		}

		LOGGER.info("ENDING TESTCASE :testToVerifyIPAddressWith2GHZDisabled() ");
	}

	/**
	 * Method to check the dmcli parse fix of RDKB-28734
	 * 
	 * @param device     Dut instance
	 * @param tapEnv     instance of {@link AutomaticsTapApi}
	 * @param testCaseId test case id
	 * @param step       current stepNumber
	 * @return int returns current step number
	 * @Refactor Sruthi Santhosh
	 */
	public int wifiDmcliParseCheck(Dut device, AutomaticsTapApi tapEnv, String testCaseId, String deviceDateTime,
			int step) {
		// String to store the test case status
		boolean status = false;
		// Test step number
		// String to store the error message
		String errorMessage = null;
		// response obtained
		String response = null;

		try {
			step++;
			String radioStepNumber = "s" + step;
			status = false;
			errorMessage = "\"Failed to get parameter value of\" is present in wifilog.txt";
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + step
					+ ": DESCRIPTION:Verify in WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\" ");
			LOGGER.info("STEP " + step
					+ " : ACTION: validate in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
			LOGGER.info("STEP " + step
					+ ": EXPECTED: \"Failed to get parameter value of\" is not present in /rdklogs/logs/WiFilog,txt.0");
			LOGGER.info("******************************************************");
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTestConstants.CONSTANT_WIFI_ERROR_MESSAGE,
					BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0);
			if (CommonMethods.isNull(response)) {
				status = true;
			} else {
				status = !BroadBandCommonUtils.verifyLogUsingTimeStamp(deviceDateTime, response);
			}
			if (status) {
				LOGGER.info("STEP " + step + ": ACTUAL :  WiFilog.txt.0 doesn't contains any failure logs");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, radioStepNumber, status, errorMessage, true);

			step++;
			radioStepNumber = "s" + step;
			status = false;
			errorMessage = "\"Failed to get parameter value of\" is present in wifilog.txt";
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + step
					+ ": DESCRIPTION:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
			LOGGER.info("STEP " + step + " : ACTION: Execute following command  dmcli eRT getv Device.WiFi.");
			LOGGER.info("STEP " + step + ": Output should contain more than 23 param");
			LOGGER.info("******************************************************");
			int passCount = 0;
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_PARAMS);
			try {
				JSONArray responseInJson = new JSONArray(response);

				LOGGER.info("JSONResponse is " + responseInJson.length());

				if (DeviceModeHandler.isRPIDevice(device)) {
					if (!(responseInJson.length() == 0)) {
						passCount = responseInJson.length();
						LOGGER.info("passcount in RPi : " + passCount);
					}
				} else {
					int counter = 0;
					for (counter = 0; counter < responseInJson.length(); counter++) {
						JSONObject json = responseInJson.getJSONObject(counter);
						String name = json.getString(BroadBandTestConstants.STRING_NAME);
						if (CommonMethods.isNotNull(name)) {
							passCount++;
						}
					}
				}
			} catch (Exception e) {
				LOGGER.error(e.getMessage());
			}
			status = passCount != 0 && passCount > BroadBandTestConstants.CONSTANT_23;
			if (status) {
				LOGGER.info("STEP " + step + ": ACTUAL :  Output has more than 23 param");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, radioStepNumber, status, errorMessage, true);
		} catch (Exception e) {
			LOGGER.error("Failed to check radio status" + e.getMessage());
			throw new TestException("Failed to check radio status" + errorMessage);
		}
		return step;
	}

	/**
	 * Steps to enable radio and check the IP connectivity
	 * 
	 * @param device
	 * @param connectedDeviceActivated
	 * @param testId
	 * @param stepNumbers
	 * @param radio
	 * @author Anandam s
	 * @Refactor Sruthi Santhosh
	 */
	private void enableRadioAndCheckConnectivity(Dut device, Dut connectedDeviceActivated, String testId,
			String[] stepNumbers, WiFiFrequencyBand radio) {
		// String to store the test case status
		boolean status = false;
		// Test step number
		String radioStepNumber = "s" + stepNumbers[0];
		// String to store the error message
		String errorMessage = null;
		String radioTobeUsed;
		if (WiFiFrequencyBand.WIFI_BAND_2_GHZ.equals(radio)) {
			radioTobeUsed = "2.4Ghz";
		} else {
			radioTobeUsed = "5Ghz";
		}
		if (stepNumbers.length == 5) {

			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + stepNumbers[0] + ":Enable Private " + radioTobeUsed + " SSID via WebPA");
			LOGGER.info("STEP " + stepNumbers[0] + ":EXPECTED : Device should enable the Private " + radioTobeUsed
					+ " GHZ SSID and WebPA command return success response.");
			LOGGER.info("******************************************************");
			radioStepNumber = "s" + stepNumbers[0];
			status = false;
			errorMessage = "Private " + radioTobeUsed + " SSID is not enabled ";
			// enable given radio
			status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(radio, tapEnv,
					device, true);
			if (status) {
				LOGGER.info("STEP " + stepNumbers[0] + ":  ACTUAL RESULT : Enabled the Private" + radioTobeUsed
						+ " GHZ SSID and WebPA command return success response.");
			} else {
				LOGGER.info("STEP " + stepNumbers[0] + ":  ACTUAL RESULT :" + errorMessage);
			}

			LOGGER.info("******************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

			LOGGER.info("Waiting for 1.5 minute");
			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + stepNumbers[1] + ":Verify the Private " + radioTobeUsed
					+ "  SSID enabled status via WebPA ");
			LOGGER.info("STEP " + stepNumbers[1] + "EXPECTED: Device should return the  Private " + radioTobeUsed
					+ "  SSID enabled status as \"Up\"");
			LOGGER.info("******************************************************");
			radioStepNumber = "s" + stepNumbers[1];
			status = false;
			errorMessage = "Private " + radioTobeUsed + " GHZ SSID is not enabled ";
			// verify radio status
			String radioStatus = BroadBandConnectedClientUtils.getRadioStatus(radio, tapEnv, device);
			if (null != radioStatus) {
				status = BroadBandConnectedClientTestConstants.RADIO_STATUS_UP.equalsIgnoreCase(radioStatus);
				if (!status) {
					errorMessage = "Verification of " + radioTobeUsed
							+ " Private SSID status failed. EXPECTED ssid status is : "
							+ BroadBandConnectedClientTestConstants.RADIO_STATUS_UP + ", but we retrieved "
							+ radioStatus + " value from WebPa command.";
				}
			} else {
				String param = radioTobeUsed.contains("2.4")
						? BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID
						: BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID;
				errorMessage = "Unable to fetch the " + radioTobeUsed + " Private SSID status using Webpa parameter "
						+ param;
			}
			if (status) {
				LOGGER.info("STEP " + stepNumbers[1] + ":  ACTUAL RESULT :Verification of " + radioTobeUsed
						+ " Private SSID status is successful. SSID enabled status is Up.");
			} else {
				LOGGER.info("STEP " + stepNumbers[1] + ":  ACTUAL RESULT :" + errorMessage);
			}
			LOGGER.info("******************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

			LOGGER.info("******************************************************");
			LOGGER.info("STEP" + stepNumbers[2] + ": Connect the device to " + radioTobeUsed
					+ " SSID and verify connection status");
			LOGGER.info("STEP " + stepNumbers[2] + "EXPECTED: Device should be connected with " + radioTobeUsed
					+ " wifi network");
			LOGGER.info("******************************************************");
			radioStepNumber = "s" + stepNumbers[2];
			status = false;
			errorMessage = "Connection to " + radioTobeUsed + " device failed";

			connectedDeviceActivated = radio.equals(WiFiFrequencyBand.WIFI_BAND_2_GHZ)
					? BroadBandConnectedClientUtils.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device,
							tapEnv)
					: BroadBandConnectedClientUtils.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device,
							tapEnv);
			if (null != connectedDeviceActivated) {
				status = true;
			} else {
				errorMessage = "Unable to connect to " + radioTobeUsed + " private SSID when " + radioTobeUsed
						+ " is enabled";
			}

			if (status) {
				LOGGER.info("STEP " + stepNumbers[2] + ":  ACTUAL RESULT :Able to connect to " + radioTobeUsed
						+ " private SSID when " + radioTobeUsed + " is enabled");
			} else {
				LOGGER.info("STEP " + stepNumbers[2] + ":  ACTUAL RESULT :" + errorMessage);
			}
			LOGGER.info("******************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);

			checkIpAddressObtained(device, tapEnv, connectedDeviceActivated, testId,
					new String[] { stepNumbers[3], stepNumbers[4] });
		} else {
			LOGGER.info("This function is meant for executing 5 steps.Current steps passed are " + stepNumbers.length);
		}
	}

	/**
	 * Common steps for checking IP address obtained for the connected client device
	 * 
	 * @param device
	 * @param connectedDeviceActivated
	 * @param testId
	 * @param stepNumbers
	 * @Refactor Sruthi Santhosh
	 */
	public static void checkIpAddressObtained(Dut device, AutomaticsTapApi tapEnv, Dut connectedDeviceActivated,
			String testId, String[] stepNumberss) {
		// String to store the test case status
		boolean status = false;
		// Test step number
		String radioStepNumber = "s" + stepNumberss[0];
		long polling_window_ms = 90000L;
		// String to store the error message
		String errorMessage = null;
		if (stepNumberss.length == 2) {

			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + stepNumberss[0] + ":Verify whether interface got the correct IPv4  address.");
			LOGGER.info("STEP " + stepNumberss[0] + "EXPECTED: Interface IP address should be shown");
			LOGGER.info("******************************************************");

			errorMessage = "Interface did not get the correct IPV4 address";
			String osType = ((Device) connectedDeviceActivated).getOsType();
			long startTime = System.currentTimeMillis();
			radioStepNumber = "s" + stepNumberss[0];
			// Loop for this function is a waiting time of max 90sec for the
			// webpa changes to get applied
			do {
				status = BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
						osType, connectedDeviceActivated, tapEnv);
				if (status) {
					break;
				}
			} while (System.currentTimeMillis() < (startTime + polling_window_ms));

			if (status) {
				LOGGER.info("STEP " + stepNumberss[0] + ":  ACTUAL RESULT : Interface got the correct IPv4 address.");
			} else {
				LOGGER.info("STEP " + stepNumberss[0] + ":  ACTUAL RESULT :" + errorMessage);
			}

			LOGGER.info("******************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, false);

			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + stepNumberss[1] + ":Verify whether interface got the correct IPv6  address.");
			LOGGER.info("EXPECTED " + stepNumberss[1] + ":Interface IP address should be shown");
			LOGGER.info("******************************************************");

			radioStepNumber = "s" + stepNumberss[1];
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				status = false;
				status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
						osType, connectedDeviceActivated, tapEnv);

				if (status) {
					LOGGER.info(
							"STEP " + stepNumberss[1] + ":  ACTUAL RESULT : Interface got the correct IPv6 address.");
				} else {
					LOGGER.info("STEP " + stepNumberss[1] + ":  ACTUAL RESULT :" + errorMessage);
				}

				LOGGER.info("******************************************************");
				tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not Available/disabled : skipping teststep ...");
				tapEnv.updateExecutionForAllStatus(device, testId, radioStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
		} else {
			LOGGER.info("This function is meant for executing 2 steps.Current steps passed are " + stepNumberss.length);
		}
	}

	/**
	 * This test case is to Verify whether WIFI supports WPA2-PSK-AES InValid-Key
	 * and valid-Key for 2.4Ghz
	 * <ol>
	 * <li>STEP 1:Execute WebPA command to Set on the security modes for 2.4ghz
	 * as-'WPA2-Personal' and validate whether they are set correctly.</li>
	 * 
	 * <li>STEP 2:Connect the connected client device to 2.4GHz SSID and verify
	 * connection status by giving valid password</li>
	 *
	 * <li>STEP 3: Verify whether interface got the correct IPv4 address.</li>
	 * 
	 * <li>STEP 4 :Verify whether interface got the correct IPv6 address.</li>
	 * 
	 * <li>STEP 5 :Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 *
	 * <li>STEP 6 :Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * 
	 * <li>STEP 7 :Connect the connected client device to 2.4GHz SSID and verify
	 * connection status by giving Invalid password</li>
	 * 
	 * </ol>
	 * 
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor Said Hisham
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WIFI-SEC-MODE-1001")
	public void testVerifyWifiSecurityModeWithValidAndInvalidKeyFor2_4Ghz(Dut device) {
		boolean status = false;
		String testId = "TC-RDKB-WIFI-SEC-MODE-101";
		int stepNumber = 1;
		String testStepNumber = "S" + stepNumber;
		String errorMessage = null;
		Dut connectedDeviceActivated = null;
		String ssidName = null;
		String command = null;
		String response = null;
		try {
			LOGGER.info("###################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-SEC-MODE-1001");
			LOGGER.info(
					"TEST DESCRIPTION: Verification of 2.4 GHz Private SSID connectivity using valid and invalid WPA2-PSK-AES key");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("Pre condition 1 : Verify the private wifi 2.4 GHz ssid is enabled");
			LOGGER.info(
					"Step : 1 Set on the security modes for 2.4ghz as-\"WPA2-Personal\" and validate whether they are set correctly.");
			LOGGER.info(
					"Step : 2 Connect the connected client device to 2.4GHz SSID and verify connection status by giving valid password");
			LOGGER.info("Step : 3 Verify whether interface got the correct IPv4 address.");
			LOGGER.info("Step : 4 Verify whether interface got the correct IPv6 address.");
			LOGGER.info("Step : 5 Verify whether you have connectivity using that particular interface using IPV4");
			LOGGER.info("Step : 6 Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info(
					"Step : 7 Connect the connected client device to 2.4GHz SSID and verify connection status by giving Invalid password");
			LOGGER.info("###################################################################################");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			errorMessage = null;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION 1 : DESCRIPTION : VERIFY WHETHER PRIVATE 2.4 GHZ SSID 'DEVICE.WIFI.SSID.10001.ENABLE' IS ENABLED, IF NOT ENABLE THE PRIVATE 2.4 GHZ SSID ");
			LOGGER.info(
					"PRE-CONDITION 1 : ACTION : VERIFY WHETHER PRIVATE 2.4 GHZ SSID 'DEVICE.WIFI.SSID.10001.ENABLE' IS ENABLED,IF NOT ENABLE THE PRIVATE 2.4 GHZ SSID USING WEBPA ");
			LOGGER.info(
					"PRE-CONDITION 1 : EXPTECTED : DEVICE SHOULD BE ENABLED WITH PRIVATE 2.4 GHZ SSID AND RESPONSE SHOULD BE TRUE");
			LOGGER.info("#######################################################################################");
			errorMessage = "NOT ABLE TO ENABLE THE 2.4GHZ PRIVATE SSID ON THIS DEVICE";
			try {
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED,
						BroadBandTestConstants.TRUE);
			} catch (TestException exception) {
				status = false;
				LOGGER.error(errorMessage + " : " + exception.getMessage());
			}
			if (!status) {
				errorMessage = "UNABLE TO SET THE PRIVATE 2.4 GHZ SSID STATUS AS 'TRUE'.";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : PRIVATE 2.4 GHZ SSID VERIFIED/ENABLED IN GATEWAY DEVICE.");
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1:Execute WebPA command to Set on the security modes for 2.4ghz
			 * as-"WPA2-Personal" and validate whether they are set correctly.
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Set on the  security modes for 2.4 GHz as-'WPA2-Personal'  and validate whether they are set correctly.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Set the Parameter Device.WiFi.AccessPoint.10001.Security.ModeEnabled with value \"WPA2-Personal\" using WebPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should be able to set the parameter by WebPA");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to Set on the Wi-Fi  security modes for 2.4 GHz as-'WPA2-Personal via via WebPA Command 'Device.WiFi.AccessPoint.10001.Security.ModeEnabled'";
			status = (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
					WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL));
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully Set on the  security modes for 2.4 GHz as-'WPA2-Personal");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2:Connect the connected client device to 2.4GHz SSID and verify
			 * connection status by giving valid password
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Connect the connected client device  to 2.4 GHz SSID and verify connection status by giving valid password");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION :  Connect to  2.4GHZ wifi using following  commands Linux :nmcli dev wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Device should be connected with 2.4 GHz wifi network by giving valid password");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to connect to 2.4 GHz Private Wi-Fi network";
			connectedDeviceActivated = BroadBandConnectedClientUtils
					.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			status = (null != connectedDeviceActivated);
			if (status) {
				tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfylly connected the client with 2.4GHz private ssid.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3: Verify whether the interface get the correct IPv4 address
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify whether the interface get the correct IPv4 address.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Get the device IPv4 address using below command Linux : ifconfig wlan0 |grep -i \"inet addr:\" Windows: ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"Pv4 Address\"");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Interface IPv4 address should be shown");
			LOGGER.info("***************************************************************************************");
			errorMessage = "Interface  didnt get the correct IPV4 address";
			status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
					connectedDeviceActivated);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : Interface  got the correct IPv4 address");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 4:Verify whether interface get the correct IPv6 address.
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				LOGGER.info("***************************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify whether interface  get the correct IPv6  address.");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Get the device IPv6 address using below command Linux : ifconfig wlan0 |grep -i \"inet6 addr:\" Windows:ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\" ");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : Interface IPv6 address should  be shown");
				LOGGER.info("***************************************************************************************");
				errorMessage = "interface  didnt got the correct IPV6 address";
				String osType = ((Device) connectedDeviceActivated).getOsType();
				status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
						osType, connectedDeviceActivated, tapEnv);
				if (status) {
					LOGGER.info("STEP " + stepNumber + " : ACTUAL : Interface  got the correct IPv6 address");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("***************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 4 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 5:Verify whether you have connectivity using that particular interface
			 * using IPV4.
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify whether there is  connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute command : Linux :  curl -4 -v --interface wlan0 'www.google.com' | grep '200 OK' Windows:curl -4 -v 'www.google.com' | grep '200 OK");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Connectivity check should return status as 200");
			LOGGER.info("***************************************************************************************");
			command = ((Device) connectedDeviceActivated).getOsType()
					.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
							? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
									.replace("<INTERFACE>", BroadbandPropertyFileHandler.getLinuxClientWifiInterface())
							: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
			errorMessage = "Connectivty check using IPV4 address failed";
			response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
			if (CommonMethods.isNotNull(response)) {
				status = response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK);
				if (!status) {
					errorMessage = "Expected 200 OK as response .But obtained " + response;
				}
			} else {
				errorMessage = "Unable to execute curl command for IPV4 on connected client device. Please check the connectivity between connected client and Jump server.";
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully Verified the internet connectivity using that particular interface using IPV4");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 6:Verify whether there is no connectivity using that particular
			 * interface using IPV6.
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;

			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {

				LOGGER.info("***************************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify whether there is  connectivity using that particular interface using IPV6 ");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute command : Linux :  curl -6 -v --interface wlan0 'www.google.com' | grep '200 OK' Windows:curl -6 -v 'www.google.com' | grep '200 OK");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : Connectivity check should return status as 200");
				LOGGER.info("***************************************************************************************");
				errorMessage = "Connectivty check using IPV6 address failed";
				command = ((Device) connectedDeviceActivated).getOsType()
						.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
								? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV6_ADDRESS
								: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV6_ADDRESS;
				response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
				if (CommonMethods.isNotNull(response)) {
					status = response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK);
					if (!status) {
						errorMessage = "Expected 200 OK as response .But obtained " + response;
					}
				} else {
					errorMessage = "Unable to execute curl command for IPv6 on connected client device. Please check the connectivity between connected client and Jump server.";
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Successfully Verified the internet connectivity using that particular interface using IPV6");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("***************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 6 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 7:Connect the connected client device to 2.4 GHz SSID and verify
			 * connection status by giving Invalid password
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Connect the connected client device  to 2.4 GHz SSID and verify connection status by giving Invalid password ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Connect to  2.4 GHZ wifi using below commands Linux :nmcli dev wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED :  Device should not be connected with 2.4 GHz wifi network by giving the invalid password");
			LOGGER.info("***************************************************************************************");
			errorMessage = "Connection to 2.4 Ghz  is successful even though by giving the invalid password";
			ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			if (CommonMethods.isNotNull(ssidName)) {
				status = !ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
						BroadbandPropertyFileHandler.getInvalidWifiPassword());
			} else {
				errorMessage = "SSID name obtained for 2.4ghz is null";
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Device is not  connected with 2.4 GHz wifi network by giving the invalid password");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		}
		LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-SEC-MODE-1001");
	}

	/**
	 * This test case is to Verify whether WIFI supports WPA2-PSK-AES InValid-Key
	 * and valid-Key for 5Ghz
	 * <ol>
	 * 
	 * <li>STEP 1:Execute WebPA command to Set on the security modes for 5ghz
	 * as-'WPA2-Personal' and validate whether they are set correctly.</li>
	 * 
	 * <li>STEP 2: Connect the connected client device to 5GHz SSID and verify
	 * connection status by giving valid password</li>
	 * 
	 * <li>STEP 3: Verify whether interface got the correct IPv4 address.</li>
	 * 
	 * <li>STEP 4 :Verify whether interface got the correct IPv6 address.</li>
	 * 
	 * <li>STEP 5 :Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 *
	 * <li>STEP 6 :Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * 
	 * <li>STEP 7 :Connect the connected client device to 5GHz SSID and verify
	 * connection status by giving Invalid password</li>
	 * 
	 * </ol>
	 * 
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor Said Hisham
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WIFI-SEC-MODE-1002")
	public void testVerifyWifiSecurityModeWithValidAndInvalidKeyFor5Ghz(Dut device) {
		boolean status = false;
		String testId = "TC-RDKB-WIFI-SEC-MODE-102";
		int stepNumber = 1;
		String testStepNumber = "S" + stepNumber;
		String errorMessage = null;
		Dut connectedDeviceActivated = null;
		String ssidName = null;
		String command = null;
		String response = null;
		try {
			LOGGER.info("###################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-SEC-MODE-1002");
			LOGGER.info(
					"TEST DESCRIPTION: Verify whether WIFI supports WPA2-PSK-AES InValid-Key and valid-Key for 5Ghz");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("Pre condition 1 : Verify the private wifi 5 GHz ssid is enabled");
			LOGGER.info(
					"Step : 1 Set on the security modes for 5 GHz as-\"WPA2-Personal\" and validate whether they are set correctly.");
			LOGGER.info(
					"Step : 2 Connect the connected client device to 5 GHz SSID and verify connection status by giving valid password");
			LOGGER.info("Step : 3 Verify whether interface got the correct IPv4 address.");
			LOGGER.info("Step : 4 Verify whether interface got the correct IPv6 address.");
			LOGGER.info("Step : 5 Verify whether you have connectivity using that particular interface using IPV4");
			LOGGER.info("Step : 6 Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info(
					"Step : 7 Connect the connected client device to 5 GHz SSID and verify connection status by giving Invalid password");
			LOGGER.info("###################################################################################");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION 1 : DESCRIPTION : SET AND VERIFY WHETHER PRIVATE 5 GHZ SSID 'DEVICE.WIFI.SSID.10101.ENABLE' IS ENABLED,IF NOT ENABLE THE PRIVATE 2.4 GHZ SSID ");
			LOGGER.info(
					"PRE-CONDITION 1 : ACTION : SET AND VERIFY WHETHER PRIVATE 5 GHZ SSID 'DEVICE.WIFI.SSID.10101.ENABLE' IS ENABLED,IF NOT ENABLE THE PRIVATE 2.4 GHZ SSID USING WEBPA ");
			LOGGER.info(
					"PRE-CONDITION 1 : EXPTECTED : DEVICE SHOULD BE ENABLED WITH PRIVATE 5 GHZ SSID AND RESPONSE SHOULD BE TRUE");
			LOGGER.info("#######################################################################################");
			errorMessage = "NOT ABLE TO ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE";
			try {
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED,
						BroadBandTestConstants.TRUE);
			} catch (TestException exception) {
				status = false;
				LOGGER.error(errorMessage + " : " + exception.getMessage());
			}
			if (!status) {
				errorMessage = "UNABLE TO SET THE PRIVATE 5 GHZ SSID STATUS AS 'TRUE'.";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : PRIVATE 5 GHZ SSID VERIFIED/ENABLED IN GATEWAY DEVICE.");
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1:Execute WebPA command to Set on the security modes for 5ghz
			 * as-"WPA2-Personal" and validate whether they are set correctly.
			 * 
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Set on the  security modes for 5 GHz as-'WPA2-Personal'  and validate whether they are set correctly.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Set the Parameter Device.WiFi.AccessPoint.10101.Security.ModeEnabled with value \"WPA2-Personal\" using WebPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should be able to set the parameter by WebPA");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to Set on the Wi-Fi  security modes for 5 GHz as-'WPA2-Personal' via WebPA Command 'Device.WiFi.AccessPoint.10101.Security.ModeEnabled'";
			status = (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
					WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL));
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully Set on the  security modes for 5 GHz as-'WPA2-Personal");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2: Connect the connected client device to 5GHz SSID and verify
			 * connection status by giving valid password
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Connect the connected client device  to 5 GHz SSID and verify connection status by giving valid password");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION :  Connect to  5 GHZ wifi using following  commands Linux :nmcli dev wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Device should be connected with 5 GHz wifi network by giving valid password");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to connect to 5 GHz Private Wi-Fi network.";
			connectedDeviceActivated = BroadBandConnectedClientUtils
					.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			status = (null != connectedDeviceActivated);
			if (status) {
				tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfylly connected the client with 5 GHz private ssid.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3: Verify whether the interface get the correct IPv4 address
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify whether the interface get the correct IPv4 address.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Get the device IPv4 address using below command Linux : ifconfig wlan0 |grep -i \"inet addr:\" Windows: ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"Pv4 Address\"");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Interface IPv4 address should be shown");
			LOGGER.info("***************************************************************************************");
			errorMessage = "Interface  didnt get the correct IPV4 address";
			status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
					connectedDeviceActivated);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : Interface  got the correct IPv4 address");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 4:Verify whether interface get the correct IPv6 address.
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				LOGGER.info("***************************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify whether interface  get the correct IPv6  address.");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Get the device IPv6 address using below command Linux : ifconfig wlan0 |grep -i \"inet6 addr:\" Windows:ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\" ");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : Interface IPv6 address should  be shown");
				LOGGER.info("***************************************************************************************");
				errorMessage = "interface  didnt got the correct IPV6 address";
				String osType = ((Device) connectedDeviceActivated).getOsType();
				status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
						osType, connectedDeviceActivated, tapEnv);
				if (status) {
					LOGGER.info("STEP " + stepNumber + " : ACTUAL : Interface  got the correct IPv6 address");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("***************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 4 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 5:Verify whether you have connectivity using that particular interface
			 * using IPV4.
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify whether there is  connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute command : Linux :  curl -4 -v --interface wlan0 'www.google.com' | grep '200 OK' Windows:curl -4 -v 'www.google.com' | grep '200 OK");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Connectivity check should return status as 200");
			LOGGER.info("***************************************************************************************");
			command = ((Device) connectedDeviceActivated).getOsType()
					.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
							? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
									.replace("<INTERFACE>", BroadbandPropertyFileHandler.getLinuxClientWifiInterface())
							: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
			errorMessage = "Connectivty check using IPV4 address failed";
			response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
			if (CommonMethods.isNotNull(response)) {
				status = response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK);
				if (!status) {
					errorMessage = "Expected 200 OK as response .But obtained " + response;
				}
			} else {
				errorMessage = "Unable to execute curl command for IPV4 on connected client device. Please check the connectivity between connected client and Jump server.";
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Successfully Verified the internet connectivity using that particular interface using IPV4");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 6:Verify whether there is no connectivity using that particular
			 * interface using IPV6.
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				LOGGER.info("***************************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify whether there is  connectivity using that particular interface using IPV6 ");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute command : Linux :  curl -6 -v --interface wlan0 'www.google.com' | grep '200 OK' Windows:curl -6 -v 'www.google.com' | grep '200 OK");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : Connectivity check should return status as 200");
				LOGGER.info("***************************************************************************************");
				errorMessage = "Connectivty check using IPV6 address failed";
				command = ((Device) connectedDeviceActivated).getOsType()
						.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
								? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV6_ADDRESS
								: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV6_ADDRESS;
				response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
				if (CommonMethods.isNotNull(response)) {
					status = response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK);
					if (!status) {
						errorMessage = "Expected 200 OK as response .But obtained " + response;
					}
				} else {
					errorMessage = "Unable to execute curl command for IPv6 on connected client device. Please check the connectivity between connected client and Jump server.";
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Successfully Verified the internet connectivity using that particular interface using IPV6");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("***************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 6 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 7:Connect the connected client device to 5 GHz SSID and verify
			 * connection status by giving Invalid password
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Connect the connected client device  to 5 GHz SSID and verify connection status by giving Invalid password ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Connect to  5 GHZ wifi using below commands Linux :nmcli dev wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED :  Device should not be connected with 5 GHz wifi network by giving the invalid password");
			LOGGER.info("***************************************************************************************");
			errorMessage = "Connection to 5 Ghz  is successful even though by giving invalid password";
			ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			if (CommonMethods.isNotNull(ssidName)) {
				status = !ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
						BroadbandPropertyFileHandler.getInvalidWifiPassword());
			} else {
				errorMessage = "SSID name obtained for 5Ghz is null";
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Device is not  connected with 5 GHz wifi network by giving the invalid password");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		}
		LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-SEC-MODE-1002");
	}

	/**
	 * Automate connecting client to 5 GHz SSID when 2.4 GHz SSID is disabled
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Pre-Condition : Disable and Verify 2.4GHz SSID<>
	 * <li>Step 1: Verify 5 GHz frequency enabled status</li>
	 * <li>Step 2: Connect & verify the client device into 5 GHz frequency SSID</li>
	 * <li>Step 3: Verify whether interface got the correct IPv4 address.</li>
	 * <li>Step 4: Verify whether interface got the correct IPv6 address.</li>
	 * <li>Step 5: Verify whether connectivity using that particular interface using
	 * IPv4 Address</li>
	 * <li>Step 6: Verify whether connectivity using that particular interface using
	 * IPv6 Address</li>
	 * </ol>
	 * 
	 * @author Susheela C
	 * @refactor Govardhan
	 * 
	 * @param device {@link Dut}
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WEBPA })
	@TestDetails(testUID = "TC-RDKB-WIFI-2050")
	public void testVerifyBroadBandWifiSsidStatusFor2GHz(Dut device) {
		String testCaseId = "TC-RDKB-WIFI-250";
		boolean status = false;
		String errorMessage = null;
		String step = "s1";
		try {
			/**
			 * Pre-condition : Disable and Verify 2.4GHz SSID
			 */
			if (!BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE,
					WebPaDataTypes.BOOLEAN.getValue(), RDKBTestConstants.FALSE, RDKBTestConstants.THREE_MINUTES,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {
				throw new TestException(
						"Pre-Condition error : Failed to disable 2.4GHz private SSID on the router device");
			}
			/**
			 * Step 1: Verify 5 GHz frequency enabled status via WebPA
			 */
			step = "s1";
			status = false;
			LOGGER.info("*****************************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify 5 GHz frequency enabled status via WebPA");
			LOGGER.info(
					"STEP 1: ACTION : Execute command  : curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA_URL><ECM_MAC>/config?names=Device.WiFi.SSID.2.Status");
			LOGGER.info("STEP 1: EXPECTED : Value must return true for 5 GHz");
			LOGGER.info("*****************************************************************************************");
			errorMessage = "Failed to validate/enable the SSID status for 5GHz Wifi network";
			/**
			 * Get the value of webpa param "Device.WiFi.SSID.10101.Enable", if true move to
			 * step 2 or enable the SSID
			 */
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					RDKBTestConstants.TRUE);
			if (!status) {
				// Enabling the 5GHz private SSID on the router device
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						WebPaDataTypes.BOOLEAN.getValue(), RDKBTestConstants.TRUE, RDKBTestConstants.THREE_MINUTES,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			}
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : The SSID enable status value is true for 5GHz WiFi network");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("Actual: "
					+ (status ? "The SSID enable status value is true for 5GHz WiFi network" : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
			/**
			 * Step 2: Connect & verify the client device into 5 GHz frequency SSID.
			 */
			step = "s2";
			status = false;
			LOGGER.info("*****************************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Connect & verify the client device into 5 GHz frequency SSID");
			LOGGER.info(
					"STEP 2: ACTION : Connected a 5GHz wifi client with the gateway device's 5GHz wifi network using SSID and PASSWORD");
			LOGGER.info("STEP 2: EXPECTED : Connected client must connect with 5 GHz frequency SSID");
			LOGGER.info("*****************************************************************************************");
			errorMessage = "Unable to Connect & verify the client device into 5 GHz SSID";
			// Connect Windows / Linuz client to 5 GHz wifi ssid
			Dut clientSettop = BroadBandConnectedClientUtils
					.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			status = null != clientSettop;
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully connected the client to 5GHz wifi network");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			LOGGER.info(
					"Actual: " + (status ? "Successfully connected the client to 5GHz wifi network" : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

			// Waiting for 2 min after connecting a client
			LOGGER.info("Waiting for two minutes after connecting a client...");
			tapEnv.waitTill(RDKBTestConstants.TWO_MINUTES);
			/**
			 * Steps 3 to 6
			 */
			BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, clientSettop, testCaseId,
					new String[] { "s3", "s4", "s5", "s6" });
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception occured while enabling/ disabling the SSID Advertisement value for 5 GHz network"
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
		} finally {
			// Enable the WebPA parameter
			// "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled"
			status = BroadBandWiFiUtils.setWebPaParams(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3);
		}
	}

	/**
	 * Validate that SSH, telnet to LAN getway IP from connected clients is not
	 * successful
	 * <ol>
	 * <li>Get a Ethernet client connected to the device</li>
	 * <li>Check if the ethernet connected client has an IP address from the
	 * gateway</li>
	 * <li>Check if the wireless client has internet access</li>
	 * <li>Validate if LAN gateway IP is reachable from client via Ping</li>
	 * <li>Validate if SSH to gateway IP is not successful from the client</li>
	 * <li>Validate if telnet to gateway IP is not successful from the client</li>
	 * <li>Get a Wi-Fi client connected to the device</li>
	 * <li>Customise the private WiFi SSID and password</li>
	 * <li>Connect clients to WIFI_BAND_2_GHZ GHz private SSIDs</li>
	 * <li>Check if the wireless connected client has an IP address from the
	 * gateway</li>
	 * <li>Check if the wireless client has internet access</li>
	 * <li>Validate if LAN gateway IP is reachable from client via Ping</li>
	 * <li>Validate if SSH to gateway IP is not successful from the client</li>
	 * <li>Validate if telnet to gateway IP is not successful from the client</li>
	 * <li>Get a Wi-Fi client connected to the device</li>
	 * <li>Customise the private WiFi SSID and password</li>
	 * <li>Connect clients to WIFI_BAND_5_GHZ GHz private SSIDs</li>
	 * <li>Check if the wireless connected client has an IP address from the
	 * gateway</li>
	 * <li>Check if the wireless client has internet access</li>
	 * <li>Validate if LAN gateway IP is reachable from client via Ping</li>
	 * <li>Validate if SSH to gateway IP is not successful from the client</li>
	 * <li>Validate if telnet to gateway IP is not successful from the client</li>
	 * </ol>
	 * 
	 * @param device
	 * @author SATHURYA RAVI
	 * @refactor Athira
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-CC-NEG-1001")

	public void validateNotificationForUnauthenticatedClients(Dut device) {

		// Variable Declaration begins
		String testCaseId = "";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		// Variable Declaration Ends

		testCaseId = "TC-RDKB-WIFI-CC-NEG-001";

		LOGGER.info("###################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CC-NEG-1001");
		LOGGER.info(
				"TEST DESCRIPTION: Validate that SSH, telnet to LAN getway IP from connected clients is not successful");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Get a Ethernet client connected to the device ");
		LOGGER.info("2. Check if the ethernet connected client has an IP address from the gateway  ");
		LOGGER.info("3. Check if the wireless client has internet access  ");
		LOGGER.info("4. Validate if LAN gateway IP is reachable from client via Ping  ");
		LOGGER.info("5. Validate if SSH to gateway IP is not successful from the client  ");
		LOGGER.info("6. Validate if telnet to gateway IP is not successful from the client  ");
		LOGGER.info("7. Get a Wi-Fi client connected to the device  ");
		LOGGER.info("8. Customise the private WiFi SSID and password  ");
		LOGGER.info("9. Connect clients to WIFI_BAND_2_GHZ GHz private SSIDs   ");
		LOGGER.info("10. Check if the wireless connected client has an IP address from the gateway   ");
		LOGGER.info("11. Check if the wireless client has internet access   ");
		LOGGER.info("12. Validate if LAN gateway IP is reachable from client via Ping  ");
		LOGGER.info("13. Validate if SSH to gateway IP is not successful from the client  ");
		LOGGER.info("14. Validate if telnet to gateway IP is not successful from the client   ");
		LOGGER.info("15. Get a Wi-Fi client connected to the device   ");
		LOGGER.info("16. Customise the private WiFi SSID and password   ");
		LOGGER.info("17. Connect clients to WIFI_BAND_5_GHZ GHz private SSIDs  ");
		LOGGER.info("18. Check if the wireless connected client has an IP address from the gateway  ");
		LOGGER.info("19. Check if the wireless client has internet access   ");
		LOGGER.info("20. Validate if LAN gateway IP is reachable from client via Ping   ");
		LOGGER.info("21. Validate if SSH to gateway IP is not successful from the client   ");
		LOGGER.info("22. Validate if telnet to gateway IP is not successful from the client  ");
		LOGGER.info("###################################################################################");

		try {
			/** executing steps 1 to 6 */
			executeTestStepsForNegativeScenario(device, tapEnv, BroadBandTestConstants.CONNECTION_TYPE_ETHERNET,
					BroadBandTestConstants.CONSTANT_1, testCaseId, ((Device) device).getConnectedDeviceList(), null);

			/** executing steps 7 to 14 */
			executeTestStepsForNegativeScenario(device, tapEnv,
					BroadBandConnectedClientTestConstants.STRING_CLIENT_DEVICE_CONNECTION_TYPE_WIFI,
					BroadBandTestConstants.CONSTANT_7, testCaseId, ((Device) device).getConnectedDeviceList(),
					WiFiFrequencyBand.WIFI_BAND_2_GHZ);

			/** executing steps 15 to 22 */
			executeTestStepsForNegativeScenario(device, tapEnv,
					BroadBandConnectedClientTestConstants.STRING_CLIENT_DEVICE_CONNECTION_TYPE_WIFI,
					BroadBandTestConstants.CONSTANT_15, testCaseId, ((Device) device).getConnectedDeviceList(),
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CC-NEG-1001");
		LOGGER.info("###################################################################################");
	}

	/**
	 * Method to execute common test steps for negative scenario
	 * 
	 * @param device
	 * @param tapEnv
	 * @param securityMode
	 * @param clientType
	 * @param stepNumber
	 * @param testCaseId
	 * @param connectedClientSettops
	 * @param band
	 * @refactor Athira
	 */

	private void executeTestStepsForNegativeScenario(Dut device, AutomaticsTapApi tapEnv, String clientType,
			int stepNumber, String testCaseId, List<Dut> connectedClientDevices, WiFiFrequencyBand band) {

		// variable declaration
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		Dut clientDevice = null;
		String gatewayIp = null;

		stepNum = "S" + stepNumber;
		errorMessage = "There is no " + clientType + " client associated to the gateway";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Get a " + clientType + " client connected to the device");
		LOGGER.info("STEP " + stepNumber + ": ACTION :  From the No.of connected clients from the account"
				+ " , get an ethernet client and check if it is connected prior to testing");
		LOGGER.info("STEP " + stepNumber
				+ ": EXPECTED : The setup should have an active ethernet client connecetd to the gateway");
		LOGGER.info("**********************************************************************************");

		clientDevice = BroadBandConnectedClientUtils.getConnectedClientFromConnectionType(device, tapEnv,
				connectedClientDevices, clientType);
		status = clientDevice != null;
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + clientType + " client is retrieved successfully ");
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		LOGGER.info("**********************************************************************************");

		if (clientType.equals(BroadBandConnectedClientTestConstants.STRING_CLIENT_DEVICE_CONNECTION_TYPE_WIFI)) {
			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "Attempt to customise private wifi SSIDs and passwords has failed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Customise the private WiFi SSID and password ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION :  Execute WebPa SET command to set customised SSID and password values");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : The parameter should get set successfully "
					+ "and return a 200 success response");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWiFiUtils.changePrivateWiFiSsidAndPassphraseFor24And5Ghz(device);

			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Attempt to customize private ssid and passphrase was successfuly. ");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			++stepNumber;
			stepNum = "S" + stepNumber;
			errorMessage = "Attempt to connect clients to " + band + " GHz private SSIDs has failed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Connect clients to " + band + " GHz private SSIDs");
			LOGGER.info("STEP " + stepNumber + ": ACTION :  Connect to " + band
					+ " GHz private wifi using below commands Linux :nmcli dev "
					+ "wifi connect <ssid> password <passwd> Windows : netsh wlan connect ssid=<ssid>"
					+ " name=<ssid name>");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : The clients should  get connected to the gateway");
			LOGGER.info("**********************************************************************************");

			status = BroadBandConnectedClientUtils
					.connectClientsToGivenTypeOfWifi(device, tapEnv, clientDevice, WEBPA_AP_INDEXES.PRIVATE_WIFI, band)
					.isStatus();

			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : The clients are connected successfully ");
				LOGGER.info("Waiting for two minutes...");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			LOGGER.info("**********************************************************************************");
		}

		++stepNumber;
		stepNum = "S" + stepNumber;
		errorMessage = "Attempt to retrieve IP address from client is not successful";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : Check if the wireless connected client has an IP address from the gateway");
		LOGGER.info(
				"STEP " + stepNumber + ": ACTION : IP Address should be retrieved from the Wireless Connected device");
		LOGGER.info("STEP " + stepNumber
				+ ": EXPECTED : IP Address should be retrieved from the Wireless Connected device");
		LOGGER.info("**********************************************************************************");

		String ipAddressRetrievedFromClient = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv, device,
				clientDevice);
		LOGGER.info("IP ADDRESS ASSIGNED TO THE CONNECTED CLIENT FROM DHCP : " + ipAddressRetrievedFromClient);
		errorMessage = "Unable to retrieve the IP Address form the cilent connected to the private Wi-Fi";
		if (CommonMethods.isNotNull(ipAddressRetrievedFromClient)) {
			status = CommonMethods.isIpv4Address(ipAddressRetrievedFromClient);
			errorMessage = "Cilent connected to the private Wi-Fi haven't received valid IP Address from Gateway";
		}
		if (status) {
			LOGGER.info("STEP " + stepNum
					+ ": ACTUAL : The IP address is successfully retrieved from the client. IpAdress: "
					+ ipAddressRetrievedFromClient);
		} else {
			LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		LOGGER.info("**********************************************************************************");

		++stepNumber;
		stepNum = "S" + stepNumber;
		errorMessage = "The client has no internet connectivity";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Check if the wireless client has internet access");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : Execute ping command to reach google using the command, \"ping www.google.com\"");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : The ping check should be successful ");
		LOGGER.info("**********************************************************************************");

		status = ConnectedNattedClientsUtils.verifyPingConnection(clientDevice, tapEnv,
				BroadBandTestConstants.PING_TO_GOOGLE);
		if (status) {
			LOGGER.info("STEP " + stepNum + ": ACTUAL :CLIENT HAS INTERNET CONNECTIVITY");
		} else {
			LOGGER.error("STEP " + stepNum + ": ACTUAL :" + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		LOGGER.info("**********************************************************************************");

		++stepNumber;
		stepNum = "S" + stepNumber;
		errorMessage = "Attempt to reach gateway IP from the client has failed";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP " + stepNumber + ": DESCRIPTION : Validate if LAN gateway IP is reachable from client via Ping");
		LOGGER.info(
				"STEP " + stepNumber + ": ACTION : Perform a ping to \"10.0.0.1\" from the command line of the client");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : The ping check should be successful ");
		LOGGER.info("**********************************************************************************");

		gatewayIp = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.GET_GATEWAY_IP_FROM_IFCONFIG);
		LOGGER.info("gatewayIp is " + gatewayIp);
		if (gatewayIp != null) {
			status = ConnectedNattedClientsUtils.verifyPingConnection(clientDevice, tapEnv, gatewayIp);
		}
		if (status) {
			LOGGER.info("STEP " + stepNum + ": ACTUAL :CLIENT HAS INTERNET CONNECTIVITY");
		} else {
			LOGGER.error("STEP " + stepNum + ": ACTUAL :" + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
		LOGGER.info("**********************************************************************************");

		++stepNumber;
		stepNum = "S" + stepNumber;
		status = false;
		String successMessage = null;
		String expectedOutput = null;
		successMessage = (DeviceModeHandler.isDSLDevice(device)
				&& !BroadBandCommonUtils.patternSearchFromTargetString(device.getFirmwareVersion(),
						BroadBandTestConstants.PROD_BUILD_SUBSTRING)) ? "SSH into gateway IP from client is successful"
								: "SSH into gateway IP from client is not successful";

		errorMessage = (DeviceModeHandler.isDSLDevice(device)
				&& !BroadBandCommonUtils.patternSearchFromTargetString(device.getFirmwareVersion(),
						BroadBandTestConstants.PROD_BUILD_SUBSTRING))
								? "Attempt to SSH into gateway IP from clients is not successful"
								: "Attempt to SSH into gateway IP from clients is successful";

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : Validate if SSH to gateway IP is not successful from the client");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : Try SSHing into the gateway IP, by executing the command \"ssh admin@10.0.0.1\"");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : The SSH should not be successful ");
		LOGGER.info("**********************************************************************************");
		expectedOutput = ((Device) clientDevice).isLinux()
				? sshTelnetCommandExpectedOutput.LINUX_COMMANDS.getExpectedSsh()
				: sshTelnetCommandExpectedOutput.WINDOWS_COMMANDS.getExpectedSsh();

		response = tapEnv.executeCommandOnOneIPClients(clientDevice,
				((Device) clientDevice).isLinux()
						? sshTelnetCommandExpectedOutput.LINUX_COMMANDS.getSshCommandWithIp(gatewayIp)
						: sshTelnetCommandExpectedOutput.WINDOWS_COMMANDS.getSshCommandWithIp(gatewayIp));
		if (DeviceModeHandler.isDSLDevice(device)
				&& !BroadBandCommonUtils.patternSearchFromTargetString(device.getFirmwareVersion(),
						BroadBandTestConstants.PROD_BUILD_SUBSTRING)) {

			status = CommonMethods.isNotNull(response)
					&& !BroadBandCommonUtils.patternSearchFromTargetString(response, expectedOutput);
		} else {
			status = CommonMethods.isNotNull(response)
					&& BroadBandCommonUtils.patternSearchFromTargetString(response, expectedOutput);
		}
		if (status) {
			LOGGER.info("STEP " + stepNum + ": ACTUAL :" + successMessage);
		} else {
			LOGGER.error("STEP " + stepNum + ": ACTUAL :" + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		LOGGER.info("**********************************************************************************");

		++stepNumber;
		stepNum = "S" + stepNumber;
		errorMessage = "Attempt to telnet into gateway IP from clients is successful";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : Validate if telnet to gateway IP is not successful from the client");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : Try telnet into the gateway IP, by executing the command \"telnet 10.0.0.1\"");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : The telnet should not be successful ");
		LOGGER.info("**********************************************************************************");

		response = tapEnv.executeCommandOnOneIPClients(clientDevice,
				((Device) clientDevice).isLinux()
						? sshTelnetCommandExpectedOutput.LINUX_COMMANDS.getTelnetCommandWithIp(gatewayIp)
						: sshTelnetCommandExpectedOutput.WINDOWS_COMMANDS.getTelnetCommandWithIp(gatewayIp));
		status = null != response && response.contains(
				((Device) clientDevice).isLinux() ? sshTelnetCommandExpectedOutput.LINUX_COMMANDS.getExpectedTelnet()
						: sshTelnetCommandExpectedOutput.WINDOWS_COMMANDS.getExpectedTelnet());

		if (status) {
			LOGGER.info("STEP " + stepNum + ": ACTUAL :Telnet into gateway IP from client is not successful ");
		} else {
			LOGGER.error("STEP " + stepNum + ": ACTUAL " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		LOGGER.info("**********************************************************************************");
	}

	/**
	 * This test case is to Verify the Reserved IP for clients for 2.4ghz
	 * 
	 * 
	 * <ol>
	 * <li>STEP 1:Getting the Wifi Mac address of Connected client having 2.4GHZ
	 * wifi Capability.</li>
	 * 
	 * <li>STEP 2: Getting the reserved Ip within the DHCP Range</li>
	 * 
	 * <li>STEP 3: Change DHCP lease time to 2 minutes</li>
	 * 
	 * <li>STEP 4: verify and add the the device with reserved Ip address for a Wifi
	 * Mac address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using
	 * WebPA POST command.</li>
	 * 
	 * <li>STEP 5 :Connect the connected client device whose wifi Mac address in the
	 * MAC table to 2.4GHz SSID and verify connection status connection status</li>
	 * 
	 * <li>STEP 6 :Verify whether the interface get the correct IPv4 address.</li>
	 * 
	 * <li>STEP 7 :Verify whether interface get the correct IPv6 address.</li>
	 * 
	 * <li>STEP 8:Verify whether you have connectivity using that particular
	 * interface using IPV4.</li>
	 * 
	 * <li>STEP 9:Verify whether you have connectivity using that particular
	 * interface using IPV6.</li>
	 * </ol>
	 * 
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor Athira
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WIFI-RESERVED-IP-1001")
	public void testToVerifyReservedIpFor2_4Ghz(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-RESERVED-IP-101";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;
		// string to store the ssid name
		String ssidName = null;
		// string to store the reserved ip address
		String reservedIp = null;
		// string to store the password
		String passPhraseName = null;
		// stores the table row number
		String tableRowNumber = null;
		// stores the WebpaServer Response
		WebPaServerResponse webPaServerResponse = null;
		// stores test result and error
		BroadBandResultObject result = null;
		// stores the mac address obtained from client
		String macAddressRetrievedFromClient = null;
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"PRE_CONDITION  : DESCRIPTION : Verify whether Private  2.4GHz SSID 'Device.WiFi.SSID.10001.Enable' is enabled using WebPA.");
			LOGGER.info("PRE-CONDITION  : ACTION : ENABLE THE 2.4GHZ PRIVATE SSID ON THIS DEVICE");
			LOGGER.info("PRE-CONDITION  : EXPECTED : 2.4GHZ private ssid should be enabled");
			LOGGER.info("#####################################################################################");

			String response1 = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS);

			status = CommonMethods.isNotNull(response1) && response1.equals(BroadBandTestConstants.TRUE);
			if (!status) {
				if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						WebPaDataTypes.BOOLEAN.getValue(), AutomaticsConstants.TRUE)) {
					throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR
							+ "NOT ABLE TO ENABLE THE 2.4GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
				}
			}
			LOGGER.info("PRE-CONDITION : ACTUAL: ENABLING  2.4GHZ SSID IS SUCCESSFUL");

			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-RESERVED-IP-1001");
			LOGGER.info("TEST DESCRIPTION: Verify the  Reserved IP for clients for 2.4ghz");

			/**
			 * STEP 1:Getting the Wifi Mac address of Connected client having 2.4GHZ wifi
			 * Capability.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 1:DESCRIPTION: Getting the Wifi Mac address of Connected client having 2.4GHZ wifi Capability.");
			LOGGER.info(
					"STEP 1:ACTION: Retrieve the Wifi Mac address of the connected client having 2.4GHZ wifi Capability");
			LOGGER.info(
					"STEP 1:EXPECTED: Device should be able to get the Wifi Mac address of the connected client having 2.4GHZ wifi Capability");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s1";
			status = false;
			errorMessage = "Unable to retrieve the connected client having 2.4GHZ wifi Capability";
			List<Dut> lockedDevices = ((Device) device).getConnectedDeviceList();
			connectedDeviceActivated = BroadBandConnectedClientUtils.getConnectedClientBasedOnTypeAndBand(device,
					tapEnv, lockedDevices, BroadBandTestConstants.WIFI, BroadBandTestConstants.BAND_2_4GHZ);
			LOGGER.info("STEP S1  : connectedDeviceActivated" + connectedDeviceActivated);
			if (connectedDeviceActivated != null) {
				errorMessage = "Unable to retreive the MAC address of the 2.4GHZ wifi Capable client";
				macAddressRetrievedFromClient = BroadBandWiFiUtils
						.retrieveMacAddressOfWifiConnectedClient(connectedDeviceActivated, device);
				LOGGER.info("STEP S1  : macAddressRetrievedFromClient" + macAddressRetrievedFromClient);
				status = CommonMethods.isNotNull(macAddressRetrievedFromClient);
			}
			LOGGER.info("S1 ACTUAL : " + (status
					? "Successfully retrieved the Wifi Mac address of the connected client having 2.4GHZ wifi Capability "
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2: Getting the reserved Ip within the DHCP Range.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 2:DESCRIPTION: Getting the reserved Ip within the DHCP Range");
			LOGGER.info("STEP 2:ACTION: Retrieve the Reserved Ip address within the DHCP Range");
			LOGGER.info("STEP 2:EXPECTED: Reserved Ip should be retrieved within the DHCP Range");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s2";
			status = false;
			errorMessage = "Unable to get the valid Ipv4 address from the client";
			LOGGER.info("STEP S2  : device" + device);
			LOGGER.info("STEP S2  : connectedDeviceActivated" + connectedDeviceActivated);
			String ipv4Address = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv, device,
					connectedDeviceActivated);
			LOGGER.info("STEP S2  : ipv4Address" + ipv4Address);
			if (CommonMethods.isIpv4Address(ipv4Address)) {
				errorMessage = "Unable to get the reserved Ip within the DHCP Range";
				reservedIp = BroadBandConnectedClientUtils.getReservedIpBetweenDhcpRangeFromRouter(tapEnv, device,
						ipv4Address);
				LOGGER.info("STEP S2  : reservedIp" + reservedIp);
				status = CommonMethods.isNotNull(reservedIp) && CommonMethods.isIpv4Address(reservedIp);
			}
			LOGGER.info("S2 ACTUAL : " + (status
					? "Successfully retrieved the reserved ip from the connected client having 2.4GHZ wifi Capability "
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3: Change DHCP lease time to 2 minutes
			 * 
			 */
			status = false;
			testStepNumber = "s3";
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 3 : DESCRIPTION :Change DHCP lease time to 2 minutes");
			LOGGER.info(
					"STEP 3 : ACTION:Execute Webpa command with Parameter :Device.DHCPv4.Server.Pool.1.LeaseTime Value:120 ");
			LOGGER.info("STEP 3 : EXPECTED:DHCP lease time values should be changes succssfully to 2 minutes");
			errorMessage = "Failed to set lease time to 120 seconds";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_LEASE_TIME_VALUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			LOGGER.info("Waiting for 2 minutes to reflect");
			tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 3:ACTUAL :DHCP Lease time changes to 2 minutes successfully ");
			} else {
				LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 4: verify and add the the device with reserved Ip address for a Wifi Mac
			 * address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using WebPA
			 * POST command.
			 * 
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 4:DESCRIPTION:Verify and add the the device with reserved Ip address for a wifi mac address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using WebPA POST command.");
			LOGGER.info(
					"STEP 4:ACTION:Device should be added with reserved Ip address for the device on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using WebPA POST command.");
			LOGGER.info("STEP 4:EXPECTED:Device should be added with reserved Ip address for the device");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s4";
			status = false;
			errorMessage = "Unable to post the add device with reserved Ip address for a wifi mac address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.'  using WebPA POST command.";
			webPaServerResponse = BroadBandConnectedClientUtils.setDeviceDetailsToDhcpServerPool(tapEnv, device,
					macAddressRetrievedFromClient, reservedIp);
			if (webPaServerResponse != null) {
				tableRowNumber = webPaServerResponse.getRow();
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			LOGGER.info("S4 ACTUAL:" + (status
					? "Successfully set the device name 'Device.DHCPv4.Server.Pool.1.StaticAddress.1.X_CISCO_COM_DeviceName' as 'Test' using WebPA command."
					: errorMessage));
			// *************************************************************************************
			ipv4Address = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv, device,
					connectedDeviceActivated);
			LOGGER.info("STEP S4  : ipv4Address" + ipv4Address);
			// *****************************************************************************************

			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 5: Connect the connected client device which is added in the MAC table
			 * to 2.4 GHz SSID and verify connection status
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 5:DESCRIPTION: Connect the connected client device whose wifi Mac address in the MAC table to 2.4 GHz SSID and verify connection status");
			LOGGER.info(
					"STEP 5:ACTION: Connect the connected client device whose wifi Mac address is added in the MAC table to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP 5:EXPECTED: Device should be connected with 2.4 GHz wifi network");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s5";
			status = false;
			LOGGER.info("Waiting for 3 minutes to reflect");
			tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
			errorMessage = "Connection to the connected client whose wifi Mac address in the MAC table having 2.4Ghz failed";
			LOGGER.info(
					"Wifi Mac Address of the Connected client whose wifi Mac address is  added in the MAC Filter is:-"
							+ ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress());
			ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			LOGGER.info("ssidName in STEP 5 is", ssidName);
			passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			LOGGER.info("passPhraseName in STEP 5 is", passPhraseName);
			status = ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
					passPhraseName);
			LOGGER.info("status in STEP 5 is", status);
			LOGGER.info("S5 ACTUAL : " + (status
					? "connected client device which is added in the MAC Filter is connected to 2.4GHZ wifi network since the MAC Filter mode is configured as 'Allow'"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 6: Verify whether the interface get the correct IPv4 address.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 6:DESCRIPTION:Verify whether the interface get the correct IPv4 address.");
			LOGGER.info("STEP 6: ACTION : Connected client should get the IPV4 Interface");
			LOGGER.info("STEP 6:EXPECTED:Interface IPv4 address should  be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s6";
			status = false;
			errorMessage = "ipv4 address obtained is not as same as the reserved Ip";
			String ipv4AddressFromClient = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv, device,
					connectedDeviceActivated);
			if (CommonMethods.isNotNull(ipv4AddressFromClient)) {
				status = ipv4AddressFromClient.equalsIgnoreCase(reservedIp);
			}

			if (status) {
				LOGGER.info(
						"STEP 6:ACTUAL :Interface  got the correct IPv4 address which is same as the reserved Ip address");
			} else {
				LOGGER.error("STEP 6:ACTUAL :" + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 7:Verify whether interface got the correct IPv6 address.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 7: DESCRIPTION : Verify whether interface  got the correct IPv6  address.");
			LOGGER.info("STEP 7: ACTION : Connected client should get the IPV6 Interface");
			LOGGER.info("STEP 7: EXPECTED:Interface IPv6 address should  be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s7";
			status = false;
			errorMessage = "interface  didnt got the correct IPV6 address";
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				String ipv6AddressRetrievedFromClient = BroadBandConnectedClientUtils
						.retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(connectedDeviceActivated, tapEnv);
				status = CommonMethods.isIpv6Address(ipv6AddressRetrievedFromClient);
				if (status) {
					LOGGER.info("S7 ACTUAL :Interface  got the correct IPv6 address");
				} else {
					LOGGER.error("S7 ACTUAL: " + errorMessage);
				}
				LOGGER.info("#####################################################################################");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 6 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 8:Verify whether you have connectivity using that particular interface
			 * using IPV4.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 8: Verify whether there is  connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP 8: ACTION : connectivity for Ipv4 interface should be successful");
			LOGGER.info("STEP 8: EXPECTED: Connectivity check should return status as 200");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s8";
			status = false;
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					connectedDeviceActivated, BroadBandTestConstants.URL_GOOGLE, BroadBandTestConstants.IP_VERSION4);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("S8 ACTUAL: connectivity successful using ipv4 interface");
			} else {
				LOGGER.error("S8 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 9:Verify whether there is connectivity using that particular interface
			 * using IPV6.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 9: Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info("STEP 9: ACTION : connectivity for Ipv6 interface should be successful");
			LOGGER.info("STEP 9:EXPECTED: Connectivity check should return status as 200");
			LOGGER.info("#####################################################################################");
			status = false;
			testStepNumber = "s9";
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
						connectedDeviceActivated, BroadBandTestConstants.URL_GOOGLE,
						BroadBandTestConstants.IP_VERSION6);
				status = result.isStatus();
				errorMessage = result.getErrorMessage();
				if (status) {
					LOGGER.info("S9 ACTUAL: connectivity successful using ipv6 interface");
				} else {
					LOGGER.error("S9 ACTUAL: " + errorMessage);
				}
				LOGGER.info("#####################################################################################");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 9 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#####################################################################################");

			LOGGER.info("#####################################################################################");
			LOGGER.info("POST CONDITION 1: Verify and  setting the default dhcp lease time value.");
			LOGGER.info("EXPECTED:Should be able to setting the default dhcp lease time value.");
			LOGGER.info("#####################################################################################");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_LEASE_TIME_DEFAULT_VALUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (!status) {
				LOGGER.error("NOT ABLE TO SET THE DHCP LEASE TIME TO DEFAULT VALUES USING  WEBPA COMMAND");
			} else {
				LOGGER.info(
						"POST-CONDITION 1 PASSED: ABLE TO SET THE DHCP LEASE TIME TO DEFAULT VALUES USING  WEBPA COMMAND");
			}

			LOGGER.info("#####################################################################################");
			LOGGER.info("POST CONDITION 2: Verify and  delete  device List table using WEBPA DELETE command.");
			LOGGER.info("EXPECTED:Should be able to delete the added device table list using webpa Delete command");
			LOGGER.info("#####################################################################################");
			webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRowNumber);
			if (webPaServerResponse != null) {
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			if (!status) {
				LOGGER.error("NOT ABLE TO DELETE THE DEVICE TABLE USING DELETE WEBPA COMMAND");
			} else {
				LOGGER.info("POST-CONDITION PASSED: ABLE TO DELETE THE DEVICE TABLE USING DELETE WEBPA COMMAND");
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TESTCASE TC-RDKB-WIFI-RESERVED-IP-1001");

	}

	/**
	 * This test case is to Verify the Reserved IP for clients for 5ghz
	 * 
	 * 
	 * <ol>
	 * <li>STEP 1:Getting the Wifi Mac address of Connected client having 5GHZ wifi
	 * Capability.</li>
	 * 
	 * <li>STEP 2: Getting the reserved Ip within the DHCP Range</li>
	 * 
	 * <li>STEP 3: Change DHCP lease time to 2 minutes</li>
	 * 
	 * <li>STEP 4: verify and add the the device with reserved Ip address for a Wifi
	 * Mac address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using
	 * WebPA POST command.</li>
	 * 
	 * <li>STEP 5 :Connect the connected client device whose wifi Mac address in the
	 * MAC table to 5GHz SSID and verify connection status</li>
	 * 
	 * <li>STEP 6 :Verify whether the interface get the correct IPv4 address.</li>
	 * 
	 * <li>STEP 7 :Verify whether interface get the correct IPv6 address.</li>
	 * 
	 * <li>STEP 8:Verify whether you have connectivity using that particular
	 * interface using IPV4.</li>
	 * 
	 * <li>STEP 9:Verify whether you have connectivity using that particular
	 * interface using IPV6.</li>
	 * </ol>
	 * 
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor Athira
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WIFI-RESERVED-IP-1002")
	public void testToVerifyReservedIpFor5Ghz(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-RESERVED-IP-102";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;
		// string to store the ssid name
		String ssidName = null;

		// string to store the reserved ip address
		String reservedIp = null;
		// string to store the password
		String passPhraseName = null;
		// stores the table row number
		String tableRowNumber = null;
		// stores the WebpaServer Response
		WebPaServerResponse webPaServerResponse = null;
		// stores test result and error
		BroadBandResultObject result = null;
		// stores the mac address obtained from client
		String macAddressRetrievedFromClient = null;
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"PRE_CONDITION: DESCRIPTION : Verify whether Private  5GHz SSID 'Device.WiFi.SSID.10101.Enable' is enabled using WebPA.");
			LOGGER.info("PRE-CONDITION : ACTION : ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE");
			LOGGER.info("PRE-CONDITION : EXPECTED : 5GHZ private ssid should be enabled");
			LOGGER.info("#####################################################################################");

			String response1 = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS);

			status = CommonMethods.isNotNull(response1) && response1.equals(BroadBandTestConstants.TRUE);
			if (!status) {
				if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						WebPaDataTypes.BOOLEAN.getValue(), AutomaticsConstants.TRUE)) {
					throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR
							+ "NOT ABLE TO ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
				}
			}
			LOGGER.info("PRE-CONDITION : ACTUAL: ENABLING  5GHZ SSID IS SUCCESSFUL");

			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-RESERVED-IP-1002");
			LOGGER.info("TEST DESCRIPTION: Verify the  Reserved IP for clients for 5Ghz");

			/**
			 * STEP 1:Getting the Wifi Mac address of Connected client having 5GHZ wifi
			 * Capability.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 1:DESCRIPTION: Getting the Wifi Mac address of Connected client having 5GHZ wifi Capability.");
			LOGGER.info(
					"STEP 1:ACTION: Retrieve the Wifi Mac address of the connected client having 5GHZ wifi Capability");
			LOGGER.info(
					"STEP 1:EXPECTED: Device should be able to get the Wifi Mac address of the connected client having 5GHZ wifi Capability");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s1";
			status = false;
			errorMessage = "Unable to retrieve the connected client having 5GHZ wifi Capability";
			List<Dut> lockedDevices = ((Device) device).getConnectedDeviceList();
			connectedDeviceActivated = BroadBandConnectedClientUtils.getConnectedClientBasedOnTypeAndBand(device,
					tapEnv, lockedDevices, BroadBandTestConstants.WIFI, BroadBandTestConstants.BAND_5GHZ);
			if (connectedDeviceActivated != null) {
				errorMessage = "Unable to retreive the MAC address of the 5GHZ wifi Capable client";
				macAddressRetrievedFromClient = BroadBandWiFiUtils
						.retrieveMacAddressOfWifiConnectedClient(connectedDeviceActivated, device);
				status = CommonMethods.isNotNull(macAddressRetrievedFromClient);
			}
			LOGGER.info("S1 ACTUAL : " + (status
					? "Successfully retrieved the Wifi Mac address of the connected client having 5GHZ wifi Capability "
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2: Getting the reserved Ip within the DHCP Range.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 2:DESCRIPTION: Getting the reserved Ip within the DHCP Range");
			LOGGER.info("STEP 2:ACTION: Retrieve the Reserved Ip address within the DHCP Range");
			LOGGER.info("STEP 2:EXPECTED: Reserved Ip should be retrieved within the DHCP Range");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s2";
			status = false;
			errorMessage = "Unable to get the valid Ipv4 address from the client";

			String ipv4Address = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv, device,
					connectedDeviceActivated);
			LOGGER.info("ipv4Address on BroadBandConnectedClientUtils.getIpv4AddressFromConnClient" + ipv4Address);

			if (CommonMethods.isIpv4Address(ipv4Address)) {
				errorMessage = "Unable to get the reserved Ip within the DHCP Range";
				reservedIp = BroadBandConnectedClientUtils.getReservedIpBetweenDhcpRangeFromRouter(tapEnv, device,
						ipv4Address);
				LOGGER.info("reservedIp on BroadBandConnectedClientUtils.getReservedIpBetweenDhcpRangeFromRouter"
						+ reservedIp);
				status = CommonMethods.isNotNull(reservedIp) && CommonMethods.isIpv4Address(reservedIp);
			}
			LOGGER.info("S2 ACTUAL : " + (status
					? "Successfully retrieved the reserved ip from the connected client having 2.4GHZ wifi Capability "
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3: Change DHCP lease time to 2 minutes
			 * 
			 */
			status = false;
			testStepNumber = "s3";
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 3 : DESCRIPTION :Change DHCP lease time to 2 minutes");
			LOGGER.info(
					"STEP 3 : ACTION:Execute Webpa command with Parameter :Device.DHCPv4.Server.Pool.1.LeaseTime Value:120 ");
			LOGGER.info("STEP 3 : EXPECTED:DHCP lease time values should be changes succssfully to 2 minutes");
			errorMessage = "Failed to set lease time to 120 seconds";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_LEASE_TIME_VALUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			LOGGER.info("Waiting for 2 minutes to reflect");
			tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 3:ACTUAL :DHCP Lease time changes to 2 minutes successfully ");
			} else {
				LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 4: verify and add the the device with reserved Ip address for a Wifi Mac
			 * address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using WebPA
			 * POST command.
			 * 
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 4:DESCRIPTION:Verify and add the the device with reserved Ip address for a wifi mac address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using WebPA POST command.");
			LOGGER.info(
					"STEP 4:ACTION:Device should be added with reserved Ip address for the device on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using WebPA POST command.");
			LOGGER.info("STEP 4:EXPECTED:Device should be added with reserved Ip address for the device");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s4";
			status = false;
			errorMessage = "Unable to post the add device with reserved Ip address for a wifi mac address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.'  using WebPA POST command.";
			webPaServerResponse = BroadBandConnectedClientUtils.setDeviceDetailsToDhcpServerPool(tapEnv, device,
					macAddressRetrievedFromClient, reservedIp);
			if (webPaServerResponse != null) {
				tableRowNumber = webPaServerResponse.getRow();
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			LOGGER.info("S4 ACTUAL:" + (status
					? "Successfully set the device name 'Device.DHCPv4.Server.Pool.1.StaticAddress.1.X_CISCO_COM_DeviceName' as 'Test' using WebPA command."
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 5: Connect the connected client device which is added in the Device
			 * table to 5GHz SSID and verify connection status
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 5:DESCRIPTION: Connect the connected client device whose wifi Mac address in the MAC table to 5GHz SSID and verify connection status");
			LOGGER.info(
					"STEP 5:ACTION: Connect the connected client device whose wifi Mac address is added in the MAC table to 5GHz SSID and verify connection status");
			LOGGER.info("STEP 5:EXPECTED: Device should be connected with 5 GHz wifi network");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s5";
			status = false;
			LOGGER.info("Waiting for 3 minutes to reflect");
			tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
			errorMessage = "Connection to the connected client whose wifi Mac address in the MAC table having 5Ghz failed";
			LOGGER.info(
					"Wifi Mac Address of the Connected client whose wifi Mac address is  added in the MAC Filter is:-"
							+ ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress());
			ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			status = ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
					passPhraseName);
			LOGGER.info("S5 ACTUAL : " + (status ? "Device should be connected with 5GHz wifi network" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 6: Verify whether the interface get the correct IPv4 address.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 6:DESCRIPTION:Verify whether the interface get the correct IPv4 address.");
			LOGGER.info("STEP 6: ACTION : Connected client should get the IPV4 Interface");
			LOGGER.info("STEP 6:EXPECTED:Interface IPv4 address should  be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s6";
			status = false;
			errorMessage = "ipv4 address obtained is not as same as the reserved Ip";
			String ipv4AddressFromClient = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv, device,
					connectedDeviceActivated);
			LOGGER.info("reservedIp on STEP 6 : " + reservedIp);
			LOGGER.info("ipv4AddressFromClient on STEP 6 : " + ipv4AddressFromClient);
			if (CommonMethods.isNotNull(ipv4AddressFromClient)) {
				status = ipv4AddressFromClient.equalsIgnoreCase(reservedIp);
			}

			if (status) {
				LOGGER.info(
						"STEP 6:ACTUAL :Interface  got the correct IPv4 address which is same as the reserved Ip address");
			} else {
				LOGGER.error("STEP 6:ACTUAL :" + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 7:Verify whether interface got the correct IPv6 address.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 7: DESCRIPTION : Verify whether interface  got the correct IPv6  address.");
			LOGGER.info("STEP 7: ACTION : Connected client should get the IPV6 Interface");
			LOGGER.info("STEP 7: EXPECTED:Interface IPv6 address should  be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s7";
			status = false;
			errorMessage = "interface  didnt got the correct IPV6 address";
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				String ipv6AddressRetrievedFromClient = BroadBandConnectedClientUtils
						.retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(connectedDeviceActivated, tapEnv);
				status = CommonMethods.isIpv6Address(ipv6AddressRetrievedFromClient);
				if (status) {
					LOGGER.info("S7 ACTUAL :Interface  got the correct IPv6 address");
				} else {
					LOGGER.error("S7 ACTUAL: " + errorMessage);
				}
				LOGGER.info("#####################################################################################");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 7 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 8:Verify whether you have connectivity using that particular interface
			 * using IPV4.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 8: Verify whether there is  connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP 8: ACTION : connectivity for Ipv4 interface should be successful");
			LOGGER.info("STEP 8: EXPECTED: Connectivity check should return status as 200");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s8";
			status = false;
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					connectedDeviceActivated, BroadBandTestConstants.URL_GOOGLE, BroadBandTestConstants.IP_VERSION4);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("S8 ACTUAL: connectivity successful using ipv4 interface");
			} else {
				LOGGER.error("S8 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 9:Verify whether there is connectivity using that particular interface
			 * using IPV6.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 9: Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info("STEP 9: ACTION : connectivity for Ipv6 interface should be successful");
			LOGGER.info("STEP 9:EXPECTED: Connectivity check should return status as 200");
			LOGGER.info("#####################################################################################");
			status = false;
			testStepNumber = "s9";
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
						connectedDeviceActivated, BroadBandTestConstants.URL_GOOGLE,
						BroadBandTestConstants.IP_VERSION6);
				status = result.isStatus();
				errorMessage = result.getErrorMessage();
				if (status) {
					LOGGER.info("S9 ACTUAL: connectivity successful using ipv6 interface");
				} else {
					LOGGER.error("S9 ACTUAL: " + errorMessage);
				}
				LOGGER.info("#####################################################################################");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 6 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#####################################################################################");

			LOGGER.info("#####################################################################################");
			LOGGER.info("POST CONDITION 1: Verify and  setting the default dhcp lease time value.");
			LOGGER.info("EXPECTED:Should be able to setting the default dhcp lease time value.");
			LOGGER.info("#####################################################################################");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_LEASE_TIME_DEFAULT_VALUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (!status) {
				LOGGER.error("NOT ABLE TO SET THE DHCP LEASE TIME TO DEFAULT VALUES USING  WEBPA COMMAND");
			} else {
				LOGGER.info(
						"POST-CONDITION 1 PASSED: ABLE TO SET THE DHCP LEASE TIME TO DEFAULT VALUES USING  WEBPA COMMAND");
			}

			LOGGER.info("#####################################################################################");
			LOGGER.info("POST CONDITION 2: Verify and  delete  device List table using WEBPA DELETE command.");
			LOGGER.info("EXPECTED:Should be able to delete the added device table list using webpa Delete command");
			LOGGER.info("#####################################################################################");
			webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRowNumber);
			if (webPaServerResponse != null) {
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			if (!status) {
				LOGGER.error("NOT ABLE TO DELETE THE DEVICE TABLE USING DELETE WEBPA COMMAND");
			} else {
				LOGGER.info("POST-CONDITION PASSED: ABLE TO DELETE THE DEVICE TABLE USING DELETE WEBPA COMMAND");
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TESTCASE TC-RDKB-WIFI-RESERVED-IP-1002");

	}

	/**
	 * This test case is to Verify the Reserved IP for clients for Ethernet client
	 * 
	 * 
	 * <ol>
	 * <li>Getting the Wifi Mac address of Connected client having Ethernet
	 * Capability</li>
	 * <li>Getting the DHCP min range and DHCP max Range Ip address of the
	 * device</li>
	 * <li>Verification of adding Host name , Wifi Mac Address,Reserved
	 * Ipaddress,Comments by using Webpa POST command on TR-181 parameter
	 * \"Device.DHCPv4.Server.Pool.1.StaticAddress\".</li>
	 * <li>Connect the connected client device whose Ethernet physical address is
	 * added in the MAC address list and verify connection status</li>
	 * <li>Verify whether interface got the correct IPv4 address as the reserved
	 * Ipv4 address</li>
	 * <li>Verify whether interface got the correct IPv6 address.</li>
	 * <li>Verify whether you have connectivity using that particular interface
	 * using IPV4</li>
	 * <li>Verify whether you have connectivity using that particular interface
	 * using IPV6</li>
	 * </ol>
	 *
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor Athira
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WIFI-RESERVED-IP-1003")
	public void testToVerifyReservedIpForEthernetClient(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-RESERVED-IP-103";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// string to store the reserved ip address
		String reservedIp = null;
		// stores the table row number
		String tableRowNumber = null;
		// stores the WebpaServer Response
		WebPaServerResponse webPaServerResponse = null;
		BroadBandResultObject result = null; // stores test result and error
		// stores the mac address obtained from client
		String macAddressRetrievedFromClient = null;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-RESERVED-IP-1003");
			LOGGER.info("TEST DESCRIPTION: Verify the  Reserved IP for clients for Ethernet client");

			LOGGER.info("TEST STEPS : ");
			LOGGER.info("1. Getting the Wifi Mac address of Connected client having  Ethernet Capability");
			LOGGER.info("2. Getting the DHCP min range  and DHCP max Range Ip address of the device ");
			LOGGER.info(
					"3. Verification of adding Host name , Wifi Mac Address,Reserved Ipaddress,Comments  by using Webpa POST command on TR-181 parameter \"Device.DHCPv4.Server.Pool.1.StaticAddress\".");
			LOGGER.info(
					"4. Connect the connected client device whose Ethernet physical  address is added in the MAC address list   and verify connection status");
			LOGGER.info("5. Verify whether interface got the correct IPv4  address as the reserved Ipv4 address");
			LOGGER.info("6. Verify whether interface got the correct  IPv6 address.");
			LOGGER.info("7. Verify whether you have connectivity using that particular interface using IPV4");
			LOGGER.info("8. Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1: Getting the physical Mac address of Connected client having Ethernet
			 * Capability
			 *
			 */
			testStepNumber = "s1";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Getting the physical Mac address of Connected client having  Ethernet Capability");
			LOGGER.info(
					"STEP 1: ACTION : Retrieve the  Mac address of the connected client having Ethernet Capability");
			LOGGER.info(
					"STEP 1: EXPECTED: Device should be able to get the  Mac address of the connected client having Ethernet Capability");
			LOGGER.info("#####################################################################################");
			Dut ethernetClient = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			errorMessage = "Unable to connect to Ethernet client";
			if (ethernetClient != null) {
				errorMessage = "Unable to retreive the MAC address of the ethernet client";
				macAddressRetrievedFromClient = BroadBandConnectedClientUtils
						.getConnectedClientIpOrMacFromTheDevice(device, ethernetClient, tapEnv, false);
				LOGGER.info("macAddressRetrievedFrom Ethernet Client is " + macAddressRetrievedFromClient);
				status = CommonMethods.isNotNull(macAddressRetrievedFromClient);
			}
			if (status) {
				LOGGER.info(
						"S1 ACTUAL: Device should be able to get the physical Mac address of the connected client having Ethernet Capability");
			} else {
				LOGGER.error("S1 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2: Getting the reserved Ip within the DHCP Range.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 2:DESCRIPTION: Getting the reserved Ip within the DHCP Range");
			LOGGER.info("STEP 2:ACTION: Retrieve the Reserved Ip address within the DHCP Range");
			LOGGER.info("STEP 2:EXPECTED: Reserved Ip should be retrieved within the DHCP Range");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s2";
			status = false;
			errorMessage = "Unable to get the valid Ipv4 address from ethernet client";
			String ipv4Address = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv, device,
					ethernetClient);
			if (CommonMethods.isIpv4Address(ipv4Address)) {
				errorMessage = "Unable to get the reserved Ip within the DHCP Range";
				reservedIp = BroadBandConnectedClientUtils.getReservedIpBetweenDhcpRangeFromRouter(tapEnv, device,
						ipv4Address);
				status = CommonMethods.isNotNull(reservedIp) && CommonMethods.isIpv4Address(reservedIp);
			}
			if (status) {
				LOGGER.info("S2 ACTUAL: Successfully retrieved  the Reserved Ip address within the DHCP Range");
			} else {
				LOGGER.error("S2 ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3: Change DHCP lease time to 2 minutes
			 * 
			 */
			status = false;
			testStepNumber = "s3";
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 3 : DESCRIPTION :Change DHCP lease time to 2 minutes");
			LOGGER.info(
					"STEP 3 : ACTION:Execute Webpa command with Parameter :Device.DHCPv4.Server.Pool.1.LeaseTime Value:120 ");
			LOGGER.info("STEP 3 : EXPECTED:DHCP lease time values should be changes succssfully to 2 minutes");
			errorMessage = "Failed to set lease time to 120 seconds";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_LEASE_TIME_VALUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			LOGGER.info("Waiting for 2 minutes to reflect");
			tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 3:ACTUAL :DHCP Lease time changes to 2 minutes successfully ");
			} else {
				LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 4: verify and add the the device with reserved Ip address for a Wifi Mac
			 * address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using WebPA
			 * POST command.
			 * 
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 4:DESCRIPTION:Verify and add the the device with reserved Ip address for a wifi mac address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using WebPA POST command.");
			LOGGER.info(
					"STEP 4:ACTION:Device should be added with reserved Ip address for the device on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using WebPA POST command.");
			LOGGER.info("STEP 4:EXPECTED:Device should be added with reserved Ip address for the device");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s4";
			status = false;
			errorMessage = "Unable to post the add device with reserved Ip address for a wifi mac address on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.'  using WebPA POST command.";
			webPaServerResponse = BroadBandConnectedClientUtils.setDeviceDetailsToDhcpServerPool(tapEnv, device,
					macAddressRetrievedFromClient, reservedIp);
			if (webPaServerResponse != null) {
				tableRowNumber = webPaServerResponse.getRow();
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			if (status) {
				LOGGER.info(
						"STEP 4:ACTUAL :Successfully added the reserved Ip address for the device on the table 'Device.DHCPv4.Server.Pool.1.StaticAddress.' using WebPA POST command.");
			} else {
				LOGGER.error("STEP 4:ACTUAL :" + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 5: Verify whether the interface get the correct IPv4 address.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 5:DESCRIPTION:Verify whether the interface get the correct IPv4 address.");
			LOGGER.info("STEP 5: ACTION : Connected client should get the IPV4 Interface");
			LOGGER.info("STEP 5:EXPECTED:Interface IPv4 address should  be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s5";
			status = false;
			LOGGER.info("Waiting for 5 minutes to reflect");
			tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			errorMessage = "ipv4 address obtained is not as same as the reserved Ip";
			String ipv4AddressFromEthernetClient = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
					device, ethernetClient);
			LOGGER.info("Reserved IP from client is: " + reservedIp);
			if (CommonMethods.isNotNull(ipv4AddressFromEthernetClient)) {
				status = ipv4AddressFromEthernetClient.equalsIgnoreCase(reservedIp);
			}

			if (status) {
				LOGGER.info(
						"STEP 5:ACTUAL :Interface  got the correct IPv4 address which is same as the reserved Ip address");
			} else {
				LOGGER.error("STEP 5:ACTUAL :" + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 6:Verify whether interface got the correct IPv6 address.
			 * 
			 */

			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 6: DESCRIPTION : Verify whether interface  got the correct IPv6  address.");
			LOGGER.info("STEP 6: ACTION : Connected client should get the IPV6 Interface");
			LOGGER.info("STEP 6: EXPECTED:Interface IPv6 address should  be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s6";
			status = false;
			errorMessage = "interface  didnt got the correct IPV6 address";
			String ipv6AddressRetrievedFromEthernetClient = BroadBandConnectedClientUtils
					.retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(ethernetClient, tapEnv);
			status = CommonMethods.isIpv6Address(ipv6AddressRetrievedFromEthernetClient);
			if (status) {
				LOGGER.info("S6 ACTUAL :Interface  got the correct IPv6 address");
			} else {
				LOGGER.error("S6 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 7:Verify whether you have connectivity using that particular interface
			 * using IPV4.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 7: Verify whether there is  connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP 7: ACTION : connectivity for Ipv4 interface should be successful");
			LOGGER.info("STEP 7: EXPECTED: Connectivity check should return status as 200");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s7";
			status = false;
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					ethernetClient, BroadBandTestConstants.URL_HTTPS_FACEBOOK, BroadBandTestConstants.IP_VERSION4);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("S7 ACTUAL: connectivity successful using ipv4 interface");
			} else {
				LOGGER.error("S7 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 8:Verify whether there is connectivity using that particular interface
			 * using IPV6.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 8: Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info("STEP 8: ACTION : connectivity for Ipv6 interface should be successful");
			LOGGER.info("STEP 8:EXPECTED: Connectivity check should return status as 200");
			LOGGER.info("#####################################################################################");
			status = false;
			testStepNumber = "s8";
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					ethernetClient, BroadBandTestConstants.URL_HTTPS_FACEBOOK, BroadBandTestConstants.IP_VERSION6);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("S8 ACTUAL: connectivity successful using ipv6 interface");
			} else {
				LOGGER.error("S8 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#####################################################################################");

			LOGGER.info("#####################################################################################");
			LOGGER.info("POST CONDITION 1: Verify and  setting the default dhcp lease time value.");
			LOGGER.info("EXPECTED:Should be able to setting the default dhcp lease time value.");
			LOGGER.info("#####################################################################################");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_LEASE_TIME_DEFAULT_VALUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (!status) {
				LOGGER.error("NOT ABLE TO SET THE DHCP LEASE TIME TO DEFAULT VALUES USING  WEBPA COMMAND");
			} else {
				LOGGER.info(
						"POST-CONDITION 1 PASSED: ABLE TO SET THE DHCP LEASE TIME TO DEFAULT VALUES USING  WEBPA COMMAND");
			}

			LOGGER.info("#####################################################################################");
			LOGGER.info("POST CONDITION 2: Verify and  delete  device List table using WEBPA DELETE command.");
			LOGGER.info("EXPECTED:Should be able to delete the added device table list using webpa Delete command");
			LOGGER.info("#####################################################################################");
			webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRowNumber);
			if (webPaServerResponse != null) {
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			if (!status) {
				LOGGER.error("NOT ABLE TO DELETE THE DEVICE TABLE USING DELETE WEBPA COMMAND");
			} else {
				LOGGER.info("POST-CONDITION PASSED: ABLE TO DELETE THE DEVICE TABLE USING DELETE WEBPA COMMAND");
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-RESERVED-IP-1003");

	}

	/**
	 * This test case is to Verify the controlled user access (Allow) for 5GHZ
	 * against different MAC addresses by way of MAC ACLs
	 * 
	 * <ol>
	 * <li>STEP 1:Getting the Wifi Mac address of Connected client having 5GHZ wifi
	 * Capability.</li>
	 * 
	 * <li>STEP 2: Verify and add the device wifi Mac address in the MAC Filter list
	 * by using WEBPA command.</li>
	 * 
	 * <li>STEP 3: Verify whether MAC Filtering mode is configured as 'Allow' by
	 * setting TR-181 parameter
	 * 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as 'true' using
	 * WebPA</li>
	 * 
	 * <li>STEP 4: Verify whether MAC Filtering mode is configured as 'Allow'by
	 * setting of TR-181 parameter
	 * 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList' as
	 * 'false' using WebPA for 5GHZ.</li>
	 * 
	 * <li>STEP 5:verify getting the Configured MAC Filter Mode by using this
	 * TR-181parameter
	 * 'Device.WiFi.AccessPoint.10101.X_COMCAST-COM_MAC_FilteringMode'</li>
	 *
	 * <li>STEP 6: Connect the connected client device which is added in the MAC
	 * Filter to 5 GHz SSID and verify connection status</li>
	 * 
	 * <li>STEP 7 :Verify whether interface got the correct IPv4 address</li>
	 * 
	 * <li>STEP 8 :Verify whether interface got the correct IPv6 address</li>
	 *
	 * <li>STEP 9 :Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * 
	 * <li>STEP 10 :Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * 
	 * <li>STEP 11: Connect the connected client device whose wifi Mac address is
	 * not added in the MAC Filter to 5GHz SSID and verify connection status</li>
	 * 
	 * <li>STEP 12: Verify whether interface did'nt get the correct IPv4
	 * address.</li>
	 *
	 * <li>STEP 13 : Verify whether interface did'nt get the correct IPv6
	 * address.</li>
	 * 
	 * <li>STEP 14: Verify whether there is no connectivity using that particular
	 * interface using IPV4</li>
	 * 
	 * <li>STEP 15: Verify whether there is no connectivity using that particular
	 * interface using IPV6</li>
	 * </ol>
	 * 
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor yamini.s
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			TestGroup.WEBPA, TestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-5014")
	public void testVerifyMacFilterAsAllow5Ghz(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-514";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// server response
		String response = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;
		// String to store the Wifi Macaddress for 5GHZ connected client
		String wifiMacAddress = null;
		// String to store the added table row number
		String tableRowNumber = null;
		// string to store the webpaserver response
		WebPaServerResponse webPaServerResponse = null;
		// string to store the ssid name
		String ssidName = null;
		// string to store the password
		String passPhraseName = null;
		// string to store the command
		String command = null;
		try {
			LOGGER.info("STARTING TESTCASE : TC-RDKB-WIFI-5014 ");
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"PRE_CONDITION 1: Verify whether Private  5GHz SSID 'Device.WiFi.SSID.10101.Enable' is enabled using WebPA.");
			LOGGER.info("#####################################################################################");
			if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					RDKBTestConstants.CONSTANT_3, RDKBTestConstants.TRUE)) {
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR
						+ "NOT ABLE TO ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
			}

			/**
			 * STEP 1:Getting the Wifi Mac address of Connected client having 5GHZ wifi
			 * Capability.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 1: Getting the Wifi Mac address of Connected client having 5GHZ wifi Capability.");
			LOGGER.info(
					"EXPECTED: Device should be able to get the Wifi Mac address of the connected client having 5GHZ wifi Capability");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s1";
			status = false;
			errorMessage = "Unable to retrieve the Wifi Mac address of the connected client having 5GHZ wifi Capability";
			List<Dut> lockedDevices = ((Device) device).getConnectedDeviceList();
			try {
				connectedDeviceActivated = BroadBandConnectedClientUtils.getConnectedClientBasedOnTypeAndBand(device,
						tapEnv, lockedDevices, BroadBandTestConstants.WIFI, BroadBandTestConstants.BAND_5GHZ);
				wifiMacAddress = ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress();
				status = CommonMethods.isNotNull(wifiMacAddress);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}
			LOGGER.info("S1 ACTUAL : " + (status
					? "Successfully retrieved the Wifi Mac address of the connected client having 5GHZ wifi Capability "
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2:Verify and add the device wifi Mac address in the MAC Filter list by
			 * using WEBPA command.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 2: Verify and add the device wifi Mac address in the MAC Filter list by using WEBPA command.");
			LOGGER.info("EXPECTED:Device should add the device wifi Mac address in the MAC Filter list");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s2";
			status = false;
			Map<String, List<String>> macFilterTable = new HashMap<String, List<String>>();
			List<String> MacAddressList = new ArrayList<String>();
			MacAddressList.add(wifiMacAddress);
			LOGGER.info("Wifi Mac Address of the Connected client obtained is:-" + wifiMacAddress);
			List<String> DeviceList = new ArrayList<String>();
			DeviceList.add(BroadBandTestConstants.CONNECTED_DEVICE);
			// adding to the Map.
			macFilterTable.put(BroadBandTestConstants.MAC_ADDRESS, MacAddressList);
			macFilterTable.put(BroadBandTestConstants.DEVICE_NAME, DeviceList);
			errorMessage = "Unable to add the wifi Mac Address to the  MAC Filter filter by Webpa POST command";
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_TABLE, macFilterTable);
			if (null != webPaServerResponse) {
				tableRowNumber = webPaServerResponse.getRow();
				status = CommonMethods.isNotNull(webPaServerResponse.getMessage())
						&& webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			LOGGER.info("S2 ACTUAL : " + (status
					? "Successfully added the wifi Mac Address to the  MAC Filter filter by Webpa POST command"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3:Verify whether MAC Filtering mode is configured as "Allow" by setting
			 * TR-181 parameter "Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable"
			 * as "true" using WebPA for 5GHZ.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 3: Verify whether MAC Filtering mode is configured as 'Allow' by setting TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as 'true' using WebPA");
			LOGGER.info("EXPECTED:Device should set MAC filtering mode as 'Allow' when we enable the MAC Filter.");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s3";
			status = false;
			errorMessage = "Unable to Set the MAC Filter as Allow by setting true to the  TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' by using Webpa commands.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_ENABLE,
					RDKBTestConstants.CONSTANT_3, RDKBTestConstants.TRUE);
			LOGGER.info("S3 ACTUAL : " + (status
					? "Successfully Set the MAC Filter as  Allow by setting the TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as true via webpa"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 4: Verify whether MAC Filtering mode is configured as 'Allow'by setting
			 * of TR-181 parameter
			 * 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList' as
			 * 'false' using WebPA for 5GHZ.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 4: Verify whether MAC Filtering mode is configured as 'Allow'by  setting of TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList' as 'false' using WebPA for 5GHZ.");
			LOGGER.info(
					"EXPECTED:Device should set 'false' to the TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList'");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s4";
			status = false;
			errorMessage = "Unable to Set the  TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList' as false  by using Webpa commands.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_BLACK_LIST,
					RDKBTestConstants.CONSTANT_3, RDKBTestConstants.FALSE);
			LOGGER.info("S4 ACTUAL : " + (status
					? "Successfully Set the TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList' as false via webpa"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 5:verify getting the Configured MAC Filter Mode by using this
			 * TR-181parameter
			 * "Device.WiFi.AccessPoint.10101.X_COMCAST-COM_MAC_FilteringMode For 5GHZ" .
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 5:verify getting the Configured MAC Filter Mode by using  this TR-181parameter 'Device.WiFi.AccessPoint.10101.X_COMCAST-COM_MAC_FilteringMode'");
			LOGGER.info("EXPECTED:The configured MAC Filter mode for the device should be as Allow ");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s5";
			status = false;
			if (DeviceModeHandler.isRPIDevice(device)) {
				errorMessage = "MAC Filter mode is not configured as Allow-ALL";
				response = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_MODE);
				status = response.equalsIgnoreCase(BroadBandTestConstants.MAC_FILTER_ALLOW_ALL);
			} else {
				errorMessage = "MAC Filter mode is not configured as Allow";
				response = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_MODE);
				status = response.equalsIgnoreCase(BroadBandTestConstants.MAC_FILTER_ALLOW);
			}
			LOGGER.info(
					"S5 ACTUAL : " + (status ? "MAC Filter mode for the device is configured as Allow" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 6:Connect the connected client device which added in the MAC Filter to
			 * 5GHz SSID and verify connection status
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 6: Connect the connected client device which added in the MAC Filter to 5GHz SSID and verify connection status");
			LOGGER.info(
					"EXPECTED: Device should be connected with 5GHz wifi network since the MAC Filter mode is configured as 'Allow'");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s6";
			status = false;
			errorMessage = "Connection to 5Ghz device failed";
			LOGGER.info("Going to wait for 1 minute before connecting the client to the wifi network");

			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			try {
				LOGGER.info(
						"Wifi Mac Address of the Connected client whose wifi Mac address is  added in the MAC Filter is:-"
								+ ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress());
				ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
						tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);

				status = ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
						passPhraseName);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}
			LOGGER.info("S6 ACTUAL : " + (status
					? "connected client device which is added in the MAC Filter is connected to 5GHZ wifi network since the MAC Filter mode is configured as 'Allow'"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Steps 7 to 10
			 */
			LOGGER.info("Going to wait for 1.5 minutes after connecting the client to the wifi network");

			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
			BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, connectedDeviceActivated,
					testId, new String[] { "s7", "s8", "s9", "s10" });

			/**
			 * STEP 11:Connect the connected client device whose wifi Mac address is not
			 * added in the MAC Filter to 5 GHz SSID and verify connection status
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 11: Connect the connected client device whose wifi Mac address is not added in the MAC Filter to 5GHz SSID and verify connection status");
			LOGGER.info(
					"EXPECTED: Device should not be connected with 5 GHz wifi network since the MAC Filter mode is configured as 'Allow'");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s11";
			status = false;
			errorMessage = "Connection to 5 Ghz  is successful even though the MAC Filter mode is configured as 'Allow'";

			LOGGER.info("Going to wait for 1 minute before connecting the client to the wifi network");
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			try {
				lockedDevices = ((Device) device).getConnectedDeviceList();
				connectedDeviceActivated = BroadBandConnectedClientUtils.getOtherConnectedClient(
						connectedDeviceActivated, tapEnv, lockedDevices, BroadBandTestConstants.WIFI,
						BroadBandTestConstants.BAND_5GHZ);
				LOGGER.info(
						"Wifi Mac Address of the Connected client whose wifi Mac address is not added in the MAC Filter is:-"
								+ ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress());
				ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
						tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);

				status = !ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
						passPhraseName);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}
			LOGGER.info("S11 ACTUAL : " + (status
					? "connected client device whose wifi Mac address is not added in the MAC Filter is not connected to 5GHZ wifi network since the MAC Filter mode is configured as 'Allow'"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 12:Verify whether the interface did'nt get the correct IPv4 address.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 12: Verify whether the interface did'nt get the correct IPv4 address.");
			LOGGER.info("EXPECTED:Interface IP address should not be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s12";
			status = false;
			errorMessage = "interface  got the correct IPV4 address";
			LOGGER.info("Going to wait for 1.5 minutes after connecting the client to the wifi network");

			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
			String osType = ((Device) connectedDeviceActivated).getOsType();
			status = !BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
					osType, connectedDeviceActivated, tapEnv);
			LOGGER.info("S12 ACTUAL : " + (status ? "Interface did'nt got the correct IPv4  address" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 13:Verify whether interface did'nt get the correct IPv6 address.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 13: Verify whether interface did'nt get the correct IPv6  address.");
			LOGGER.info("EXPECTED:Interface IP address should not be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s13";
			status = false;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				errorMessage = "Interface  got the correct IPV6 address";
				status = !BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
						osType, connectedDeviceActivated, tapEnv);
				LOGGER.info(
						"S13 ACTUAL : " + (status ? "Interface did'nt got the correct IPv6  address" : errorMessage));
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 13 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			/**
			 * STEP 14:Verify whether there is no connectivity using that particular
			 * interface using IPV4.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 14: Verify whether there is no connectivity using that particular interface using IPV4 ");
			LOGGER.info("EXPECTED: Connectivity check should'nt  return the status as 200");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s14";
			status = false;
			errorMessage = "Connectivty check using IPV4 address success";
			command = ((Device) connectedDeviceActivated).getOsType()
					.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
							? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
									.replace("<INTERFACE>", BroadbandPropertyFileHandler.getLinuxClientWifiInterface())
							: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
			response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
			status = !(CommonMethods.isNotNull(response)
					&& response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK));
			LOGGER.info("S14 ACTUAL : "
					+ (status ? "connectivity using that particular interface using IPV4 Failed" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 15:Verify whether there is no connectivity using that particular
			 * interface using IPV6.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 15: Verify whether there is no  connectivity using that particular interface using IPV6");
			LOGGER.info("EXPECTED: Connectivity check should'nt return status as 200");
			LOGGER.info("#####################################################################################");
			status = false;
			testStepNumber = "s15";
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				errorMessage = "Connectivty check using IPV6 address success";
				command = ((Device) connectedDeviceActivated).getOsType()
						.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
								? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV6_ADDRESS
								: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV6_ADDRESS;
				response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
				status = !(CommonMethods.isNotNull(response)
						&& response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK));
				LOGGER.info("S15 ACTUAL : "
						+ (status ? "connectivity using that particular interface using IPV6 Failed" : errorMessage));
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 15 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Following exception occured during execution : " + errorMessage);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, true);
		} finally {
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"POST CONDITION 1: Verify whether MAC Filtering mode is configured as 'Allow-All'(default Mode) by setting TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as 'false' using WebPA");
			LOGGER.info(
					"EXPECTED:Device should set the  MAC filtering mode as 'Allow All' when we disable the MAC Filter.");
			LOGGER.info("#####################################################################################");
			if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_ENABLE,
					RDKBTestConstants.CONSTANT_3, RDKBTestConstants.FALSE)) {
				LOGGER.error("NOT ABLE TO SET THE MAC FILTERING MODE AS ALLOW-ALL(DEFAULT MODE) USING WEBPA");
			}

			if (null != tableRowNumber) {
				LOGGER.info("#####################################################################################");
				LOGGER.info(
						"POST CONDITION 2: Verify and  delete  the device wifi Mac address in the MAC Filter list by using WEBPA DELETE command.");
				LOGGER.info(
						"EXPECTED:Should be able to delete the added wifi Mac address in MAC Filter list using webpa Delete command");
				LOGGER.info("#####################################################################################");
				status = false;
				webPaServerResponse = null;
				webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRowNumber);
				if (null != webPaServerResponse) {
					status = CommonMethods.isNotNull(webPaServerResponse.getMessage())
							&& webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
				}
				if (!status) {
					LOGGER.error(
							"NOT ABLE TO DELETE THE ADDED WIFI MAC ADDRESS IN THE MAC FILTER TABLE USING DELETE WEBPA COMMAND");
				}
			}
			LOGGER.info("ENDING TESTCASE :TC-RDKB-WIFI-5014 ");
		}
	}

	/**
	 * Test to verify wifi client connect disconnect events
	 * 
	 * <Pre-Condition 1. Get 5GHz supported client device</li>
	 * <li>1. Verify hostapd process status</li>
	 * <li>2. Verify CcspWifiSsp process status</li>
	 * <li>3. Create WiFi monitor debug file under /nvram directory</li>
	 * <li>4. Update the telemetry log upload interval to 5 min</li>
	 * <li>5. Update the telemetry log interval using webpa</li>
	 * <li>6. Verify Rapid Reconnect Indication Enable is disabled by default using
	 * webpa</li>
	 * <li>7. Enable Rapid reconnect indication enable using webpa</li>
	 * <li>8. Verify Rapid reconnect count enable is enabled for private SSIDs</li>
	 * <li>9. Verify default value of rapidReconnectMaxTime using webpa</li>
	 * <li>10. Verify client device connected with 5GHz SSID</li>
	 * <li>11. Verify SSID remains same before and after Enabling Rapid Reconnect
	 * Indication</li>
	 * <li>12. Verify client is connected with ath1 wifi interface in wifiMon log
	 * file</li>
	 * <li>13. Verify telemetry logging for 5GHz client in wifihealth.txt file</li>
	 * <li>Post-Condition 1. Disable RapidReconnectIndicationEnable using webpa</li>
	 * 
	 * @author ArunKumar Jayachandran
	 * @refactor Govardhan
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			TestGroup.NEW_FEATURE, TestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-CLIENT-1001")
	public void testTo(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CLIENT-1001");
		LOGGER.info("TEST DESCRIPTION: Test to verify wifi client connect disconnect events");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("Pre-Condition 3. Get 5GHz supported client device");
		LOGGER.info("1. Verify hostapd process status");
		LOGGER.info("2. Verify CcspWifiSsp process status");
		LOGGER.info("3. Create WiFi monitor debug file under /nvram directory");
		LOGGER.info("4. Update the telemetry log upload interval to 5 min");
		LOGGER.info("5. Update the telemetry log interval using webpa");
		LOGGER.info("6. Verify Rapid Reconnect Indication Enable is disabled by default using webpa");
		LOGGER.info("7. Enable Rapid reconnect indication enable using webpa");
		LOGGER.info("8. Verify Rapid reconnect count enable is enabled for private SSIDs");
		LOGGER.info("9. Verify default value of rapidReconnectMaxTime using webpa");
		LOGGER.info("10. Verify client device connected with 5GHz SSID");
		LOGGER.info("11. Verify SSID remains same before and after Enabling Rapid Reconnect Indication");
		LOGGER.info("12. Verify client is connected with ath1 wifi interface in wifiMon log file");
		LOGGER.info("13. Verify telemetry logging for 5GHz client in wifihealth.txt file");
		LOGGER.info("Post-Condition 1. Disable RapidReconnectIndicationEnable using webpa");
		LOGGER.info("#######################################################################################");
		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-WIFI-CLIENT-001";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store response
		String response = null;
		Dut clientDevice = null;
		Boolean preStatus = false;
		// variable declaration ends
		String macAddress = null;
		// String to store SSID
		String ssidFor5GhzBeforeRpdInd = null;
		String ssidFor5GhzAfterRpdInd = null;
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			BroadBandPreConditionUtils.executePreConditionToFactoryResetAndReacitivateDevice(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1, true);

			/**
			 * PRE CONDITION 3 : Get 5GHz supported client device
			 */

			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 3 : DESCRIPTION : Get 5GHz supported client device");
			LOGGER.info("PRE-CONDITION 3 : ACTION : Connect the client device with 5GHz SSID");
			LOGGER.info("PRE-CONDITION 3 : EXPECTED : Should connect the client with 5GHz SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to get the client device connected with 5GHz private ssid";
			clientDevice = BroadBandConnectedClientUtils
					.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			preStatus = (clientDevice != null);
			if (preStatus) {
				LOGGER.info("PRE-CONDITION 3 : ACTUAL: Successfully connected the 5GHz client device");
			} else {
				LOGGER.error("PRE-CONDITION 3 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + " PRE-CONDITION 3 : FAILED : " + errorMessage);
			}
			LOGGER.info("PRE-CONFIGURATIONS : FINAL STATUS -  " + preStatus);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			/**
			 * STEP 1 : Verify hostapd process status
			 */
			stepNumber = "s1";
			status = false;
			errorMessage = "Failed to get the hostapd process id";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: Verify hostapd process status");
			LOGGER.info("STEP 1: ACTION: Execute command: pidof hostapd");
			LOGGER.info("STEP 1: EXPECTED: hostapd process should run and should get the process id");
			LOGGER.info("******************************************************************************");
			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, true,
						BroadBandTestConstants.PROCESS_NAME_HOSTAPD);
				if (status) {
					LOGGER.info("STEP 1: ACTUAL: Successfully verified the hostapd process running status");
				} else {
					LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				errorMessage = "Not applicable for " + device.getModel();
				LOGGER.info("STEP 1: ACTUAL: step is not applicable for " + device.getModel());
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			/**
			 * STEP 2 : Verify CcspWifiSsp process status
			 */
			stepNumber = "s2";
			status = false;
			errorMessage = "Failed to get the CcspWifiSsp process id";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION: Verify CcspWifiSsp process status");
			LOGGER.info("STEP 2: ACTION: Execute command: pidof CcspWifiSsp");
			LOGGER.info("STEP 2: EXPECTED: CcspWifiSsp process should run and should get the process id");
			LOGGER.info("******************************************************************************");
			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, true,
						StbProcess.CCSP_WIFI_AGENT.getProcessName());
			} else {
				status = CommonMethods.isNotNull(tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_PID_OF,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								StbProcess.CCSP_WIFI_AGENT.getProcessName())));
			}
			if (status) {
				LOGGER.info("STEP 2: ACTUAL: Successfully verified the CcspWifiSsp process running status");
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			/**
			 * STEP 3 : Create WiFi monitor debug file under /nvram directory
			 */
			stepNumber = "s3";
			status = false;
			errorMessage = "Failed to create a file wifiMonDbg under /nvram directory";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION: Create WiFi monitor debug file under /nvram directory");
			LOGGER.info("STEP 3: ACTION: Execute command: touch /nvram/wifiMonDbg");
			LOGGER.info("STEP 3: EXPECTED: Should create the wifiMonDbg file under /nvram directory.");
			LOGGER.info("******************************************************************************");
			BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_TOUCH,
							BroadBandCommandConstants.FILE_WIFI_MON_DBG));
			status = (CommonMethods.isAtomSyncAvailable(device, tapEnv)) ? BroadBandCommonUtils
					.doesFileExistInAtomConsole(device, tapEnv, BroadBandCommandConstants.FILE_WIFI_MON_DBG).isStatus()
					: CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_WIFI_MON_DBG);
			if (status) {
				LOGGER.info("STEP 3: ACTUAL: Successfully created wifi Monitor debug file under nvram directory");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			/**
			 * STEP 4 : Update the telemetry log upload interval to 5 min
			 */
			stepNumber = "s4";
			status = false;
			errorMessage = "Failed to update upload interval value in upload file";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION: Update the telemetry log upload interval to 5 min");
			LOGGER.info("STEP 4: ACTION: Execute command: 1.echo 5 > /tmp/upload 2.ls /tmp/upload");
			LOGGER.info("STEP 4: EXPECTED: upload file should contain the value as 5");
			LOGGER.info("******************************************************************************");
			CommonMethods.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
					BroadBandCommandConstants.CMD_UPLOAD_TIME, CommonMethods.isAtomSyncAvailable(device, tapEnv));
			response = CommonMethods.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
							BroadBandCommandConstants.FILE_UPLOAD),
					CommonMethods.isAtomSyncAvailable(device, tapEnv)).trim();
			status = CommonMethods.isNotNull(response)
					&& CommonUtils.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.STRING_5);
			if (status) {
				LOGGER.info("STEP 4: ACTUAL: Successfully updated the upload file with interval time as 5");
			} else {
				LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			/**
			 * STEP 5 : Update the telemetry log interval using webpa
			 */
			stepNumber = "s5";
			status = false;
			errorMessage = "Failed to update the telemetry log interval for webpa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION: Update the telemetry log interval using webpa");
			LOGGER.info(
					"STEP 5: ACTION: Execute webpa set command: Parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval as 300");
			LOGGER.info("STEP 5: EXPECTED: Webpa set operation should success and value should be 300");
			LOGGER.info("******************************************************************************");
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_LOG_INTERVAL, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_300, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 5: ACTUAL: Successfully updated telemetry log interval using webpa set operation");
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			/**
			 * STEP 6 : Verify Rapid Reconnect Indication Enable is disabled by default
			 * using webpa
			 */
			stepNumber = "s6";
			status = false;
			String successMessage = "Successfully verified default status of rapid reconnect indication enable as "
					+ status;
			errorMessage = "Failed to get the response for webpa parameter Device.WiFi.X_RDKCENTRAL-COM_RapidReconnectIndicationEnable";
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION: Verify Rapid Reconnect Indication Enable is disabled by default using webpa");
			LOGGER.info(
					"STEP 6: ACTION: Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_RapidReconnectIndicationEnable");
			LOGGER.info("STEP 6: EXPECTED: Should get the response from webpa and value as false");
			LOGGER.info("******************************************************************************");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_RAPID_RECONNECT_INDICATION);
			status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("STEP 6: ACTUAL: " + successMessage);
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			/**
			 * STEP 7 : Enable Rapid reconnect indication enable using webpa
			 */
			stepNumber = "s7";
			status = false;
			errorMessage = "Failed update the rapid reconnect indication enable parameter Device.WiFi.X_RDKCENTRAL-COM_RapidReconnectIndicationEnable using webpa";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION: Enable Rapid reconnect indication enable using webpa");
			LOGGER.info(
					"STEP 7: ACTION: Execute webpa set command: Parameter: Device.WiFi.X_RDKCENTRAL-COM_RapidReconnectIndicationEnable as true");
			LOGGER.info(
					"STEP 7: EXPECTED: Webpa set operation should success and should enable the rapid reconnect indication enable");
			LOGGER.info("******************************************************************************");
			ssidFor5GhzBeforeRpdInd = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device,
					tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_RAPID_RECONNECT_INDICATION, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL: Successfully enabled rapid reconnect indication enable parameter using webpa");
			} else {
				LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			/**
			 * STEP 8 : Verify Rapid reconnect count enable is enabled for private SSIDs
			 */
			stepNumber = "s8";
			status = false;
			errorMessage = "Failed to get the response for webpa parameters Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_rapidReconnectCountEnable, Device.WiFi.AccessPoint.10101.X_RDKCENTRAL-COM_rapidReconnectCountEnable";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION: Verify Rapid reconnect count enable is enabled for private SSIDs");
			LOGGER.info(
					"STEP 8: ACTION: Execute webpa command: 1. Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_rapidReconnectCountEnable\n 2. Device.WiFi.AccessPoint.10101.X_RDKCENTRAL-COM_rapidReconnectCountEnable");
			LOGGER.info("STEP 8: EXPECTED: Should be in enabled state for rapid reconnect count enable");
			LOGGER.info("******************************************************************************");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_2_4_RAPID_COUNT_ENABLE);
			if (CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.TRUE)) {
				response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_5_RAPID_COUNT_ENABLE);
				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
			}
			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL: Successfully verified rapid reconnect count enable is enabled for private SSIDs");
			} else {
				LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			/**
			 * STEP 9 : Verify default value of rapidReconnectMaxTime using webpa
			 */
			stepNumber = "s9";
			status = false;
			errorMessage = "Failed to get the response for webpa parameter Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_rapidReconnectMaxTime, Device.WiFi.AccessPoint.10101.X_RDKCENTRAL-COM_rapidReconnectMaxTime";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION: Verify default value of rapidReconnectMaxTime using webpa");
			LOGGER.info(
					"STEP 9: ACTION: Execute webpa command: 1. Device.WiFi.AccessPoint.10001.X_RDKCENTRAL-COM_rapidReconnectMaxTime\n 2. Device.WiFi.AccessPoint.10101.X_RDKCENTRAL-COM_rapidReconnectMaxTime");
			LOGGER.info("STEP 9: EXPECTED: Response should contain the value as 180");
			LOGGER.info("******************************************************************************");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_2_4_RECONNECT_MAXTIME);
			if (CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.STRING_180)) {
				response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_5_RECONNECT_MAXTIME);
				status = CommonMethods.isNotNull(response)
						&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_180);
			}
			if (status) {
				LOGGER.info("STEP 9: ACTUAL: Successfully verified rapid reconnect max time as " + response);
			} else {
				LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			/**
			 * STEP 10 : Verify client device connected with 5GHz SSID
			 */
			stepNumber = "s10";
			status = false;
			errorMessage = "Failed to connect client device with 5GHz SSID";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION: Verify client device connected with 5GHz SSID");
			LOGGER.info("STEP 10: ACTION: Execute command: SSID, passphrase");
			LOGGER.info("STEP 10: EXPECTED: client should connect with 5GHz SSID");
			LOGGER.info("******************************************************************************");
			if (!ConnectedNattedClientsUtils.verifyConnectToSSID(clientDevice, tapEnv, tapEnv.executeWebPaCommand(
					device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID), true)) {
				clientDevice = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			}
			status = (clientDevice != null);
			if (status) {
				LOGGER.info("STEP 10: ACTUAL: Successfully verified 5GHz client device");
			} else {
				LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			/**
			 * STEP 11 : Verify client device connected with 5GHz SSID
			 */
			stepNumber = "s11";
			status = false;
			errorMessage = "Failed to verify SSID remains same before and after enabling Rapid Reconnect Indication";
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP 11: DESCRIPTION: Verify SSID remains same before and after Enabling Rapid Reconnect Indication");
			LOGGER.info("STEP 11: ACTION: Execute command: "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);
			LOGGER.info(
					"STEP 11: EXPECTED: SSID should remain same before and after enabling Rapid Reconnect Indication");
			LOGGER.info("******************************************************************************");
			ssidFor5GhzAfterRpdInd = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device,
					tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			status = CommonMethods.isNotNull(ssidFor5GhzAfterRpdInd) && CommonMethods.isNotNull(ssidFor5GhzBeforeRpdInd)
					&& CommonMethods.patternMatcher(ssidFor5GhzBeforeRpdInd, ssidFor5GhzAfterRpdInd);
			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL: Successfully verified SSID remains same before and after enabling Rapid Reconnect Indication");
			} else {
				LOGGER.error("STEP 11: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			/**
			 * STEP 12 : Verify client is connected with ath1 wifi interface in wifiMon log
			 * file
			 */
			stepNumber = "s12";
			status = false;
			errorMessage = "Failed to get the response connected in /tmp/wifiMon file";
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP 12: DESCRIPTION: Verify client is connected with ath1 wifi interface in wifiMon log file");
			LOGGER.info("STEP 12: ACTION: Execute command: grep -i connected /tmp/wifiMon | grep -I ap:1");
			LOGGER.info("STEP 12: EXPECTED: Response should contain the log message and should connected with ap:1");
			LOGGER.info("******************************************************************************");
			response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
					BroadBandTraceConstants.LOG_MESSAGE_CONNECTED, BroadBandCommandConstants.FILE_WIFI_MON,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (CommonMethods.isNotNull(response)) {
				macAddress = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_CONNECTED_DEVICE_MAC);
				status = CommonMethods.isNotNull(macAddress);
			}
			if (status) {
				LOGGER.info("STEP 12: ACTUAL: Successfully verified log message for client connected with 5GHz SSID");
			} else {
				LOGGER.error("STEP 12: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			/**
			 * STEP 13 : Verify telemetry logging for 5GHz client in wifihealth.txt file
			 */
			stepNumber = "s13";
			status = false;
			errorMessage = "Failed to get the log message WIFI_RECONNECT in wifihealth.txt file or could not verify with emacAddress got in STEP 11 with the response from /rdklogs/logs/wifihealth.txt";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION: Verify telemetry logging for 5GHz client in wifihealth.txt file");
			LOGGER.info("STEP 13: ACTION: Execute command: grep -i WIFI_RECONNECT /rdklogs/logs/wifihealth.txt");
			LOGGER.info("STEP 13: EXPECTED: Response should contain the log message for 5GHz reconnect");
			LOGGER.info("******************************************************************************");
			response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_RECONNECT,
					BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(macAddress)
					&& CommonUtils.isGivenStringAvailableInCommandOutput(response,
							BroadBandTraceConstants.LOG_MESSAGE_WIFI_RECONNECT)
					&& CommonUtils.isGivenStringAvailableInCommandOutput(response, macAddress);
			if (status) {
				LOGGER.info("STEP 13: ACTUAL: Successfully verified the telemetry log message in wifihealth.txt file");
			} else {
				LOGGER.error("STEP 13: ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying wifi client connect disconnect events" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * POST CONDITION 1 : Disable RapidReconnectIndicationEnable using webpa
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1: DESCRIPTION: Disable RapidReconnectIndicationEnable using webpa");
			LOGGER.info(
					"POST-CONDITION 1: ACTION: Execute webpa set command: Parameter: Device.WiFi.X_RDKCENTRAL-COM_RapidReconnectIndicationEnable data type: bool value: false");
			LOGGER.info("POST-CONDITION 1: EXPECTED: Should disable the RapidReconnectIndicationEnable parameter");
			LOGGER.info("#######################################################################################");
			if (preStatus) {
				BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_REMOVE_FORCEFULLY,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandCommandConstants.FILE_WIFI_MON_DBG));
				status = !CommonMethods.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_WIFI_MON_DBG)
						&& BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_RAPID_RECONNECT_INDICATION,
								BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
								BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
								BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				if (status) {
					LOGGER.info(
							"POST-CONDITION 1: ACTUAL: Successfully disabled rapid reconnect indication enable parameter");
				} else {
					LOGGER.info(
							"POST-CONDITION 1: ACTUAL: Failed to disable rapid reconnect indication enable parameter");
				}
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CLIENT-1001");
	}

	/**
	 * Verify the IP range in 2.4GHZ WiFi connected client when DHCP Server
	 * Disabled.
	 * <ol>
	 * <li>PRE-CONDITION : Get the DHCP Minimum and Maximum IP Range Configured for
	 * Gateway.</li>
	 * <li>Step 1 : Verify the DHCP Server Enable Status value for the 2.4GHZ WiFi
	 * client Via WEBPA Parameter: Device.DHCPv4.Server.Enable.</li>
	 * <li>Step 2 : Set and Verify the DHCP Server Disabled for 2.4GHZ WiFi client
	 * via WEBPA Parameter: Device.DHCPv4.Server.Enable with Set Value as
	 * FALSE.</li>
	 * <li>Step 3 : Obtain and Connect to the 2.4GHZ WiFi Client Associated with the
	 * Gateway and verify connection status.</li>
	 * <li>Step 4 : Verify IPv4 assigned on the 2.4GHZ WiFi client is not in the
	 * DHCP address range.</li>
	 * <li>Step 5 : Verify Internet is not accessible by using interface IPv4 on the
	 * 2.4GHZ WiFi client Connected.</li>
	 * <li>POST-CONDITION 1 : Set and Verify the DHCP Server Enabled for 2.4GHZ WiFi
	 * client via WEBPA Parameter: Device.DHCPv4.Server.Enable with Set Value as
	 * TRUE.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Vignesh
	 * @refactor Govardhan
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-WIFI-2GHZ-DHCP-IPV4-DSBLE-5001")

	public void testToVerifyIpv4ValuePostDhcpServerDisableFor2GhzWifiClient(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-2GHZ-DHCP-IPV4-DSBLE-501";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		Dut deviceConnectedWith2GHZ = null;
		boolean isDHCPServerDisabled = false;
		HashMap<String, String> defaultDchpIpv4ValuesMap = null;
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-2GHZ-DHCP-IPV4-DSBLE-501");
		LOGGER.info("TEST DESCRIPTION: Verify the IP range in 2.4GHZ WiFi connected client when DHCP Server Disabled.");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION : Get the DHCP Minimum and Maximum IP Range Configured for Gateway.");
		LOGGER.info("1. Verify the DHCP Server Enable Status value for the 2.4GHZ WiFi client Via WEBPA GET Parameter: "
				+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER);
		LOGGER.info("2. Set and Verify the DHCP Server Disabled for 2.4GHZ WiFi client via WEBPA Parameter: "
				+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER + " with Set Value as "
				+ BroadBandTestConstants.FALSE);
		LOGGER.info(
				"3. Obtain and Connect to the 2.4GHZ Client Associated with the Gateway and verify connection status.");
		LOGGER.info("4. Verify IPv4  assigned for client connected with 2.4GHz SSID is not in DHCP address range.");
		LOGGER.info(
				"5. Verify internet is not accessibble by using intetface IPv4 obtained on the 2.4GHZ WiFi client.");
		LOGGER.info(
				"POST-CONDITION 1 : Set and Verify the DHCP Server Enabled for 2.4GHZ WiFi client via WEBPA Parameter: "
						+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER + " with Set Value as "
						+ BroadBandTestConstants.TRUE);
		LOGGER.info("#######################################################################################");
		try {
			/**
			 * Pre Condition: To get the default DHCP Server Values.
			 */
			defaultDchpIpv4ValuesMap = executePreconditionToGetTheDefaultDchpIpv4Values(device, tapEnv);

			/**
			 * Step 1-2: Verify the DHCP Server value for the 2GHZ client.
			 */
			isDHCPServerDisabled = executeTestStepsToSetAndGetDHCPServer(device, testCaseId);

			/**
			 * STEP 3 : OBTAIN A 2.4GHZ WiFi CLIENT ASSOSIATED WITH THE GATEWAY
			 */
			stepNum = "S3";
			errorMessage = "Failed to obtain a 2.4GHZ WiFi client Associated with the gateway.";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3 : DESCRIPTION : Connect the client setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info("STEP 3 : ACTION : Obtain and connect to 2.4GHZ WiFi Client associated with the gateway");
			LOGGER.info("STEP 3 : EXPECTED : The 2.4GHZ WiFi client connection should be successful.");
			LOGGER.info("**********************************************************************************");
			try {
				deviceConnectedWith2GHZ = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (null != deviceConnectedWith2GHZ);
			if (status) {
				LOGGER.info(
						"Step 3 : ACTUAL : Obtained and connected to 2.4GHZ WiFi Client Associated with the gateway successfully.");
			} else {
				LOGGER.error("STEP 3 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 4-5: Verify the DHCP Server value for the 2GHZ client.
			 */
			executeTestStepsToVerifyIpv4AndInternetConnectivity(device, testCaseId, deviceConnectedWith2GHZ,
					defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_DHCPV4_ENDING_ADDRESS),
					defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_DHCPV4_BEGIN_ADDRESS));

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			/**
			 * Post Condition: To Enable the DHCP Server.
			 */
			PostConditionToEnableDhcpServer(device, isDHCPServerDisabled);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-2GHZ-DHCP-IPV4-DSBLE-5001");
	}

	/**
	 * Pre-Condition Method to get the default DHCP Server Values.
	 * 
	 * @param device                     {@link Dut}
	 * @param testCaseId                 Test case Id
	 * @param deviceConnectedWithGateway {@link Dut}
	 * @refactor Govardhan
	 */
	public static HashMap<String, String> executePreconditionToGetTheDefaultDchpIpv4Values(Dut device,
			AutomaticsTapApi tapEnv) {
		boolean status = false;
		HashMap<String, String> defaultDchpIpv4ValuesMap = null;
		String errorMessage = null;
		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		LOGGER.info("PRE-CONDITION STEPS");
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION : DESCRIPTION : Get the DHCP Minimum and Maximum IP Range Configured for Gateway.");
		LOGGER.info("PRE-CONDITION : ACTION : Get the DHCP Minimum and Maximum IP Range using WEBPA Parameter : "
				+ BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_ENDING_IP_ADDRESS + " and "
				+ BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_STARTING_IP_ADDRESS);
		LOGGER.info(
				"PRE-CONDITION : EXPECTED : Getting the Minimum and Maximum Range using WEBPA Parameter Should be successful.");
		LOGGER.info("#######################################################################################");
		errorMessage = "Unable to Get the DHCP IPV4 Minimum and Maximum Range Using WEBPA.";
		defaultDchpIpv4ValuesMap = BroadBandPreConditionUtils.executePreconditionToGetTheDefaultDchpIpv4Values(device,
				tapEnv);
		if (defaultDchpIpv4ValuesMap != null) {
			LOGGER.info(
					"PRE-CONDITION : ACTUAL: Getting the Minimum and Maximum Range using WEBPA Parameter is successful.");
		} else {
			LOGGER.error("PRE-CONDITION : ACTUAL: " + errorMessage);
			throw new TestException("Pre Condition : FAILED : " + errorMessage);
		}
		LOGGER.info("PRE-CONFIGURATIONS : FINAL STATUS -  " + status);
		LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
		return defaultDchpIpv4ValuesMap;
	}

	/**
	 * Helper Method to Get, Set and Verify the DHCP Server.
	 * 
	 * @param device     {@link Dut}
	 * @param testCaseId Test case Id
	 * @refactor Govardhan
	 */
	public static boolean executeTestStepsToSetAndGetDHCPServer(Dut device, String testCaseId) {
		// Variable Declaration begins
		String errorMessage = null;
		String stepNum = "";
		String webPaResponse = null;
		boolean status = false;

		// Variable Declaration Ends

		stepNum = "S1";
		errorMessage = "DHCP Server value obtained via WEBPA is not " + BroadBandTestConstants.TRUE + " As Expected";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP 1: DESCRIPTION : Verify the DHCP Server Enable Status value for the client connected Via WEBPA GET Parameter: "
						+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER);
		LOGGER.info(
				"STEP 1: ACTION : Execute command  :curl -X -H Authorization: Bearer <SAT_TOKEN> GET  '<WEBPA_URL><CM-MAC>/config?names="
						+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER);
		LOGGER.info("STEP 1: EXPECTED : DHCP Server value obtained via WEBPA should be " + BroadBandTestConstants.TRUE
				+ " as Expected.");
		LOGGER.info("**********************************************************************************");
		webPaResponse = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER);
		LOGGER.info("DHCP Server Enable Status value Retrieved From WEBPA GET is " + webPaResponse);
		status = CommonMethods.isNotNull(webPaResponse)
				&& CommonUtils.patternSearchFromTargetString(webPaResponse, BroadBandTestConstants.TRUE);
		if (status) {
			LOGGER.info("STEP 1: ACTUAL : DHCP Server value obtained via WEBPA is " + BroadBandTestConstants.TRUE
					+ " As Expected.");
		} else {
			LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		stepNum = "S2";
		errorMessage = "Unable to disable the DHCP Server via WEBPA Parameter "
				+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER + " with value as"
				+ BroadBandTestConstants.FALSE;
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP 2: DESCRIPTION : Set and Verify the DHCP Server Disabled for client connected via WEBPA Parameter:"
						+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER + " with Set Value as FALSE.");
		LOGGER.info(
				"STEP 2: ACTION : Execute WEBPA SET command :curl -4 -k -H Authorization: Bearer <SAT Token> -X PATCH <WEBPA_URL><MAC Address>/config?names="
						+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER + " with value set as "
						+ BroadBandTestConstants.FALSE);
		LOGGER.info("STEP 2: EXPECTED : DHCP Server should be disabled successfully via WEBPA SET.");
		LOGGER.info("**********************************************************************************");
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER, BroadBandTestConstants.CONSTANT_3,
				BroadBandTestConstants.FALSE, BroadBandTestConstants.THREE_MINUTES,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (status) {
			LOGGER.info("STEP 2: ACTUAL : DHCP Server is disabled successfully via WEBPA SET with value as "
					+ BroadBandTestConstants.FALSE);
		} else {
			LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		return status;
	}

	/**
	 * Helper Method to Verify Ipv4 And Internet Connectivity in the client.
	 * 
	 * @param device                     {@link Dut}
	 * @param testCaseId                 Test case Id
	 * @param deviceConnectedWithGateway {@link Dut}
	 * @refactor Govardhan
	 */
	public static void executeTestStepsToVerifyIpv4AndInternetConnectivity(Dut device, String testCaseId,
			Dut deviceConnectedWithGateway, String dhcpMaxRange, String dhcpMinRange) {
		// Variable Declaration begins
		String errorMessage = null;
		String stepNum = "";
		boolean status = false;
		// Variable Declation Ends
		/**
		 * Step 4 : Verify IPv4 assigned on the client connected is not in the DHCP
		 * address range.
		 */
		stepNum = "S4";
		errorMessage = "Failed to verify the assigned IPv4 in the connected client when DHCP servers is disabled.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 4: DESCRIPTION : Verify IPv4  assigned on the  client is not in the DHCP address range.");
		LOGGER.info(
				"STEP 4: ACTION : Get the device IPv4 address using below command :For windows: ipconfig | grep -A 10 Wireless LAN adapter Wi-Fi |grep -i Pv4 Address For Linux :ifconfig | grep 'inet '");
		LOGGER.info(
				"STEP 4: EXPECTED : Obtained IPv4 address value should not be within DHCP address range of the Gateway.");
		LOGGER.info("**********************************************************************************");
		boolean isRenewSuccessful = false;
		long startTime = System.currentTimeMillis();
		do {
			isRenewSuccessful = BroadBandConnectedClientUtils.renewIpAddressInDhcp(device, tapEnv,
					deviceConnectedWithGateway);
			if (isRenewSuccessful) {
				String ipAddressRetrievedFromClient = BroadBandConnectedClientUtils
						.getIpv4AddressFromConnClientWithoutArpCheck(tapEnv, device, deviceConnectedWithGateway);
				LOGGER.info("IP ADDRESS ASSIGNED TO THE CONNECTED CLIENT FROM DHCP : " + ipAddressRetrievedFromClient);
				if (CommonUtils.isNotEmptyOrNull(ipAddressRetrievedFromClient)) {
					LOGGER.info("IS OBTAINED IPV4 BETWEEN DHCP RANGE :"
							+ BroadBandConnectedClientUtils.validateDhcpIpv4AddressBetweenRangeInConnectedClient(
									dhcpMinRange, dhcpMaxRange, ipAddressRetrievedFromClient));
					status = !BroadBandConnectedClientUtils.validateDhcpIpv4AddressBetweenRangeInConnectedClient(
							dhcpMinRange, dhcpMaxRange, ipAddressRetrievedFromClient);

				} else {
					errorMessage = "Unable to get the DHCP IPV4 Address for the Client Connected.";
					break;
				}
			} else {
				errorMessage = "Unable to Renew DHCP IPV4 Address for the Client Connected After Disabling the DHCP Server.";
				break;
			}
		} while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
				&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
		if (status) {
			LOGGER.info(
					"STEP 4: ACTUAL : Obtained IPv4 address value is not within DHCP address range of the Gateway as Expected.");
		} else {
			LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		/**
		 * Step 5 : Verify Internet is not Accessible by using interface IPv4 on the
		 * client.
		 */
		stepNum = "S5";
		errorMessage = "Failed to verify the internet connectiviety using the IPV4 address Obtained in the client when DHCP servers is disabled.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 5 : DESCRIPTION : Verify internet is not accessibble by using intetface IPv4 on the client.");
		LOGGER.info(
				"STEP 5 : ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -4 -n 5 google.com ON THE CONNECTED LAN CLIENT");
		LOGGER.info(
				"STEP 5 : EXPECTED : Internet should not be accessible using DHCP IPv4 address obtained in the client. ");
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Is Internet Accessible Via CURL Command: " + BroadBandConnectedClientUtils
				.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv, deviceConnectedWithGateway,
						BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
						BroadBandTestConstants.IP_VERSION4)
				.isStatus());
		status = !BroadBandConnectedClientUtils
				.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv, deviceConnectedWithGateway,
						BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
						BroadBandTestConstants.IP_VERSION4)
				.isStatus();
		if (status) {
			LOGGER.info(
					"STEP 5 : ACTUAL : Internet is not Accessible using DHCP IPv4 address obtained in the client As Expected.");
		} else {
			LOGGER.error("STEP 5 : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	}

	/**
	 * Post-Condition Method to set the DHCP Server to Enabvled Via WEBPA.
	 * 
	 * @param device               {@link Dut}
	 * @param isDhcpServerDisabled boolean value
	 * @refactor Govardhan
	 */
	public static void PostConditionToEnableDhcpServer(Dut device, boolean isDHCPServerDisabled) {
		boolean status = false;
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		if (isDHCPServerDisabled) {
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION : DESCRIPTION : Set and Verify the DHCP Server Enabled for 5GHZ WiFi client via WEBPA Parameter: "
							+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER + " with Set Value as "
							+ BroadBandTestConstants.TRUE);
			LOGGER.info(
					"POST-CONDITION : ACTION : Execute command :curl -4 -k -H Authorization: Bearer <SAT Token> -X PATCH <WEBPA_URL><MAC Address>/config -d "
							+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER);
			LOGGER.info("POST-CONDITION : EXPECTED : DHCP Server should be enabled successfully via WEBPA. ");
			LOGGER.info("#######################################################################################");
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.THREE_MINUTES,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"POST-CONDITION : ACTUAL : DHCP Server is enabled for 5GHZ WiFi client successfully via WEBPA.");
			} else {
				LOGGER.error(
						"POST-CONDITION : ACTUAL : Failed to Enable the DHCP Server for 5GHZ WiFi client via WEBPA.");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		} else {
			LOGGER.info("DHCP Server is Already Enabled client, So skipping Post Condition.");
		}
	}

	/**
	 * Test case is created as part of RDKB Coverage AUTOMATION based on the Test
	 * Case : Verify the IP range in 5GHZ WiFi connected client when DHCP Server
	 * Disabled.
	 * <ol>
	 * <li>PRE-CONDITION : Get the DHCP Minimum and Maximum IP Range Configured for
	 * Gateway.</li>
	 * <li>Step 1 : Verify the DHCP Server Enable Status value for the 5GHZ WiFi
	 * client Via WEBPA Parameter: Device.DHCPv4.Server.Enable.</li>
	 * <li>Step 2 : Set and Verify the DHCP Server Disabled for 5GHZ WiFi client via
	 * WEBPA Parameter: Device.DHCPv4.Server.Enable with Set Value as FALSE.</li>
	 * <li>Step 3 : Obtain and Connect to the 5GHZ WiFi Client Associated with the
	 * Gateway and verify connection status.</li>
	 * <li>Step 4 : Verify IPv4 assigned on the 5GHZ WiFi client is not in the DHCP
	 * address range.</li>
	 * <li>Step 5 : Verify Internet is not accessible by using interface IPv4 on the
	 * 5GHZ WiFi client Connected.</li>
	 * <li>POST-CONDITION 1 : Set and Verify the DHCP Server Enabled for 5GHZ WiFi
	 * client via WEBPA Parameter: Device.DHCPv4.Server.Enable with Set Value as
	 * TRUE.</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Vignesh
	 * @refactor Govardhan
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-WIFI-5GHZ-DHCP-IPV4-DSBLE-5001")

	public void testToVerifyIpv4ValuePostDhcpServerDisableFor5GhzWifiClient(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-5GHZ-DHCP-IPV4-DSBLE-501";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		Dut deviceConnectedWith5GHZ = null;
		boolean isDHCPServerDisabled = false;
		HashMap<String, String> defaultDchpIpv4ValuesMap = null;
		// Variable Declaration Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-2GHZ-DHCP-IPV4-DSBLE-501");
		LOGGER.info("TEST DESCRIPTION: Verify the IP range in 5GHZ WiFi connected client when DHCP Server Disabled.");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION : Get the DHCP Minimum and Maximum IP Range Configured for Gateway.");
		LOGGER.info("1. Verify the DHCP Server Enable Status value for the 5GHZ WiFi client Via WEBPA GET Parameter: "
				+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER);
		LOGGER.info("2. Set and Verify the DHCP Server Disabled for 5GHZ WiFi client via WEBPA Parameter: "
				+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER + " with Set Value as "
				+ BroadBandTestConstants.FALSE);
		LOGGER.info(
				"3. Obtain and Connect to the 5GHZ Client Associated with the Gateway and verify connection status.");
		LOGGER.info("4. Verify IPv4  assigned for client connected with 5GHz SSID is not in DHCP address range.");
		LOGGER.info("5. Verify internet is not accessibble by using intetface IPv4 obtained on the 5GHZ WiFi client.");
		LOGGER.info(
				"POST-CONDITION 1 : Set and Verify the DHCP Server Enabled for 5GHZ WiFi client via WEBPA Parameter: "
						+ BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_SERVER + " with Set Value as "
						+ BroadBandTestConstants.TRUE);
		LOGGER.info("#######################################################################################");
		try {
			/**
			 * Pre Condition: To get the default DHCP Server Values.
			 */
			defaultDchpIpv4ValuesMap = executePreconditionToGetTheDefaultDchpIpv4Values(device, tapEnv);

			/**
			 * Step 1-2: Verify the DHCP Server value for the 5GHZ client.
			 */
			isDHCPServerDisabled = executeTestStepsToSetAndGetDHCPServer(device, testCaseId);

			/**
			 * STEP 3 : OBTAIN A 5GHZ WiFi CLIENT ASSOSIATED WITH THE GATEWAY
			 */
			stepNum = "S3";
			errorMessage = "Failed to obtain a 5GHZ WiFi client Associated with the gateway.";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3 : DESCRIPTION : Connect the client setup to 5GHz SSID and verify connection status.");
			LOGGER.info("STEP 3 : ACTION : Obtain and connect to 5GHZ WiFi Client associated with the gateway");
			LOGGER.info("STEP 3 : EXPECTED : The 5GHZ WiFi client connection should be successful.");
			LOGGER.info("**********************************************************************************");
			try {
				deviceConnectedWith5GHZ = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (null != deviceConnectedWith5GHZ);
			if (status) {
				LOGGER.info(
						"Step 3 : ACTUAL : Obtained and connected to 5GHZ WiFi Client Associated with the gateway successfully.");
			} else {
				LOGGER.error("STEP 3 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 4-5: Verify the DHCP Server value for the 5GHZ client.
			 */
			executeTestStepsToVerifyIpv4AndInternetConnectivity(device, testCaseId, deviceConnectedWith5GHZ,
					defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_DHCPV4_ENDING_ADDRESS),
					defaultDchpIpv4ValuesMap.get(BroadBandTestConstants.DEFAULT_DHCPV4_BEGIN_ADDRESS));

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			/**
			 * Post Condition: To Enable the DHCP Server.
			 */
			PostConditionToEnableDhcpServer(device, isDHCPServerDisabled);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-5GHZ-DHCP-IPV4-DSBLE-5001");
	}

	/**
	 * This test case is to verify the controlled user access (Allow All) to a
	 * Specific Device for 2.4 and 5 GHZ against different MAC addresses by way of
	 * MAC ACLs
	 * 
	 * <ol>
	 * <li>STEP 1:Verify whether MAC Filtering mode is configured as 'Allow-All' by
	 * setting TR-181 parameter
	 * 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as 'false' using
	 * WebPA</li>
	 * 
	 * <li>STEP 2:verify getting the Configured MAC Filter Mode by using this
	 * TR-181parameter
	 * 'Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode'</li>
	 * 
	 * <li>STEP 3:Connect the device to 2.4 GHz SSID and verify connection
	 * status</li>
	 * 
	 * <li>STEP 4 :Verify whether interface got the correct IPv4 address</li>
	 * 
	 * <li>STEP 5 :Verify whether interface got the correct IPv6 address</li>
	 *
	 * <li>STEP 6 :Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * 
	 * <li>STEP 7 :Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * 
	 * <li>STEP 8: Verify whether MAC Filtering mode is configured as 'Allow-All' by
	 * setting TR-181 parameter
	 * 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as 'false' using
	 * WebPA</li>
	 * 
	 * <li>STEP 9:verify getting the Configured MAC Filter Mode by using this
	 * TR-181parameter
	 * 'Device.WiFi.AccessPoint.10101.X_COMCAST-COM_MAC_FilteringMode'</li>
	 * 
	 * <li>STEP 10: Connect the device to 5 GHz SSID and verify connection
	 * status</li>
	 * 
	 * <li>STEP 11 :Verify whether interface got the correct IPv4 address</li>
	 * 
	 * <li>STEP 12 :Verify whether interface got the correct IPv6 address</li>
	 *
	 * <li>STEP 13 :Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * 
	 * <li>STEP 14 :Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * 
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Joseph_Maduram
	 * @refactor Said Hisham
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WIFI-5011")
	public void testToVerifyMacFilterAsAllowAll(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-511";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// server response
		String response = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;

		try {

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"PRE-CONDITION 1 : DESCRIPTION: Verify whether Private 2.4 GHz SSID 'Device.WiFi.SSID.10001.Enable' is enabled using WebPA.");
			LOGGER.info("PRE-CONDITION 1 : ACTION: Execute WebPA Command 'Device.WiFi.SSID.10001.Enable'");
			LOGGER.info("PRE-CONDITION 1 : EXPECTED: WebPA Set and Verify  should be successful ");
			LOGGER.info("**********************************************************************************");
			if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					AutomaticsConstants.CONSTANT_3, AutomaticsConstants.TRUE)) {
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
						+ "NOT ABLE TO ENABLE THE 2.4GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
			}

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"PRE-CONDITION 2 : DESCRIPTION: Verify whether Private 5 GHz SSID 'Device.WiFi.SSID.10101.Enable' is enabled using WebPA.");
			LOGGER.info("PRE-CONDITION 2 : ACTION: Execute WebPA Command 'Device.WiFi.SSID.10101.Enable'");
			LOGGER.info("PRE-CONDITION 2 : EXPECTED: WebPA Set and Verify  should be successful ");
			LOGGER.info("**********************************************************************************");
			if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					AutomaticsConstants.CONSTANT_3, AutomaticsConstants.TRUE)) {
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
						+ "NOT ABLE TO ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
			}

			/**
			 * STEP 1: Verify whether MAC Filtering mode is configured as "Allow-All" by
			 * setting TR-181 parameter
			 * Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable as "false" using
			 * WebPA for 2.4GHZ.
			 * 
			 */
			testStepNumber = "s1";
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 1: DESCRIPTION:  Verify whether MAC Filtering mode is configured as 'Allow-All' by setting TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as 'false' using WebPA");
			LOGGER.info("STEP 1: ACTION: Execute the WebPA command");
			LOGGER.info(
					"STEP 1: EXPECTED: Device should set MAC filtering mode as 'Allow All' when we disable the MAC Filter.");
			LOGGER.info("#####################################################################################");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_ENABLE,
					AutomaticsConstants.CONSTANT_3, AutomaticsConstants.FALSE);
			errorMessage = "Unable to Set the MAC Filter as Allow All/Disable by setting false to the  TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' by using Webpa commands.";
			LOGGER.info("STEP 1: ACTUAL : " + (status
					? "Successfully Set the MAC Filter as  Allow-All/disabled by setting the TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as false via webpa"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2:verify getting the Configured MAC Filter Mode by using this
			 * TR-181parameter
			 * "Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode" .
			 * 
			 */
			testStepNumber = "s2";
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 2: DESCRIPTION: verify getting the Configured MAC Filter Mode by using  this TR-181parameter 'Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode'");
			LOGGER.info("STEP 2: ACTION: Execute the WebPA command ");
			LOGGER.info(
					"STEP 2: EXPECTED: The configured MAC Filter mode for the device should be as Allow-All/disabled ");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_MODE);
			status = response.equalsIgnoreCase(BroadBandTestConstants.MAC_FILTER_ALLOW_ALL);
			errorMessage = "MAC Filter mode is not configured as Allow-All/disabled";
			LOGGER.info("STEP 2: ACTUAL : "
					+ (status ? "MAC Filter mode for the device is configured as Allow-All/disabled" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3:Connect the device to 2.4 GHz SSID and verify connection status .
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 3: DESCRIPTION: Connect the device to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP 3: ACTION: connect the device with 2.4 GHz wifi network");
			LOGGER.info("STEP 3: EXPECTED: Device should be connected with 2.4 GHz wifi network");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s3";
			status = false;
			errorMessage = "Connection to 2.4Ghz device failed";
			// get the 2.4ghz SSId and password of RDKB device
			// get all connected devices and connect a device to 5Ghz

			connectedDeviceActivated = BroadBandConnectedClientUtils
					.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			status = (null != connectedDeviceActivated);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			// wait for 1.5 min for connected client to get the connectivity
			tapEnv.waitTill(90000);

			/** Steps s4 to s7 */
			BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, connectedDeviceActivated,
					testId, new String[] { "s4", "s5", "s6", "s7" });

			/**
			 * STEP 8: Verify whether MAC Filtering mode is configured as "Allow-All" by
			 * setting TR-181 parameter
			 * "Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable" as "false" using
			 * WebPA for 5 GHZ.
			 * 
			 */
			testStepNumber = "s8";
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 8: DESCRIPTION: Verify whether MAC Filtering mode is configured as 'Allow-All' by setting TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as 'false' using WebPA");
			LOGGER.info("STEP 8: ACTION: Execute the WebPA command");
			LOGGER.info(
					"STEP 8: EXPECTED: Device should set MAC filtering mode as 'Allow All' when we disable the MAC Filter.");
			LOGGER.info("#####################################################################################");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_ENABLE,
					AutomaticsConstants.CONSTANT_3, AutomaticsConstants.FALSE);
			errorMessage = "Unable to Set the MAC Filter as Allow All/Disable by setting false to the  TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' by using Webpa commands.";
			LOGGER.info("STEP 8: ACTUAL : " + (status
					? "Successfully Set the MAC Filter as  Allow-All/disabled by setting the TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as false via webpa"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 9:verify getting the Configured MAC Filter Mode by using this
			 * TR-181parameter
			 * "Device.WiFi.AccessPoint.10101.X_COMCAST-COM_MAC_FilteringMode" .
			 * 
			 */
			testStepNumber = "s9";
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 9: DESCRIPTION: verify getting the Configured MAC Filter Mode by using  this TR-181parameter 'Device.WiFi.AccessPoint.10101.X_COMCAST-COM_MAC_FilteringMode'");
			LOGGER.info("STEP 9: ACTION: Execute the WebPA command");
			LOGGER.info(
					"STEP 9: EXPECTED:The configured MAC Filter mode for the device should be as Allow-All/disabled ");
			LOGGER.info("#####################################################################################");
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_MODE);
			status = response.equalsIgnoreCase(BroadBandTestConstants.MAC_FILTER_ALLOW_ALL);
			errorMessage = "MAC Filter mode is not configured as Allow-All/disabled";
			LOGGER.info("STEP 9: ACTUAL : "
					+ (status ? "MAC Filter mode for the device is configured as Allow-All/disabled" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 10: Connect the device to 5 GHz SSID and verify connection status
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 10: DESCRIPTION: Connect the device to 5 GHz SSID and verify connection status");
			LOGGER.info("STEP 10: ACTION: connect the device with 5 GHz wifi network");
			LOGGER.info("STEP 10: EXPECTED: Device should be connected with 5 GHz wifi network");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s10";
			status = false;
			errorMessage = "Connection to 5Ghz device failed";
			// get all connected devices and connect a device to 5Ghz

			connectedDeviceActivated = BroadBandConnectedClientUtils
					.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			status = (null != connectedDeviceActivated);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			// wait for 1.5 min for connected client to get the connectivity
			tapEnv.waitTill(90000);

			/**
			 * Steps 11 to 14
			 */
			BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, connectedDeviceActivated,
					testId, new String[] { "s11", "s12", "s13", "s14" });

		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, true);
		}
		LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-5011");
	}

	/**
	 * This test case is to Verify the controlled user access (Allow) to the
	 * specific Devices for 2.4 GHZ against different MAC addresses by way of MAC
	 * ACLs
	 * 
	 * <ol>
	 * <li>STEP 1:Getting the Wifi Mac address of Connected client having 2.4GHZ
	 * wifi Capability.</li>
	 * 
	 * <li>STEP 2: Verify and add the device wifi Mac address in the MAC Filter list
	 * by using WEBPA command.</li>
	 * 
	 * <li>STEP 3: Verify whether MAC Filtering mode is configured as 'Allow' by
	 * setting TR-181 parameter
	 * 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as 'true' using
	 * WebPA</li>
	 * 
	 * <li>STEP 4: Verify whether MAC Filtering mode is configured as 'Allow'by
	 * setting of TR-181 parameter
	 * 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList' as
	 * 'false' using WebPA for 2.4GHZ.</li>
	 * 
	 * <li>STEP 5:verify getting the Configured MAC Filter Mode by using this
	 * TR-181parameter
	 * 'Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode'</li>
	 *
	 * <li>STEP 6: Connect the connected client device which is added in the MAC
	 * Filter to 2.4 GHz SSID and verify connection status</li>
	 * 
	 * <li>STEP 7 :Verify whether interface got the correct IPv4 address</li>
	 * 
	 * <li>STEP 8 :Verify whether interface got the correct IPv6 address</li>
	 *
	 * <li>STEP 9 :Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * 
	 * <li>STEP 10 :Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * 
	 * <li>STEP 11: Connect the connected client device whose wifi Mac address is
	 * not added in the MAC Filter to 2.4 GHz SSID and verify connection status</li>
	 * 
	 * <li>STEP 12: Verify whether interface did'nt get the correct IPv4
	 * address.</li>
	 *
	 * <li>STEP 13 : Verify whether interface did'nt get the correct IPv6
	 * address.</li>
	 * 
	 * <li>STEP 14: Verify whether there is no connectivity using that particular
	 * interface using IPV4</li>
	 * 
	 * <li>STEP 15: Verify whether there is no connectivity using that particular
	 * interface using IPV6</li>
	 * </ol>
	 * 
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor Said Hisham
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WIFI-5012")
	public void testVerifyMacFilterAsAllow2_4Ghz(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-512";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// server response
		String response = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;
		// String to store the WIFI MAC address for 2GHZ connected client
		String wifiMacAddress = null;
		// String to store the added table row number
		String tableRowNumber = null;
		// string to store the webpa server response
		WebPaServerResponse webPaServerResponse = null;
		// string to store the SSID name
		String ssidName = null;
		// string to store the password
		String passPhraseName = null;
		// string to store command
		String command = null;
		try {
			LOGGER.info("STARTING TESTCASE : TC-RDKB-WIFI-5012 ");
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"PRE_CONDITION 1: Verify whether Private 2.4 GHz SSID 'Device.WiFi.SSID.10001.Enable' is enabled using WebPA.");
			LOGGER.info("#####################################################################################");
			if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					AutomaticsConstants.CONSTANT_3, AutomaticsConstants.TRUE)) {
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR
						+ "NOT ABLE TO ENABLE THE 2.4GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION");
			}

			/**
			 * STEP 1:Getting the Wifi Mac address of Connected client having 2.4GHZ wifi
			 * Capability.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 1: DESCRIPTION: Getting the Wifi Mac address of Connected client having 2.4GHZ wifi Capability.");
			LOGGER.info("STEP 1: ACTION : Get the Wifi Mac address");
			LOGGER.info(
					"STEP 1: EXPECTED: Device should be able to get the Wifi Mac address of the connected client having 2.4GHZ wifi Capability");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s1";
			status = false;
			errorMessage = "Unable to retrieve the Wifi Mac address of the connected client having 2.4GHZ wifi Capability";
			List<Dut> lockedDevices = ((Device) device).getConnectedDeviceList();
			try {
				connectedDeviceActivated = BroadBandConnectedClientUtils.getConnectedClientBasedOnTypeAndBand(device,
						tapEnv, lockedDevices, BroadBandTestConstants.WIFI, BroadBandTestConstants.BAND_2_4GHZ);
				wifiMacAddress = ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress();
				LOGGER.info("Wifi Mac Address of the Connected client having 2.4GHZ Capability obtained is:-"
						+ wifiMacAddress);
				status = CommonMethods.isNotNull(wifiMacAddress);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}
			LOGGER.info("S1 ACTUAL : " + (status
					? "Successfully retrieved the Wifi Mac address of the connected client having 2.4GHZ wifi Capability "
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2:Verify and add the device wifi Mac address in the MAC Filter list by
			 * using WEBPA command.
			 */
			testStepNumber = "s2";
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verify and add the device wifi Mac address in the MAC Filter list by using WEBPA command.");
			LOGGER.info("STEP 2: ACTION : Add the device wifi Mac address in the MAC Filter list");
			LOGGER.info("STEP 2: EXPECTED : Device should add the device wifi Mac address in the MAC Filter list");
			LOGGER.info("#####################################################################################");
			Map<String, List<String>> macFilterTable = new HashMap<String, List<String>>();
			List<String> MacAddressList = new ArrayList<String>();
			MacAddressList.add(wifiMacAddress);
			List<String> DeviceList = new ArrayList<String>();
			DeviceList.add(BroadBandTestConstants.CONNECTED_DEVICE);
			// adding to the Map.
			macFilterTable.put(BroadBandTestConstants.MAC_ADDRESS, MacAddressList);
			macFilterTable.put(BroadBandTestConstants.DEVICE_NAME, DeviceList);
			errorMessage = "Unable to add the wifi Mac Address to the  MAC Filter filter by Webpa POST command";
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_TABLE,
					macFilterTable);
			if (null != webPaServerResponse) {
				tableRowNumber = webPaServerResponse.getRow();
				status = CommonMethods.isNotNull(webPaServerResponse.getMessage())
						&& webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			LOGGER.info("S2 ACTUAL : " + (status
					? "Successfully added the wifi Mac Address to the  MAC Filter filter by Webpa POST command"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 3:Verify whether MAC Filtering mode is configured as "Allow" by setting
			 * TR-181 parameter "Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable"
			 * as "true" using WebPA for 2.4GHZ.
			 */
			testStepNumber = "s3";
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 3: DESCRIPTION: Verify whether MAC Filtering mode is configured as 'Allow' by setting TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as 'true' using WebPA");
			LOGGER.info("STEP 3: ACTION: Set MAC filtering mode as 'Allow'.");
			LOGGER.info(
					"STEP 3: EXPECTED: Device should set MAC filtering mode as 'Allow' when we enable the MAC Filter.");
			LOGGER.info("#####################################################################################");
			errorMessage = "Unable to Set the MAC Filter as Allow by setting true to the  TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' by using Webpa commands.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_ENABLE,
					AutomaticsConstants.CONSTANT_3, AutomaticsConstants.TRUE);
			LOGGER.info("S3 ACTUAL : " + (status
					? "Successfully Set the MAC Filter as  Allow by setting the TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as true via webpa"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 4: Verify whether MAC Filtering mode is configured as 'Allow'by setting
			 * of TR-181 parameter
			 * 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList' as
			 * 'false' using WebPA for 2.4GHZ.
			 */
			testStepNumber = "s4";
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 4: DESCRIPTION: Verify whether MAC Filtering mode is configured as 'Allow'by  setting of TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList' as 'false' using WebPA for 2.4GHZ.");
			LOGGER.info("STEP 4: ACTION: Set MAC filtering mode as 'Allow'.");
			LOGGER.info(
					"STEP 4: EXPECTED: Device should set 'false' to the TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList'");
			LOGGER.info("#####################################################################################");
			errorMessage = "Unable to Set the  TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList' as false  by using Webpa commands.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_BLACK_LIST,
					AutomaticsConstants.CONSTANT_3, AutomaticsConstants.FALSE);
			LOGGER.info("S4 ACTUAL : " + (status
					? "Successfully Set the TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList' as false via webpa"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 5:verify getting the Configured MAC Filter Mode by using this
			 * TR-181parameter
			 * "Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode" .
			 */
			testStepNumber = "s5";
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 5: DESCRIPTION: verify getting the Configured MAC Filter Mode by using  this TR-181parameter 'Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode'");
			LOGGER.info("STEP 5: ACTION: Verify the configured MAC Filter mode");
			LOGGER.info("STEP 5: EXPECTED: The configured MAC Filter mode for the device should be as Allow ");
			LOGGER.info("#####################################################################################");
			errorMessage = "MAC Filter mode is not configured as Allow";
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_MODE);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.MAC_FILTER_ALLOW);
			LOGGER.info(
					"S5 ACTUAL : " + (status ? "MAC Filter mode for the device is configured as Allow" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 6:Connect the connected client device which is added in the MAC Filter
			 * to 2.4 GHz SSID and verify connection status
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 6: DESCRIPTION: Connect the connected client device which is added in the MAC Filter to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP 6: ACTION: Connect the device with 2.4 GHz wifi network ");
			LOGGER.info(
					"STEP 6: EXPECTED: Device should be connected with 2.4 GHz wifi network since the MAC Filter mode is configured as 'Allow'");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s6";
			status = false;
			errorMessage = "Connection to 2.4Ghz device failed";
			LOGGER.info("Going to wait for 1 minute before connecting the client to the wifi network");
			// wait for 1 min for before connecting to the ssid
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			LOGGER.info(
					"Wifi Mac Address of the Connected client whose wifi Mac address is  added in the MAC Filter is:-"
							+ ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress());
			try {
				ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
						tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				status = ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
						passPhraseName);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			LOGGER.info("S6 ACTUAL : " + (status
					? "connected client device which is added in the MAC Filter is connected to 2.4GHZ wifi network since the MAC Filter mode is configured as 'Allow'"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * Steps 7 to 10
			 */
			LOGGER.info("Going to wait for 1.5 minutes after connecting the client to the wifi network");
			// wait for 1.5 min after connecting
			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
			BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, connectedDeviceActivated,
					testId, new String[] { "s7", "s8", "s9", "s10" });

			/**
			 * STEP 11:Connect the connected client device whose wifi Mac address is not
			 * added in the MAC Filter to 2.4 GHz SSID and verify connection status
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 11: DESCRIPTION: Connect the connected client device whose wifi Mac address is not added in the MAC Filter to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP 11: ACTION: Connect the device with 2.4 GHz wifi network ");
			LOGGER.info(
					"STEP 11: EXPECTED: Device should not be connected with 2.4 GHz wifi network since the MAC Filter mode is configured as 'Allow'");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s11";
			status = false;
			errorMessage = "Connection to 2.4Ghz device is successful even though the MAC Filter mode is configured as 'Allow'";
			// wait for 1 min for before connecting to the ssid
			LOGGER.info("Going to wait for 1 minute before connecting the client to the wifi network");
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			lockedDevices = ((Device) device).getConnectedDeviceList();
			try {
				connectedDeviceActivated = BroadBandConnectedClientUtils.getOtherConnectedClient(
						connectedDeviceActivated, tapEnv, lockedDevices, BroadBandTestConstants.WIFI,
						BroadBandTestConstants.BAND_2_4GHZ);
				LOGGER.info(
						"Wifi Mac Address of the Connected client whose wifi Mac address is not added in the MAC Filter is:-"
								+ ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress());
				ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
						tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				status = !ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
						passPhraseName);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}
			LOGGER.info("S11 ACTUAL : " + (status
					? "connected client device whose wifi Mac address is not added in the MAC Filter is not connected to 2.4GHZ wifi network since the MAC Filter mode is configured as 'Allow'"
					: errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 12:Verify whether interface did'nt get the correct IPv4 address.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 12: DESCRIPTION: Verify whether interface did'nt get the correct IPv4  address.");
			LOGGER.info("STEP 12: ACTION: Get the interface IPv4 address");
			LOGGER.info("STEP 12: EXPECTED:Interface IP address should not be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s12";
			status = false;
			errorMessage = "Interface  got the correct IPV4 address";
			LOGGER.info("Going to wait for 1.5 minutes after connecting the client to the wifi network");
			// wait for 1.5 min after connecting
			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
			String osType = ((Device) connectedDeviceActivated).getOsType();
			status = !BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
					osType, connectedDeviceActivated, tapEnv);
			LOGGER.info("S12 ACTUAL : " + (status ? "Interface did'nt got the correct IPv4  address" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 13:Verify whether interface did'nt get the correct IPv6 address.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 13: DESCRIPTION: Verify whether interface did'nt get the correct IPv6  address.");
			LOGGER.info("STEP 13: ACTION: Get the interface IPv6 address");
			LOGGER.info("STEP 13: EXPECTED:Interface IP address should not be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s13";
			status = false;
			errorMessage = "Interface  got the correct IPV6 address";
			status = !BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
					osType, connectedDeviceActivated, tapEnv);
			LOGGER.info("S13 ACTUAL : " + (status ? "Interface did'nt got the correct IPv6  address" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 14:Verify whether there is no connectivity using that particular
			 * interface using IPV4.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 14: DESCRIPTION: Verify whether there is no connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP 14: ACTION: Get the Connectivity with the interface");
			LOGGER.info("STEP 14: EXPECTED: Connectivity check should'nt  return the status as 200");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s14";
			status = false;
			errorMessage = "Connectivty check using IPV4 address success";
			command = ((Device) connectedDeviceActivated).getOsType()
					.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
							? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
									.replace("<INTERFACE>", BroadbandPropertyFileHandler.getLinuxClientWifiInterface())
							: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
			response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
			status = !(CommonMethods.isNotNull(response)
					&& response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK));
			LOGGER.info("S14 ACTUAL : "
					+ (status ? "connectivity using that particular interface using IPV4 Failed" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 15:Verify whether there is no connectivity using that particular
			 * interface using IPV6.
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 15: DESCRIPTION: Verify whether there is no  connectivity using that particular interface using IPV6");
			LOGGER.info("STEP 15: ACTION: Get the Connectivity with the interface");
			LOGGER.info("STEP 15: EXPECTED: Connectivity check should'nt return status as 200");
			LOGGER.info("#####################################################################################");
			status = false;
			testStepNumber = "s15";
			errorMessage = "Connectivity check using IPV6 address success";
			command = ((Device) connectedDeviceActivated).getOsType()
					.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
							? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV6_ADDRESS
							: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV6_ADDRESS;
			response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
			status = !(CommonMethods.isNotNull(response)
					&& response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK));
			LOGGER.info("S15 ACTUAL : "
					+ (status ? "connectivity using that particular interface using IPV6 Failed" : errorMessage));
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Following exception occured during execution : " + errorMessage);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, true);
		} finally {
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"POST CONDITION 1: DESCRIPTION: Verify whether MAC Filtering mode is configured as 'Allow-All'(default Mode) by setting TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as 'false' using WebPA");
			LOGGER.info(
					"POST CONDITION 1: ACTION: Set TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as 'false' using WebPA");
			LOGGER.info(
					"POST CONDITION 1: EXPECTED:Device should set the  MAC filtering mode as 'Allow All' when we disable the MAC Filter.");
			LOGGER.info("#####################################################################################");
			if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_ENABLE,
					AutomaticsConstants.CONSTANT_3, AutomaticsConstants.FALSE)) {
				LOGGER.error("NOT ABLE TO SET THE MAC FILTERING MODE AS ALLOW-ALL(DEFAULT MODE) USING WEBPA");
			}

			if (null != tableRowNumber) {
				LOGGER.info("#####################################################################################");
				LOGGER.info(
						"POST CONDITION 2: DESCRIPTION: Verify and  delete  the device wifi Mac address in the MAC Filter list by using WEBPA DELETE command.");
				LOGGER.info(
						"POST CONDITION 2: ACTION: Delete the mac address from the MAC Filter list using the WEBPA DELETE command");
				LOGGER.info(
						"POST CONDITION 2: EXPECTED:Should be able to delete the added wifi Mac address in MAC Filter list using webpa Delete command");
				LOGGER.info("#####################################################################################");
				status = false;
				webPaServerResponse = null;
				webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRowNumber);
				if (null != webPaServerResponse) {
					status = CommonMethods.isNotNull(webPaServerResponse.getMessage())
							&& webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
				}
				if (!status) {
					LOGGER.error(
							"NOT ABLE TO DELETE THE ADDED WIFI MAC ADDRESS IN THE MAC FILTER TABLE USING DELETE WEBPA COMMAND");
				}
			}
			LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-5012");
		}
	}

	/**
	 * Test to Verify Security Mode for WiFi access bands (2.4 and 5 GHz)
	 * 
	 * <Steps>
	 * 
	 * Verify default security mode from gateway device using webpa commands for 2.4
	 * and 5 GHz Verify default security mode from client device Change the security
	 * mode to WPA-WPA2-Personal Verify default security mode from gateway device
	 * using webpa commands for 2.4 and 5 GHz Verify default security mode from
	 * client device Change the security mode to Open Verify default security mode
	 * from gateway device using webpa commands for 2.4 and 5 GHz Verify default
	 * security mode from client device
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Gnanaprakasham S
	 * @Refactor Sruthi Santhosh
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI, BroadBandTestGroup.WIFI_SECURITY })
	@TestDetails(testUID = "TC-RDKB-WIFI-4005")
	public void testVerifySecurityModesforWiFiNetworksUsingTR69Params(Dut device) {

		// Variable to store test case ID
		String testCaseId = "TC-RDKB-WIFI-405";
		// Variable to store test step number
		String testStepNumber = "s1";
		// Variable to store execution status
		boolean status = false;
		// Variable to store error Message
		String errorMessage = null;
		// Dut instance to store 2 ghz client device
		Dut clientConnectedWith2Ghz = null;
		// Dut instance to store 5 ghz client device
		Dut clientConnectedWith5Ghz = null;

		String ssidName2Ghz = null;

		String ssidName5Ghz = null;

		try {

			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-4005");

			LOGGER.info("**************************************************************");
			LOGGER.info("TEST DESCRIPTION: Verify Security Mode for Wifi frequency band using tr69 params ");
			LOGGER.info("*************************************************************************");

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: Verify default security mode from gateway device using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" for 2.4 GHz wifi network");
			LOGGER.info("EXPECTED: Default Security mode should be WPA2-Personal for 2.4 GHz");
			LOGGER.info(
					"STEP 2: Verify default security mode from client device using netsh wlan show networks(windows) for 2.4 GHz wifi network");
			LOGGER.info("EXPECTED:  Default Security mode should be WPA2-Personal for 2.4 GHz");
			LOGGER.info(
					"STEP 3: Change the security mode to WPA2-Enterprise mode using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" and verify status for 2.4 GHz wifi network");
			LOGGER.info("EXPECTED: Security mode should be changed to WPA2-Enterprise");
			LOGGER.info(
					"STEP 4: Verify security mode from gateway device using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" for 2.4 GHz wifi network");
			LOGGER.info("EXPECTED: Security mode should be WPA2-Enterprise for 2.4 GHz");
			LOGGER.info(
					"STEP 5: Verify security mode from client device using netsh wlan show networks(windows) for 2.4 GHz wifi network");
			LOGGER.info("EXPECTED: Security mode should be WPA2-Enterprise for 2.4 GHz");
			LOGGER.info(
					"STEP 6: Change the security mode to None mode using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" and verify status for 2.4 GHz wifi network");
			LOGGER.info("EXPECTED: Security mode should be changed to Open");
			LOGGER.info(
					"STEP 7: Verify security mode from gateway device using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" for 2.4 GHz wifi network");
			LOGGER.info("EXPECTED: Security mode should be None for 2.4 GHz wifi network  ");
			LOGGER.info(
					"STEP 8: Verify security mode from client device using netsh wlan show networks(windows) for 2.4 GHz wifi network ");
			LOGGER.info("EXPECTED: Security mode should be Open for 2.4 GHz");

			LOGGER.info(
					"STEP 9: Verify default security mode from gateway device using \"Device.WiFi.AccessPoint.10101.Security.ModeEnabled\" for 5 GHz wifi network");
			LOGGER.info("EXPECTED: Default Security mode should be WPA2-Personal for 5 GHz");
			LOGGER.info(
					"STEP 10: Verify default security mode from client device using netsh wlan show networks(windows) for 5 GHz wifi network");
			LOGGER.info("EXPECTED: Default Security mode should be WPA2-Personal for 5 GHz");
			LOGGER.info(
					"STEP 11: Change the security mode to WPA2-Enterprise mode using \"Device.WiFi.AccessPoint.10101.Security.ModeEnabled\" and verify status for 5 GHz wifi network");
			LOGGER.info("EXPECTED: Security mode should be changed to WPA2-Enterprise for 5 GHz");
			LOGGER.info(
					"STEP 12: Verify security mode from gateway device using \"Device.WiFi.AccessPoint.10101.Security.ModeEnabled\" for 5 GHz wifi network");
			LOGGER.info("EXPECTED: Security mode should be WPA2-Enterprise for for 5 GHz");
			LOGGER.info(
					"STEP 13: Verify security mode from client device using netsh wlan show networks(windows) for 5 GHz wifi network");
			LOGGER.info("EXPECTED: Security mode should be WPA2-Enterprise for for 5 GHz");
			LOGGER.info(
					"STEP 14: Change the security mode to None mode using \"Device.WiFi.AccessPoint.10101.Security.ModeEnabled\" and verify status for 5 GHz wifi network");
			LOGGER.info("EXPECTED: Security mode should be changed to None for 5 GHz");
			LOGGER.info(
					"STEP 15: Verify security mode from gateway device using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" for 2.4 GHz wifi network");
			LOGGER.info("EXPECTED: Security mode should be None for for 5 GHz wifi network  ");
			LOGGER.info(
					"STEP 16: Verify security mode from client device using netsh wlan show networks(windows) for 2.4 GHz wifi network ");
			LOGGER.info("EXPECTED: Security mode should be Open for for 5 GHz");

			LOGGER.info("**********************************************************************************");

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: Verify default security mode from gateway device using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" for 2.4 GHz wifi network");
			LOGGER.info("STEP 1: EXPECTED: Default Security mode should be WPA2-Personal for 2.4 GHz");
			LOGGER.info("**********************************************************************************");
			boolean isWpa2EnterpriseModeApplicable = false;

			isWpa2EnterpriseModeApplicable = DeviceModeHandler.isBusinessClassDevice(device)
					|| DeviceModeHandler.isFibreDevice(device);

			try {

				status = BroadBandConnectedClientUtils.verifyWiFiSecutityModeUsingWebPaOrDmcliCommand(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
						BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);

				errorMessage = status ? "Successfully verified default security mode from gateway device for 2 GHz"
						: "Failed verified default security mode from gateway device for 2 GHz";
				LOGGER.info("STEP 1 :  ACTUAL RESULT : " + errorMessage);

			} catch (Exception exception) {
				errorMessage = errorMessage + "  Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: Verify default security mode from client device using netsh wlan show networks(windows) for 2.4 GHz wifi network");
			LOGGER.info("STEP 2: EXPECTED:  Default Security mode should be WPA2-Personal for 2.4 GHz");

			LOGGER.info("**********************************************************************************");

			errorMessage = "Failed to verify default security mode from connected client device for 2.4 GHz";

			try {

				clientConnectedWith2Ghz = BroadBandConnectedClientUtils.getWindowsClientsAndConnectToGivenSSID(device,
						tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);

				ssidName2Ghz = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_2_GHZ);

				status = BroadBandConnectedClientUtils.verifyWiFiSecurityModeFromClientDeviceUsingSystemCommand(
						clientConnectedWith2Ghz, tapEnv,
						new String[] { BroadBandConnectedClientTestConstants.WINDOWS_COMMAND_TO_GET_WIFI_SECURITY_MODE
								.replaceAll("<ssid>", ssidName2Ghz) },
						BroadBandConnectedClientTestConstants.PATTERN_GET_SECURITY_MODE_2GHZ_CLIENT_DEVICES,
						BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
				errorMessage = status
						? "Successfully verified default security mode from connected client device for 2.4 GHz"
						: "Failed to verify default security mode from connected client device for 2.4 GHz";
				LOGGER.info("STEP 2 :  ACTUAL RESULT : " + errorMessage);

			} catch (Exception exception) {

				errorMessage = errorMessage + " Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s3";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: Change the security mode to WPA2-Enterprise mode using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" and verify status for 2.4 GHz wifi network");
			LOGGER.info("STEP 3: EXPECTED: Security mode should be changed to WPA2-Enterprise");
			LOGGER.info("**********************************************************************************");
			try {
				errorMessage = "Failed to set the security mode as WPA_WAP2_ENTERPRISE for 2.4 GHz";
				if (isWpa2EnterpriseModeApplicable) {
					status = BroadBandConnectedClientUtils.setSecurityModeForWiFiNetwork(tapEnv, device,
							BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE,
							BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ);
					errorMessage = status ? "Successfully updated security mode as WPA_WAP2_ENTERPRISE for 2.4 GHz : "
							: "Failed to set security mode as WAP2_Enterprise for 2.4 GHz : ";
				}
			} catch (Exception exception) {
				errorMessage = "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (isWpa2EnterpriseModeApplicable) {
				tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			} else {
				errorMessage = "WPA-2 Enterprise Mode is not applicable for atom, dsl device models.";
				tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			LOGGER.info("STEP 3 ACTUAL RESULT : " + errorMessage);

			testStepNumber = "s4";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: Verify security mode from gateway device using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" for 2.4 GHz wifi network");
			LOGGER.info("STEP 4: EXPECTED: Security mode should be WPA2-Enterprise for 2.4 GHz");

			LOGGER.info("**********************************************************************************");
			try {
				LOGGER.info("Waiting for TWO minutes to reflect the security mode changes made via WebPA");
				tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);

				errorMessage = "Failed to security mode as WPA-WPA2-Enterprise from gateway device for 2.4 GHz";
				if (isWpa2EnterpriseModeApplicable) {
					status = BroadBandConnectedClientUtils.verifyWiFiSecutityModeUsingWebPaOrDmcliCommand(device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
							BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE);
					errorMessage = status
							? "Successfully verified security mode as WPA2-Enterprise from gateway device for 2.4 GHz: "
							: "Failed to verify security mode as WPA2-Enterprise from gateway device for 2.4 GHz ";
				}
			} catch (Exception exception) {
				errorMessage = errorMessage + " Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (isWpa2EnterpriseModeApplicable) {
				tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			} else {
				errorMessage = "WPA-2 Enterprise Mode is not applicable for atom, dsl device models.";
				tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			LOGGER.info("STEP 4 ACTUAL RESULT : " + errorMessage);

			testStepNumber = "s5";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5: Verify security mode from client device using netsh wlan show networks(windows) for 2.4 GHz wifi network");
			LOGGER.info("STEP 5: EXPECTED: Security mode should be WPA2-Enterprise for 2.4 GHz");

			LOGGER.info("**********************************************************************************");

			try {
				LOGGER.info("Waiting for 2 minutes to reflect the security mode changes in connected clients");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				if (isWpa2EnterpriseModeApplicable) {
					status = BroadBandConnectedClientUtils.verifyWiFiSecurityModeFromClientDeviceUsingSystemCommand(
							clientConnectedWith2Ghz, tapEnv,
							new String[] {
									BroadBandConnectedClientTestConstants.WINDOWS_COMMAND_TO_GET_WIFI_SECURITY_MODE
											.replaceAll("<ssid>", ssidName2Ghz) },
							BroadBandConnectedClientTestConstants.PATTERN_GET_SECURITY_MODE_2GHZ_CLIENT_DEVICES,
							BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE);
					errorMessage = status
							? "Successfully verified security mode as WPA2-Enterprise from 2.4 GHz connected client device"
							: "Failed to verify security mode as WPA2-Enterprise from 2.4 GHz connected client device";
				}
			} catch (Exception exception) {
				errorMessage = errorMessage + " Execption occured during execution => " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (isWpa2EnterpriseModeApplicable) {
				tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			} else {
				errorMessage = "WPA-2 Enterprise Mode is not applicable for atom, dsl device models.";
				tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			LOGGER.info("STEP 5 ACTUAL RESULT : " + errorMessage);

			testStepNumber = "s6";
			status = false;
			LOGGER.info(
					"*****************************************************************************************************************************************************************");
			LOGGER.info(
					"STEP 6: Change the security mode to None mode using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" and verify status for 2.4 GHz wifi network");
			LOGGER.info("STEP 6: EXPECTED: Security mode should be changed to Open");

			LOGGER.info(
					"*****************************************************************************************************************************************************************");

			try {
				errorMessage = "Failed to set the security mode as Open for 2 GHz ";
				status = BroadBandConnectedClientUtils.setSecurityModeForWiFiNetwork(tapEnv, device,
						BroadBandTestConstants.SECURITY_MODE_NONE,
						BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES,
						WiFiFrequencyBand.WIFI_BAND_2_GHZ);

				errorMessage = status ? "Successfully changed security mode as None for 2 GHz  "
						: "Failed to change security mode as None for 2 GHz  ";
				LOGGER.info("STEP 6 : ACTUAL RESULT : " + errorMessage);

			} catch (Exception exception) {
				errorMessage = errorMessage + " Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s7";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 7: Verify security mode from gateway device using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" for 2.4 GHz wifi network");
			LOGGER.info("STEP 7: EXPECTED: Security mode should be None for 2.4 GHz wifi network  ");

			LOGGER.info("**********************************************************************************");

			try {

				LOGGER.info("Waiting for THREE minutes to reflect the security mode changes made via WebPA");
				tapEnv.waitTill(AutomaticsConstants.THREE_MINUTES);

				status = BroadBandConnectedClientUtils.verifyWiFiSecutityModeUsingWebPaOrDmcliCommand(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
						BroadBandTestConstants.SECURITY_MODE_NONE);
				errorMessage = status ? "Successfully verified security mode as None from gateway device  for 2.4 GHz"
						: "Failed to verify security mode as None from gateway device  for 2.4 GHz";
				LOGGER.info("STEP 7 : ACTUAL RESULT : " + errorMessage);

			} catch (Exception exception) {
				errorMessage = errorMessage + "  Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s8";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 8: Verify security mode from client device using netsh wlan show networks(windows) for 2.4 GHz wifi network ");
			LOGGER.info("STEP 8: EXPECTED: Security mode should be Open for 2.4 GHz");
			LOGGER.info("**********************************************************************************");

			try {

				LOGGER.info("Waiting for 2 minutes to reflect the security mode changes in connected clients");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);

				status = BroadBandConnectedClientUtils.verifyWiFiSecurityModeFromClientDeviceUsingSystemCommand(
						clientConnectedWith2Ghz, tapEnv,
						new String[] { BroadBandConnectedClientTestConstants.WINDOWS_COMMAND_TO_GET_WIFI_SECURITY_MODE
								.replaceAll("<ssid>", ssidName2Ghz) },
						BroadBandConnectedClientTestConstants.PATTERN_GET_SECURITY_MODE_2GHZ_CLIENT_DEVICES,
						BroadBandConnectedClientTestConstants.SECURITY_MODE_OPEN);
				errorMessage = status
						? "Successfully verified security mode as none from connected client device for 2.4 GHz"
						: "Failed to verify security mode as none from 2Ghz connected client device";
				LOGGER.info("STEP 8 : ACTUAL RESULT : " + errorMessage);

			} catch (Exception exception) {
				errorMessage = errorMessage + "  Execption occured during execution => " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			testStepNumber = "s9";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 9: Verify default security mode from gateway device using \"Device.WiFi.AccessPoint.10101.Security.ModeEnabled\" for 5 GHz wifi network");
			LOGGER.info("STEP 9: EXPECTED: Default Security mode should be WPA2-Personal for 5 GHz");

			LOGGER.info("**********************************************************************************");

			try {
				errorMessage = "Failed to verify default security mode from gateway device for 5 GHz";
				status = BroadBandConnectedClientUtils.verifyWiFiSecutityModeUsingWebPaOrDmcliCommand(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
						BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);

				errorMessage = status ? "Successfully verified default security mode from gateway device for 5 GHz"
						: "Failed verified default security mode from gateway device for 5 GHz";
				LOGGER.info("STEP 9 :  ACTUAL RESULT : " + errorMessage);

			} catch (Exception exception) {
				errorMessage = errorMessage + "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s10";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 10: Verify default security mode from client device using netsh wlan show networks(windows) for 5 GHz wifi network");
			LOGGER.info("STEP 10: EXPECTED: Default Security mode should be WPA2-Personal for 5 GHz");

			LOGGER.info("**********************************************************************************");

			ssidName5Ghz = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);

			clientConnectedWith5Ghz = BroadBandConnectedClientUtils.getWindowsClientsAndConnectToGivenSSID(device,
					tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			LOGGER.info("Obtained MAC address for 5 GHz device : " + clientConnectedWith5Ghz.getHostMacAddress());

			try {

				status = BroadBandConnectedClientUtils.verifyWiFiSecurityModeFromClientDeviceUsingSystemCommand(
						clientConnectedWith5Ghz, tapEnv,
						new String[] { BroadBandConnectedClientTestConstants.WINDOWS_COMMAND_TO_GET_WIFI_SECURITY_MODE
								.replaceAll("<ssid>", ssidName5Ghz) },
						BroadBandConnectedClientTestConstants.PATTERN_GET_SECURITY_MODE_5GHZ_CLIENT_DEVICES,
						BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
				errorMessage = status
						? "Successfully verified default security mode from connected client device for 5 GHz"
						: "Failed to verify default security mode from connected client device for 5 GHz";
				LOGGER.info("STEP 10 :  ACTUAL RESULT : " + errorMessage);

			} catch (Exception exception) {
				errorMessage = errorMessage + "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			testStepNumber = "s11";
			status = false;

			LOGGER.info(
					"*****************************************************************************************************************************************************************");
			LOGGER.info(
					"STEP 11: Change the security mode to WPA2-Enterprise mode using \"Device.WiFi.AccessPoint.10101.Security.ModeEnabled\" and verify status for 5 GHz wifi network");
			LOGGER.info("STEP 11: EXPECTED: Security mode should be changed to WPA2-Enterprise for 5 GHz");
			LOGGER.info(
					"******************************************************************************************************************************************************************");
			try {
				if (isWpa2EnterpriseModeApplicable) {
					errorMessage = "Failed to set the security mode as WAP2_Enterprise for 5 GHz ";
					status = BroadBandConnectedClientUtils.setSecurityModeForWiFiNetwork(tapEnv, device,

							BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE,

							BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ);

					errorMessage = status ? "Successfully updated security mode as WPA_WAP2_ENTERPRISE for 5 GHz : "
							: "Failed to set security mode as WAP2_Enterprise for 5 GHz : ";
				}

			} catch (Exception exception) {
				errorMessage = errorMessage + "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (isWpa2EnterpriseModeApplicable) {
				tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			} else {
				errorMessage = "WPA-2 Enterprise Mode is not applicable for atom, dsl device models.";
				tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			LOGGER.info("STEP 11 ACTUAL RESULT : " + errorMessage);

			testStepNumber = "s12";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 12: Verify security mode from gateway device using \"Device.WiFi.AccessPoint.10101.Security.ModeEnabled\" for 5 GHz wifi network");
			LOGGER.info("STEP 12: EXPECTED: Security mode should be WPA2-Enterprise for for 5 GHz");

			LOGGER.info("**********************************************************************************");

			try {
				LOGGER.info("Waiting for TWO minutes to reflect the security mode changes made via WebPA");
				tapEnv.waitTill(RDKBTestConstants.TWO_MINUTES_IN_SECONDS);

				if (isWpa2EnterpriseModeApplicable) {
					errorMessage = "Failed to verify security mode as WPA-WPA2-Enterprise from gateway device for 5 GHz";
					status = BroadBandConnectedClientUtils.verifyWiFiSecutityModeUsingWebPaOrDmcliCommand(device,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
							BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE);
					errorMessage = status
							? "Successfully verified security mode as WPA2-Enterprise from gateway device for 5 GHz: "
							: "Failed to verify security mode as WPA2-Enterprise from gateway device for 5 GHz ";
				}
			} catch (Exception exception) {
				errorMessage = errorMessage + "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			if (isWpa2EnterpriseModeApplicable) {
				tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			} else {
				errorMessage = "WPA-2 Enterprise Mode is not applicable for atom, dsl device models.";
				tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			LOGGER.info("STEP 12 ACTUAL RESULT : " + errorMessage);

			testStepNumber = "s13";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 13: Verify security mode from client device using netsh wlan show networks(windows) for 5 GHz wifi network");
			LOGGER.info("STEP 13: EXPECTED: Security mode should be WPA2-Enterprise for for 5 GHz");

			LOGGER.info("**********************************************************************************");

			try {
				LOGGER.info(
						"Waiting for TWO minutes to reflect the security mode changes reflected in Connected clients");
				tapEnv.waitTill(RDKBTestConstants.TWO_MINUTES_IN_SECONDS);
				if (isWpa2EnterpriseModeApplicable) {
					status = BroadBandConnectedClientUtils.verifyWiFiSecurityModeFromClientDeviceUsingSystemCommand(
							clientConnectedWith5Ghz, tapEnv,
							new String[] {
									BroadBandConnectedClientTestConstants.WINDOWS_COMMAND_TO_GET_WIFI_SECURITY_MODE
											.replaceAll("<ssid>", ssidName5Ghz) },
							BroadBandConnectedClientTestConstants.PATTERN_GET_SECURITY_MODE_5GHZ_CLIENT_DEVICES,
							BroadBandTestConstants.SECURITY_MODE_WPA2_ENTERPRISE);
					errorMessage = status
							? "Successfully verified security mode as WPA2-Enterprise from 5 GHz connected client device"
							: "Failed to verify security mode as WPA2-Enterprise from 5 GHz connected client device";
				}
			} catch (Exception exception) {
				errorMessage = errorMessage + "Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (isWpa2EnterpriseModeApplicable) {
				tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			} else {
				errorMessage = "WPA-2 Enterprise Mode is not applicable for atom, dsl device models.";
				tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			LOGGER.info("STEP 13: ACTUAL RESULT : " + errorMessage);

			testStepNumber = "s14";
			status = false;
			LOGGER.info(
					"*****************************************************************************************************************************************************************");
			LOGGER.info(
					"STEP 14: Change the security mode to None mode using \"Device.WiFi.AccessPoint.10101.Security.ModeEnabled\" and verify status for 5 GHz wifi network");
			LOGGER.info("STEP 14: EXPECTED: Security mode should be changed to None for 5 GHz");
			LOGGER.info(
					"*****************************************************************************************************************************************************************");

			try {
				errorMessage = "Failed to set the security mode as Open for 5 GHz ";
				status = BroadBandConnectedClientUtils.setSecurityModeForWiFiNetwork(tapEnv, device,

						BroadBandTestConstants.SECURITY_MODE_NONE,

						BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES,
						WiFiFrequencyBand.WIFI_BAND_5_GHZ);

				errorMessage = status ? "Successfully changed security mode as None for 5 GHz  "
						: "Failed to change security mode as None for 5 GHz  ";
				LOGGER.info("STEP 14 : ACTUAL RESULT : " + errorMessage);

			} catch (Exception exception) {
				errorMessage = errorMessage + " Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s15";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 15: Verify security mode from gateway device using \"Device.WiFi.AccessPoint.10001.Security.ModeEnabled\" for 5 GHz wifi network");
			LOGGER.info("STEP 15: EXPECTED: Security mode should be None for for 5 GHz wifi network  ");

			LOGGER.info("**********************************************************************************");

			try {

				LOGGER.info("Waiting for THREE minutes to reflect the security mode changes via webPA");
				tapEnv.waitTill(AutomaticsConstants.THREE_MINUTES);

				errorMessage = "Failed to security mode as None from gateway device for 5 GHz";
				status = BroadBandConnectedClientUtils.verifyWiFiSecutityModeUsingWebPaOrDmcliCommand(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
						BroadBandTestConstants.SECURITY_MODE_NONE);
				errorMessage = status ? "Successfully verified security mode as None from gateway device  for 5 GHz"
						: "Failed to verify security mode as None from gateway device  for 5 GHz";
				LOGGER.info("STEP 15 : ACTUAL RESULT : " + errorMessage);
			} catch (Exception exception) {
				errorMessage = errorMessage + " Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s16";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 16: Verify security mode from client device using netsh wlan show networks(windows) for 5 GHz wifi network ");
			LOGGER.info("STEP 16: EXPECTED: Security mode should be Open for for 5 GHz");

			LOGGER.info("**********************************************************************************");
			try {

				LOGGER.info(
						"Waiting for TWO minutes to reflect the security mode changes reflected in Connected clients");
				tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);

				status = BroadBandConnectedClientUtils.verifyWiFiSecurityModeFromClientDeviceUsingSystemCommand(
						clientConnectedWith5Ghz, tapEnv,
						new String[] { BroadBandConnectedClientTestConstants.WINDOWS_COMMAND_TO_GET_WIFI_SECURITY_MODE
								.replaceAll("<ssid>", ssidName5Ghz) },
						BroadBandConnectedClientTestConstants.PATTERN_GET_SECURITY_MODE_5GHZ_CLIENT_DEVICES,
						BroadBandConnectedClientTestConstants.SECURITY_MODE_OPEN);
				errorMessage = status
						? "Successfully verified security mode as none from connected client device for 5 GHz"
						: "Failed to verify security mode as none from 5 Ghz connected client device";
				LOGGER.info("STEP 16 : ACTUAL RESULT : " + errorMessage);

			} catch (Exception exception) {
				errorMessage = errorMessage + " Exception occurred during execution : " + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);
		} catch (Exception exception) {
			LOGGER.error("Exception occurred during execution : " + exception.getMessage());
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);
		} finally {

			BroadBandConnectedClientUtils.setSecurityModeForWiFiNetwork(tapEnv, device,

					BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL,

					BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES,
					WiFiFrequencyBand.WIFI_BAND_2_GHZ);

			// wait for 1.5 minutes to complete webpa chanages
			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

			BroadBandConnectedClientUtils.setSecurityModeForWiFiNetwork(tapEnv, device,

					BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL,

					BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			// wait for 1.5 minutes to complete webpa chanages
			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

			LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-4005");
		}

	}

	/**
	 * Test case is created to check the ability to change mode from g-n to n-only
	 * mode on the 2.4 GHz radio
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Step 1: Check the operating mode for 2.4GHz Wi-Fi from RDKB device</li>
	 * <li>Step 2: Check the mode of operation in 2.4 Ghz client</li>
	 * <li>Step 3: Change the mode of operation in 2.4GHz client from 802.11 g/n to
	 * 802.11 n and verify the status</li>
	 * <li>Step 4: Check 2.4 Ghz client is connected and IPV4 address obtained</li>
	 * <li>Step 5: Check the mode of operation in 2.4 Ghz client</li>
	 * <li>Step 6: Verify the mode of operation in 2.4 Ghz client</li>
	 * </ol>
	 * 
	 * @author anandam.s
	 * @refactor Alan_Bivera
	 * 
	 * @param device {@link Dut}
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-3002")
	public void testVerifyChangeOperatingModeFor2GHz(Dut device) {

		// string variable to store test case id
		String testCaseId = "TC-RDKB-WIFI-WEBPA-302";
		// Boolean variable to store the status of each step
		boolean status = false;
		// String variable to store the errorMessage in each step
		String errorMessage = null;
		// String variable to store the step number
		String step = "s1";
		// String variable to store the command to be executed in command
		Dut connectedClientDevice = null;
		// variable to store the ostype of connected client
		String clientOsType = null;
		BroadBandResultObject resultObject = null;
		// Variable to store default operating Standard
		String operatingStandard = null;

		try {
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-WEBPA-3002");
			LOGGER.info("**************************************************************");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the ability to change mode from g-n to n-only mode on the 2.4 GHz radio");
			LOGGER.info("*************************************************************************");

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: Verify default operating mode from gateway device for 2.4 GHz wifi network");
			LOGGER.info("STEP 2: Verify 2.4 Ghz client is connected and IPV4 address obtained");
			LOGGER.info("STEP 3: Verify default operating mode from client device for 2.4 GHz wifi network");
			LOGGER.info("STEP 4: Change the operating mode to (n or g/n) mode");
			LOGGER.info("STEP 5: Verify 2.4 Ghz client is connected and IPV4 address obtained");
			LOGGER.info("STEP 6: Verify the mode of operation in 2.4 Ghz client ");
			LOGGER.info("**********************************************************************************");

			/*
			 * Step1 : Verify the operating mode for 2.4GHz WiFi from RDKB device
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Verify default operating mode from gateway device for 2.4 GHz wifi network.");
			LOGGER.info("STEP 1: ACTION : Execute Webpa Parameter-Device.WiFi.Radio.10000.OperatingStandards");
			LOGGER.info(
					"STEP 1: EXPECTED : Operating mode should be 802.11g/n; 802.11b/g/n for DSL device; 802.11g/n/ax for others.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "default value obtained for operating standard in 2.4Ghz band is NOT \'g/n\' in RDKB device or not b/g/n for DSL or not g/n/ax for others";

			if (DeviceModeHandler.isDSLDevice(device)) {
				operatingStandard = WifiOperatingStandard.OPERATING_STANDARD_B_G_N.getOperatingmode();

			} else {
				operatingStandard = WifiOperatingStandard.OPERATING_STANDARD_G_N_AX.getOperatingmode();
			}

			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
					operatingStandard);
			// for including devices working in g,n operating standard
			if (!status) {
				operatingStandard = WifiOperatingStandard.OPERATING_STANDARD_G_N.getOperatingmode();

				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
						operatingStandard);
			}
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : The operating mode for 2.4GHz network in RDKB device is g/n or b/g/n for DSL device or g/n/ax for others");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

			/**
			 * Step2 : Check 2.4 GHz client is connected and IPV4 address obtained
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verify 2.4 Ghz client is connected and IPV4 address obtained with default operating standard or mode (g,n,ax)");
			LOGGER.info("STEP 2: ACTION : Execute command to connect 2.4 GHz client");
			LOGGER.info("STEP 2: EXPECTED :IPV4 address should be obtained and able to access the internet .");
			LOGGER.info("**********************************************************************************");
			step = "s2";
			status = false;
			List<Dut> settopList = ((Device) device).getConnectedDeviceList();
			LOGGER.debug("Dut List: " + settopList.size());

			if (null != settopList && !settopList.isEmpty()) {
				connectedClientDevice = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
				// wait after connecting a client
				LOGGER.info("waiting for 30 seconds ");
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

				if (connectedClientDevice != null) {
					clientOsType = ((Device) connectedClientDevice).getOsType();
					errorMessage = "Client device is NOT connected properly over 2.4Ghz wifi after changing the operating mode in gateway -";
					// Verify the pattern of IPv4 address in Windows and Linux os
					try {
						status = BroadBandConnectedClientUtils
								.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(clientOsType,
										connectedClientDevice, tapEnv)
								&& checkConnectivityOfDevice(connectedClientDevice);
					} catch (TestException exception) {
						errorMessage = errorMessage + exception.getMessage();
						LOGGER.error(errorMessage);
					}
					if (status) {
						LOGGER.info(
								"STEP 2: ACTUAL :2.4 Ghz client is connected and IPV4 address obtained with default operating standard or mode (g,n,ax)");
					} else {
						LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
					}
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

					/**
					 * Step3 : Verify the operating mode for 2.4GHz WiFi from client device
					 */

					LOGGER.info("**********************************************************************************");
					LOGGER.info(
							"STEP 3: DESCRIPTION :Verify default operating mode from client device for 2.4 GHz wifi network");
					LOGGER.info("STEP 3: ACTION : Execute command to get Operating mode");
					LOGGER.info(
							"STEP 3: EXPECTED :Operating mode should be 802.11g or 802.11n and 802.11b/g/n for DSL and 802.11g/n/ax for others.");
					LOGGER.info("**********************************************************************************");

					step = "s3";
					status = false;
					List<String> clientOperatingMode = null;
					errorMessage = "The default value for operating mode in 2.4GHz network for client device is NOT 802.11g or 802.11n and not 802.11g or 802.11n or 802.11ax";
					try {
						if (DeviceModeHandler.isDSLDevice(device)) {
							clientOperatingMode = WifiOperatingStandard.OPERATING_STANDARD_B_G_N
									.getClientOperatingMode();
						} else {
							clientOperatingMode = WifiOperatingStandard.OPERATING_STANDARD_G_N_AX
									.getClientOperatingMode();
						}
						status = BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
								connectedClientDevice, tapEnv, clientOperatingMode);
					} catch (TestException exception) {
						errorMessage = exception.getMessage();
						LOGGER.error(errorMessage);
					}
					if (status) {
						LOGGER.info(
								"STEP 3: ACTUAL :The operating mode for 2.4GHz network in client device is g/n or b/g/n for DSL or g/n/ax for others ");
					} else {
						LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
					}
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

					/**
					 * Step4 : Change the mode of operation in 2.4GHz client to 802.11 n
					 */

					LOGGER.info("**********************************************************************************");
					LOGGER.info(
							"STEP 4: DESCRIPTION :Change the operating mode to n-only and g/n mode in gateway using WebPA - Device.WiFi.Radio.10000.OperatingStandards");
					LOGGER.info("STEP 4: ACTION : Execute command to set Operating mode");
					LOGGER.info(
							"STEP 4: EXPECTED :The operating mode should change from g/n to n-only and g/n/ax to g/n");
					LOGGER.info("**********************************************************************************");
					step = "s4";
					status = false;
					if (!DeviceModeHandler.isDSLDevice(device)) {
						errorMessage = "Failed to change the operating mode for 2.4GHz network of gateway from g/n to n-only and g/n/ax to g/n mode using WebPA - Device.WiFi.Radio.10000.OperatingStandards";

						if (DeviceModeHandler.isDSLDevice(device)) {
							operatingStandard = WifiOperatingStandard.OPERATING_STANDARD_N.getOperatingmode();

						} else {
							operatingStandard = WifiOperatingStandard.OPERATING_STANDARD_G_N.getOperatingmode();
						}
						status = BroadBandWiFiUtils.setWebPaParams(device,
								BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
								operatingStandard, WebPaDataTypes.STRING.getValue());
						LOGGER.info("Actual: " + (status
								? "Changed the operating mode for 2.4GHz network from g/n to n using WebPA - Device.WiFi.Radio.10000.OperatingStandards."
								: errorMessage));
						tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

						// Wait for client to connect to gateway after changing the operating standard
						LOGGER.info(
								"waiting for 90 seconds after changing the operating standard for client to change its operating standard");
						if (status) {
							LOGGER.info("STEP 4: ACTUAL :The operating mode for 2.4GHz network is set successfully  ");
						} else {
							LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
						}
						LOGGER.info(
								"**********************************************************************************");
						tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
					} else {
						LOGGER.info(
								"**********************************************************************************");
						tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
								BroadBandTestConstants.NA_MSG_FOR_DSL_DEVICES, false);
					}

					/**
					 * Step 5 : Connect to 2.4 GHz client and verify whether device is connected and
					 * IPV4 address obtained
					 */
					LOGGER.info("**********************************************************************************");
					LOGGER.info(
							"STEP 5: DESCRIPTION :Verify 2.4 Ghz client is connected and IPV4 address obtained after changing the operating standard to 'n only and to g/n'");
					LOGGER.info("STEP 5: ACTION : Execute command to connect 2.4 Ghz client and verify IPV4 address");
					LOGGER.info("STEP 5: EXPECTED :IPV4 address should be obtained");
					LOGGER.info("**********************************************************************************");
					step = "s5";
					status = false;
					if (!DeviceModeHandler.isDSLDevice(device)) {
						errorMessage = "Client device is NOT connected properly over 2.4Ghz wifi after changing the operating mode in gateway -";
						try {
							resultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device,
									tapEnv, connectedClientDevice, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
							if (resultObject.isStatus()) {
								status = BroadBandConnectedClientUtils
										.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(clientOsType,
												connectedClientDevice, tapEnv)
										&& checkConnectivityOfDevice(connectedClientDevice);
							} else {
								errorMessage = "Failed to Connected client device after change in operating standard with message -"
										+ resultObject.getErrorMessage();
								LOGGER.error(errorMessage);
							}
						} catch (TestException exception) {
							errorMessage = errorMessage + exception.getMessage();
							LOGGER.error(errorMessage);
						}
						tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
						// Verify the pattern of IPv4 address in Windows and Linux os
						try {
							status = BroadBandConnectedClientUtils
									.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(clientOsType,
											connectedClientDevice, tapEnv)
									&& checkConnectivityOfDevice(connectedClientDevice);
						} catch (TestException exception) {
							errorMessage = errorMessage + exception.getMessage();
							LOGGER.error(errorMessage);
						}
						if (status) {
							LOGGER.info(
									"STEP 5: ACTUAL : 2.4 Ghz client is connected in n mode or g/n mode and IPV4 address obtained");
						} else {
							LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
						}
						LOGGER.info(
								"**********************************************************************************");
						tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
					} else {
						LOGGER.info(
								"**********************************************************************************");
						tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
								BroadBandTestConstants.NA_MSG_FOR_DSL_DEVICES, false);
					}

					/**
					 * Step6 : Check the mode of operation in 2.4 Ghz client
					 */
					LOGGER.info("**********************************************************************************");
					LOGGER.info("STEP 6: DESCRIPTION :Verify the mode of operation in 2.4 Ghz client");
					LOGGER.info("STEP 6: ACTION : Execute command to verify 2.4 Ghz mode of operation");
					LOGGER.info("STEP 6: EXPECTED :The mode of operation should be n-only or g/n");
					LOGGER.info("**********************************************************************************");
					step = "s6";
					status = false;
					if (!DeviceModeHandler.isDSLDevice(device)) {
						errorMessage = "The operating mode for 2.4GHz network is not changed from \'g/n\' to \'n-only' mode or from \'g/n/ax\' to \'g/n\' mode in client device";
						try {

							if (DeviceModeHandler.isDSLDevice(device)) {
								clientOperatingMode = WifiOperatingStandard.OPERATING_STANDARD_N
										.getClientOperatingMode();
							} else {
								clientOperatingMode = WifiOperatingStandard.OPERATING_STANDARD_G_N
										.getClientOperatingMode();
							}

							status = BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
									connectedClientDevice, tapEnv, clientOperatingMode);
						} catch (TestException exception) {
							errorMessage = exception.getMessage();
							LOGGER.error(errorMessage);
						}
						if (status) {
							LOGGER.info(
									"STEP 6: ACTUAL :The operating mode for 2.4GHz network is n only mode or g/n mode ");
						} else {
							LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
						}
						LOGGER.info(
								"**********************************************************************************");
						tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
					} else {
						LOGGER.info(
								"**********************************************************************************");
						tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
								BroadBandTestConstants.NA_MSG_FOR_DSL_DEVICES, false);
					}

				} else {
					errorMessage = "Failed to find device with 2.4GHz Wifi support";
					LOGGER.error(errorMessage);
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
				}
			} else {
				errorMessage = "RDKB device does not have any connected client devices";
				LOGGER.error(errorMessage);
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
			}

		} catch (TestException exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"Exception occured while changing the operating mode from g-n to n-only mode for 2.4 GHz network using WebPA - Device.WiFi.Radio.10000.OperatingStandards"
							+ errorMessage);
			LOGGER.info("**********************************************************************************");
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
		} finally {
			// Set the operating mode of 2.4GHz radio to default value 802.11g/n
			// using WebPA - Device.WiFi.Radio.10000.OperatingStandards
			if (!DeviceModeHandler.isDSLDevice(device)) {
				status = BroadBandWiFiUtils.setWebPaParams(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
						operatingStandard, WebPaDataTypes.STRING.getValue());
				// wait for 1.5 minutes to complete webpa chanages
				tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
				LOGGER.info("Finally : " + (status
						? "Changed the operating mode for 2.4GHz network from g/n to n using WebPA - Device.WiFi.Radio.10000.OperatingStandards."
						: "Operating mode is not changed to default value using WebPA - Device.WiFi.Radio.10000.OperatingStandards"));
			}
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-WEBPA-3002");
	}

	/**
	 * Method to check the connectivity of device over wlan
	 * 
	 * @param Dut device
	 * 
	 * @return boolean connection status
	 * @refactor Alan_Bivera
	 */

	private boolean checkConnectivityOfDevice(Dut device) {

		LOGGER.debug("Entering checkConnectivityOfDevice");
		String commandResponse = null;
		String command = ((Device) device).getOsType().equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
				? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS.replace("<INTERFACE>",
						BroadbandPropertyFileHandler.getLinuxClientWifiInterface())
				: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
		commandResponse = tapEnv.executeCommandOnOneIPClients(device, command);
		LOGGER.info("Curl response from device - " + commandResponse);

		if ((CommonMethods.isNotNull(commandResponse) && commandResponse.contains("200 OK"))) {
			LOGGER.info("Obtained 200 ok message on checking connectivity");
			LOGGER.debug("Ending heckConnectivityOfDevice");
			return true;
		} else {
			throw new TestException("Device failed connectivity test using curl command");
		}
	}

	/**
	 * Validate CPU & Memory usage for Maximum Tx&Rx rate
	 * 
	 * <li>1. Disable vAP stats via WebPa</li>
	 * <li>2. Collect CPU and memory usage stats for 10 minutes when feature is
	 * disabled</li>
	 * <li>3. Enable vAP stats via WebPa</li>
	 * <li>4. Validate if Telemetry log interval can be configured</li>
	 * <li>5. Validate if /rdklogs/logs/wifihealth.txt file is present</li>
	 * <li>6. Customize the private WiFi SSID and password</li>
	 * <li>7. Connect clients to 2.4 GHz SSID and 5GHz SSID and verify connection
	 * status</li>
	 * <li>8. Validate the max TX, RX rate for private wifi clients</li>
	 * <li>9. Collect CPU and memory usage stats for 10 minutes when feature is
	 * enabled</li>
	 * <li>10. Compare the results from Step 2 and Step 9</li>
	 * 
	 * @author ArunKumar Jayachandran
	 * @refactor Athira
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			TestGroup.NEW_FEATURE, BroadBandTestGroup.TELEMETRY })
	@TestDetails(testUID = "TC-RDKB-WIFI-TELEMETRY-1004")
	public void testToVerifyCpuUsageForMaxTxRxRate(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1004");
		LOGGER.info("TEST DESCRIPTION: Test to verify CPU usage for Maximum TX_RATE, RX_RATE per client ");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Disable vAP stats via WebPa");
		LOGGER.info("2. Collect CPU and memory usage stats for 10 minutes when feature is disabled");
		LOGGER.info("3. Enable vAP stats via WebPa");
		LOGGER.info("4. Validate if Telemetry log interval can be configured");
		LOGGER.info("5. Validate if /rdklogs/logs/wifihealth.txt file is present");
		LOGGER.info("6. Customize the private WiFi SSID and password");
		LOGGER.info("7. Connect clients to 2.4 GHz SSID and 5GHz SSID and  verify connection status");
		LOGGER.info("8. Validate the max TX, RX rate for private wifi clients");
		LOGGER.info("9. Collect CPU and memory usage stats for 10 minutes when feature is enabled");
		LOGGER.info("10. Compare the results from Step 2 and Step 9");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-WIFI-TELEMETRY-004";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store response
		String response = null;
		// String to store CPU usage before enabling feature
		String beforeEnablingFeature = null;
		// String to store CPU usage after enabling feature
		String afterEnablingFeature = null;
		Dut connectedClient2GHzDut = null; // connected device to be verified
		Dut connectedClient5GHzDut = null; // connected device to be verified
		// variable declaration ends

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("PRE-CONDITION1 :DESCRIPTION : Validate whether the required type and "
					+ "No of clients are available to proceed ");
			LOGGER.info("PRE-CONDITION1 :ACTION : Get the no of client devices and validate the type ");
			LOGGER.info("PRE-CONDITION1 : EXPECTED : Atleast two windows clients should be available ");

			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			stepNumber = "s1";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: Disable the vAPStatsEnable parameter using webpa");
			LOGGER.info(
					"STEP 1: ACTION: Execute webpa set command: Device.WiFi.X_RDKCENTRAL-COM_vAPStatsEnable as false");
			LOGGER.info("STEP 1: EXPECTED: Should be disabled for vAPStatsEnable parameter");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the parameter Device.WiFi.X_RDKCENTRAL-COM_vAPStatsEnable as false";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_VAP_STATS_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL: Successfully updated vAPstatsEnable parameter as false using webpa ");
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Calculate the average CPU and memory utilisation for 10 minutes"
					+ " with client stats telemetry disabled");
			LOGGER.info("STEP 2: ACTION : execute the following command every one minute for 10 minutes and"
					+ " calculate the average");
			LOGGER.info("STEP 2: EXPECTED : Command execution on the device should be successful");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Calculating the average CPU and Memory utilisation for 10 minutes"
					+ " with client stats telemetry disabled has failed";
			response = BroadBandWifiWhixUtils.calculateAverageCpuAndMemoryUtilisation(device, tapEnv,
					BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			LOGGER.info("STEP 2: Response for CPU and memory utilisation: " + response);
			status = CommonMethods.isNotNull(response);
			if (status) {
				beforeEnablingFeature = response;
				LOGGER.info("STEP 2: ACTUAL : Calculating the average CPU and Memory utilisation for 10 minutes"
						+ " with client stats telemetry disabled is successful");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s3";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION: Enable the vAPStatsEnable parameter using webpa");
			LOGGER.info(
					"STEP 3: ACTION: Execute webpa set command: Device.WiFi.X_RDKCENTRAL-COM_vAPStatsEnable as true");
			LOGGER.info("STEP 3: EXPECTED: Should be enabled for vAPStatsEnable parameter");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the parameter Device.WiFi.X_RDKCENTRAL-COM_vAPStatsEnable as true";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_VAP_STATS_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 3: ACTUAL: Successfully updated vAPstatsEnable parameter as true using webpa ");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s4";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Validate if Telemetry log interval can be configured ");
			LOGGER.info("STEP 4: ACTION : Execute WebPa SET command on the object "
					+ "Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval");
			LOGGER.info("STEP 4: EXPECTED : The log interval should get modified to 300 seconds successfully");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Attempt to configure wifi telemetry logging interval has failed";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_LOG_INTERVAL, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.FIVE_MINUTES_IN_SECONDS)
					&& BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_STATS_ENABLE,
							BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Successfully updated the telemetry log interval as 300 using webpa");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s5";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Validate if /rdklogs/logs/wifihealth.txt file is present ");
			LOGGER.info("STEP 5: ACTION : execute the following command inside the RG console of the gateway,"
					+ " \"if [ -f /rdklogs/logs/wifihealth.txt ] ; then echo \"true\" ; else echo \"false\" ; fi\"");
			LOGGER.info("STEP 5: EXPECTED : The file wifihealth.txt parameter should present ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "The log file wifihealth.txt is not present even after 10 minutes of waitime";
			status = BroadBandCommonUtils.doesFileExistWithinGivenTimeFrame(tapEnv, device,
					BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 5: ACTUAL : The log file wifihealth.txt is present on the device");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s6";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Customise the private WiFi SSID and password ");
			LOGGER.info(
					"STEP 6: ACTION : Execute WebPa SET command on parameters Device.WiFi.SSID.10001.SSID and Device.WiFi.AccessPoint.10001.Security.X_COMCAST-COM_KeyPassphrase");
			LOGGER.info("STEP 6: EXPECTED : The SSID and passphrase should get customized successfully");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Attempt to customise the private wifi SSID and password has failed";
			status = BroadBandWiFiUtils.changePrivateWiFiSsidAndPassphraseFor24And5Ghz(device);
			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL : Attempt to customise the private wifi SSID and" + " password is successful");
				LOGGER.info("Waiting for 90 seconds for the changes to take effect");
				tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s7";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Connect clients to 2.4 GHz SSID and 5GHz SSID and "
					+ " verify connection status ");
			LOGGER.info("STEP 7: ACTION : Connect to 2.4 and 5 GHz wifi using below commands Linux :nmcli"
					+ " dev wifi connect <ssid> password <passwd>");
			LOGGER.info("STEP 7: EXPECTED : The file wifihealth.txt parameter should present within"
					+ " 10 minutes of uptime");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Attempt to connect clients to 2.4 and 5 Ghz private wifi has failed";

			try {
				connectedClient2GHzDut = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			status = null != connectedClient2GHzDut;
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				LOGGER.info("Device has been connected with 2.4 GHz private Wi-Fi network");

				try {
					connectedClient5GHzDut = BroadBandConnectedClientUtils.getOtherWiFiCapableClientDeviceAndConnect(
							device, tapEnv, connectedClient2GHzDut, BroadBandTestConstants.BAND_5GHZ);
				} catch (Exception e) {
					errorMessage = e.getMessage();
					LOGGER.error(errorMessage);
				}
				status = null != connectedClient5GHzDut;
				if (status) {
					LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
					tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
					LOGGER.info("Device has been connected with 5 GHz private Wi-Fi network");
				}
			}

			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Attempt to connect clients to 2.4GHz and 5ghZ"
						+ " Private wifi is successful");
				LOGGER.info("Waiting for two minutes for the changes to take effect");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s8";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Validate the client stats telemetry for private wifi clients ");
			LOGGER.info(
					"STEP 8: ACTION : execute the command , \"cat /rdklogs/logs/wifihealth.txt | grep -i max | grep -i clients_1 | tail -2\""
							+ " at RG console and check for telemetry markers. If markers are not seen ,"
							+ " wait  for 5 more minutes before checking the same. ");
			LOGGER.info("STEP 8: EXPECTED : Telemetry markers should be available before or at"
					+ " 5th minute after connecting clients ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Telemetry markers are not available for client stats ";
			status = CommonMethods.isNotNull(BroadBandCommonUtils.executeCommandByPolling(device, tapEnv,
					BroadBandCommandConstants.CMD_MAX_TX_RX_CLIENT_1, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
					BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Telemetry markers are available for Max TxRx rate client");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s9";
			errorMessage = "Calculating the average CPU and Memory utilisation for 10 minutes "
					+ "with client stats telemetry enabled has failed";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Calculate the average CPU and memory utilisation for 10 minutes"
					+ " with client stats telemetry enabled");
			LOGGER.info("STEP 9: ACTION : execute the following command every one minute for 10 minutes"
					+ " and calculate the average");
			LOGGER.info("STEP 9: EXPECTED : Command execution on the device should be successful");
			LOGGER.info("**********************************************************************************");
			response = BroadBandWifiWhixUtils.calculateAverageCpuAndMemoryUtilisation(device, tapEnv,
					BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			LOGGER.info("STEP 9: Response for CPU and memory utilisation: " + response);
			status = CommonMethods.isNotNull(response);
			if (status) {
				afterEnablingFeature = response;
				LOGGER.info("STEP 9: ACTUAL : Calculating the average CPU and Memory utilisation "
						+ "for 10 minutes with client stats telemetry enabled is successful");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s10";
			errorMessage = "There is negative impact on the device when this feature is enabled";
			status = false;
			BroadBandResultObject bandResultObject = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Compare the before and after values to validate the existence"
					+ " of negative impact");
			LOGGER.info("STEP 10: ACTION : calculate the difference in utilisation and convert to percent"
					+ " and validate against 10%");
			LOGGER.info("STEP 10: EXPECTED : The increase in utilisation shouldn't be more than 10%");
			LOGGER.info("**********************************************************************************");
			bandResultObject = BroadBandWifiWhixUtils
					.validateCpuAndMemoryUtilisationForNegativeEffect(beforeEnablingFeature, afterEnablingFeature);
			if (bandResultObject.isStatus()) {
				LOGGER.info("STEP 10: ACTUAL : There is no negative impact on the device when "
						+ "this feature is enabled");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, bandResultObject.isStatus(),
					bandResultObject.getErrorMessage(), true);
			// ##################################################################################################//

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1004");
	}

	/**
	 * Verify enhanced Wifi logging in Telemetry for 5ghz private wifi parameters
	 * <ol>
	 * <li>Get 5GHz WiFi connected client device</li>
	 * <li>Verify wifihealth.txt file availability in /rdklogs/logs folder and clear
	 * the contents</li>
	 * <li>Check the whether wifi statistics are enabled</li>
	 * <li>Verify WiFi log interval is 3600 by default using WebPA command</li>
	 * <li>Change the WiFi log interval to 300sec using WebPA command" and wait for
	 * 5 minutes</li>
	 * <li>Run the script sh /usr/ccsp/wifi/aphealth_log.sh</li>
	 * <li>Run the script sh /usr/ccsp/wifi/aphealth_log.sh</li>
	 * <li>Verify the log print for RSSI value for client device connected with 5
	 * GHz wifi band</li>
	 * <li>Verify the log print for bytes of AP RX for client device connected with
	 * 5 GHz wifi band</li>
	 * <li>Verify the log print for RX DELTA of current and previous value for
	 * client device connected with 5 GHz wifi band</li>
	 * <li>Verify the log print for RXTXDELTA of 5Ghz client</li>
	 * <li>Change the WiFi log interval to default of 3600sec using WebPA
	 * command</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author anandam.s
	 * @refactor Athira
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-WIFI-TELEMETRY-1005")
	public void verifyTelemetryMarkersFor5ghzClientsForPrivateWifi(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-TELEMETRY-105";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1005");
		LOGGER.info("TEST DESCRIPTION: Verify enhanced Wifi logging in Telemetry for 5ghz  private wifi parameters");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Get 5GHz WiFi connected client device");
		LOGGER.info("2. Verify wifihealth.txt file availability in /rdklogs/logs folder  and clear the contents");
		LOGGER.info("3. Check the whether wifi statistics are enabled ");
		LOGGER.info("4. Verify WiFi log interval is 3600 by default using WebPA command");
		LOGGER.info("5. Change the WiFi log interval to 300sec using WebPA command\" and wait for 5 minutes ");
		LOGGER.info("6. Run the script sh /usr/ccsp/wifi/aphealth_log.sh");
		LOGGER.info("7. Verify the log print for  RSSI value for client device connected with 5 GHz wifi band");
		LOGGER.info("8. Verify the log print for bytes of AP RX for  client device connected with 5 GHz wifi band");
		LOGGER.info(
				"9. Verify the log print for RX DELTA of current and previous value for  client device connected with 2.4 GHz wifi band");
		LOGGER.info("10. Verify the log print for  RXTXDELTA  of   5Ghz client");
		LOGGER.info("11. Change the WiFi log interval to default of 3600sec using WebPA command");

		LOGGER.info("#######################################################################################");

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			BroadBandPreConditionUtils.executePreConditionToFactoryResetAndReacitivateDevice(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1, true);
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 3: DESCRIPTION :Check whether 5ghz private wifi is enabled. ");
			LOGGER.info("PRE-CONDITION 3: ACTION : Execute get on webpa Device.WiFi.SSID.10101.Enable");
			LOGGER.info("PRE-CONDITION 3: EXPECTED :5ghz private wifi  should be in enabled state.  ");
			LOGGER.info("#######################################################################################");
			String response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS);
			status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info(
						"PRE-CONDITION 3: ACTUAL : Pre condition executed successfully.5ghz private wifi is in enabled state.");
			} else {
				LOGGER.error("PRE-CONDITION 3: ACTUAL : Pre condition failed. Trying to eanble the 5ghz private WIFI");

				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
				if (!status) {
					errorMessage = "Could not enable 5ghz private wifi.So skipping the tests";
					throw new TestException(errorMessage);
				} else {
					tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				}
			}

			LOGGER.info("**********************************************************************************");

			stepNum = "s1";
			errorMessage = "Failed to get a 5ghz connected client device";
			status = false;
			Dut fiveGhzWifiDut = null;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Get a 5GHz private WiFi connected client device");
			LOGGER.info("STEP 1: ACTION : Get 5GHz WiFi connected client device");
			LOGGER.info("STEP 1: EXPECTED :  Should get 5GHz WiFi connected device");
			LOGGER.info("**********************************************************************************");
			try {
				fiveGhzWifiDut = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
				status = (fiveGhzWifiDut != null);
			} catch (Exception e) {
				errorMessage = "Exception while trying to connect a client to 5ghz private WIFI";
				LOGGER.error(errorMessage + " . " + e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully connected a client to 5ghz private wifi");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			/** Step 2 to Step 11 */
			executeCommonStepsForTelemetryTestCases(testCaseId, stepNum, device, WiFiFrequencyBand.WIFI_BAND_5_GHZ,
					BroadBandTestConstants.STRING_VALUE_TWO, fiveGhzWifiDut);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1005");
	}

	/**
	 * Test to verify telemetry Logging for Ethernet Connected devices
	 * <ol>
	 * <li>1. Verify ethernet log enabled is false by default using WebPa
	 * command</li>
	 * <li>2. Verify Ethernet log interval is 3600 by default using WebPA
	 * command</li>
	 * <li>3. Collect CPU and memory usage stats for 10 minutes when feature is
	 * disabled</li>
	 * <li>4. Enable ETH log enabled using WebPA command</li>
	 * <li>5. Change the Ethernet log period to 10sec using WebPA command</li>
	 * <li>6. Verify the number of Ethernet devices connected</li>
	 * <li>7. Verify the Ethernet connected device mac from log message</li>
	 * <li>8. Verify the Ethernet connected device phy rate from log message</li>
	 * <li>9. Compare Ehernet mac total count with WebPa command</li>
	 * <li>10. Get the Ethernet client mac address using Webpa operation and compare
	 * the value with log message</li>
	 * <li>11. Collect CPU and memory usage stats for 10 minutes when feature is
	 * enabled</li>
	 * <li>12. Compare the results from Step 3 and Step 11</li>
	 * </ol>
	 * 
	 * @author ArunKumar Jayachandran
	 * @refactor Athira
	 * 
	 * @param device
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SECURITY })
	@TestDetails(testUID = "TC-RDKB-TELEMETRY-CC-1005")
	public void testTelemetryMarkersForLanClient(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRY-CC-1005");
		LOGGER.info("TEST DESCRIPTION: Test to verify telemetry Logging for Ethernet Connected devices");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify ethernet log enabled is false by default using WebPa command");
		LOGGER.info("2. Verify Ethernet log interval is 3600 by default using WebPA command");
		LOGGER.info("3. Collect CPU and memory usage stats for 10 minutes when feature is disabled");
		LOGGER.info("4. Enable ETH log enabled using WebPA command");
		LOGGER.info("5. Change the Ethernet log period to 10sec using WebPA command");
		LOGGER.info("6. Verify the number of Ethernet devices connected");
		LOGGER.info("7. Verify the Ethernet connected device mac from log message");
		LOGGER.info("8. Verify the Ethernet connected device phy rate from log message");
		LOGGER.info("9. Compare Ehernet mac total count with WebPa command");
		LOGGER.info(
				"10. Get the Ethernet client mac address using Webpa operation and compare the value with log message");
		LOGGER.info("11. Collect CPU and memory usage stats for 10 minutes when feature is enabled");
		LOGGER.info("12. Compare the results from Step 3 and Step 11");
		LOGGER.info("#######################################################################################");
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-TELEMETRY-CC-005";
		String stepNumber = "s1";
		String errorMessage = null;
		boolean status = false;
		String response = null;
		Dut lanDut = null;
		String ethMacTotalCount = null;
		String beforeEnablingFeature = null;
		String afterEnablingFeature = null;
		String ethMac = null;
		String telemetryLogFile = null;
		String macInterface = null;
		// Variable Declaration Ends
		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		LOGGER.info("PRE-CONDITION STEPS");
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Get Ethernet connected device from connected clients");
		LOGGER.info("PRE-CONDITION 1 : ACTION : Get Ethernet connected device");
		LOGGER.info("PRE-CONDITION 1 : Should get the Ethernet connected device");
		LOGGER.info("#######################################################################################");
		errorMessage = "Failed to get the Ethernet connected client device";
		lanDut = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
		if (lanDut == null) {
			throw new TestException(
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.PRE_CONDITION_ERROR,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER, errorMessage));
		}
		LOGGER.info("PRE-CONDITION : ACTUAL: Ethernet connected device is available: " + lanDut);
		LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
		LOGGER.info("#######################################################################################");
		try {
			/**
			 * Step 1: VERIFY ETHERNET LOG ENABLED IS FALSE BY DEFAULT USING WEBPA COMMAND
			 */
			stepNumber = "s1";
			status = false;
			telemetryLogFile = BroadBandCommandConstants.FILE_ETH_TELEMETRY_TXT;
			if (DeviceModeHandler.isDSLDevice(device)) {
				telemetryLogFile = BroadBandCommandConstants.FILE_ETH_AGENT_LOG;
			}
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify Ethernet log enabled is false by default using WebPA command");
			LOGGER.info(
					"STEP 1: ACTION: Execute Command Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMEthLogEnabled");
			LOGGER.info(
					"STEP 1: EXPECTED: The webPA command should execute successfully and return default value as false");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to get the WebPa Response for webpa parameter: "
					+ BroadBandWebPaConstants.WEBPA_PARAM_ETH_LOG_ENABLED;
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ETH_LOG_ENABLED, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL: Successfully verified default value of Ethernet log enabled");
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 2 : VERIFY ETHERNET LOG INTERVAL IS 3600 BY DEFAULT USING WEBPA COMMAND
			 */
			stepNumber = "s2";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify Ethernet log period is 3600 by default using WebPA command");
			LOGGER.info(
					"STEP 2: ACTION: Execute Command Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMEthLogPeriod");
			LOGGER.info(
					"STEP 2: EXPECTED: The webPA command should execute successfully and return default value as false");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to get Webpa Response for webpa parameter: "
					+ BroadBandWebPaConstants.WEBPA_PARAM_ETH_LOG_PERIOD;
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ETH_LOG_PERIOD,
					String.valueOf(BroadBandTestConstants.INTERGER_CONSTANT_3600),
					BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL: Successfully verified default value of Ethernet log period");
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 3 : COLLECT CPU AND MEMORY USAGE STATS FOR 10 MINUTES WHEN FEATURE IS
			 * DISABLED
			 */
			stepNumber = "s3";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION: Collect CPU and memory usage stats for 10 minutes when feature is disabled");
			LOGGER.info(
					"STEP 3: ACTION: a) execute the following command inside the RG console of the gateway for every one minute and collect the data for CPU and memory usage, "
							+ "\"top -n 1 |grep -i Mem |sed  's/^[^0-9]*//;s/[^0-9].*$//'\" and \"top -n 1 |grep CPU: |sed  's/^[^0-9]*//;s/[^0-9].*$//'\"\n b) Calculate the average for the data collected ");
			LOGGER.info("STEP 3: EXPECTED: Command execution on the device should be successful");
			LOGGER.info("******************************************************************************");
			errorMessage = "Unable to collect CPU and memory usage data";
			response = BroadBandWifiWhixUtils.calculateAverageCpuAndMemoryUtilisation(device, tapEnv,
					BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			LOGGER.info("STEP 3: Response for CPU and memory utilisation: " + response);
			status = CommonMethods.isNotNull(response);
			if (status) {
				beforeEnablingFeature = response;
				LOGGER.info(
						"STEP 3: ACTUAL : Calculating the average CPU and Memory utilisation for 10 minutes before enabling logging");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 4 : ENABLE ETH LOG ENABLED USING WEBPA COMMAND
			 */
			stepNumber = "s4";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Enable ETH log enabled using WebPA command");
			LOGGER.info(
					"STEP 4: ACTION : Execute WebPa set command for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMEthLogEnabled WebPa parameter");
			LOGGER.info("STEP 4: EXPECTED : WebPa set command should be successful");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to set the WebPa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMEthLogEnabled as true";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ETH_LOG_ENABLED, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 4: ACTUAL: Successfully enabled Ethernet log enabled using WebPa set operation");
			} else {
				LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 5 : CHANGE THE ETHERNET LOG PERIOD TO 10SEC USING WEBPA COMMAND
			 */
			stepNumber = "s5";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Change the Ethernet log period to 10sec using WebPA command");
			LOGGER.info(
					"STEP 5: ACTION: Execute WebPa set command for Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMEthLogPeriod WebPa parameter");
			LOGGER.info("STEP 5: EXPECTED: WebPa set command should be successful");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to set the WebPa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMEthLogPeriod as 10";
			status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ETH_LOG_PERIOD, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_10, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL: Successfully updated the Ethernet log period to 10 second using WebPa set operation");
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 6 : VERIFY THE NUMBER OF ETHERNET DEVICES CONNECTED
			 */
			stepNumber = "s6";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify the number of Ethernet devices connected");
			LOGGER.info("STEP 6: ACTION : Execute command 1. grep -i \"ETH_MAC_1_TOTAL_COUNT:\" " + telemetryLogFile);
			LOGGER.info(
					"STEP 6: EXPECTED : Successfully verified the total no.of devices connected through ethernet interface");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to get the total no.of ethernet connected devices";
			for (int counter = BroadBandTestConstants.CONSTANT_1; counter < BroadBandTestConstants.CONSTANT_5; counter++) {
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_ETH_MAC_TOTAL_COUNT.replace(
								BroadBandTestConstants.STRING_REPLACE, Integer.toString(counter)),
						telemetryLogFile, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
						BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				if (CommonMethods.isNotNull(response)) {
					ethMacTotalCount = CommonMethods.patternFinder(response,
							BroadBandTestConstants.PATTERN_ETH_MAC_TOTAL_COUNT
									.replace(BroadBandTestConstants.STRING_REPLACE, Integer.toString(counter)));
					status = CommonMethods.isNotNull(ethMacTotalCount)
							&& Integer.parseInt(ethMacTotalCount) != BroadBandTestConstants.CONSTANT_0;
				}
				if (status) {
					macInterface = Integer.toString(counter);
					break;
				}
			}
			if (status) {
				LOGGER.info("STEP 6: ACTUAL: Successfully collected the total no.of Ethernet connected client devices");
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 7 : VERIFY THE ETHERNET CONNECTED DEVICE MAC FROM LOG MESSAGE
			 */
			stepNumber = "s7";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify the Ethernet connected device mac from log message");
			LOGGER.info("STEP 7: ACTION : Execute command 1. grep -i \"ETH_MAC_1:\" " + telemetryLogFile);
			LOGGER.info("STEP 7: EXPECTED : Successfully verified the device mac connected through ethernet interface");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to get the ethernet connected device mac address";
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_ETH_MAC.replace(BroadBandTestConstants.STRING_REPLACE,
							macInterface),
					telemetryLogFile, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (CommonMethods.isNotNull(response)) {
				ethMac = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_ETH_MAC
						.replace(BroadBandTestConstants.STRING_REPLACE, macInterface));
				status = CommonMethods.isNotNull(ethMac);
			}
			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL: Successfully collected the mac address of Ethernet connected client devices");
			} else {
				LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 8 : VERIFY THE ETHERNET CONNECTED DEVICE PHY RATE FROM LOG MESSAGE
			 */
			stepNumber = "s8";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify the Ethernet connected device phy rate from log message");
			LOGGER.info("STEP 8: ACTION : Execute command 1. grep -i \"ETH_PHYRATE_1:\" " + telemetryLogFile);
			LOGGER.info("STEP 8: EXPECTED : Successfully verified the device mac connected through ethernet interface");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to get the ethernet connected device mac address";
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_ETH_PHYRATE.replace(BroadBandTestConstants.STRING_REPLACE,
							macInterface),
					telemetryLogFile, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_ETH_PHYRATE
					.replace(BroadBandTestConstants.STRING_REPLACE, macInterface));
			if (status) {
				LOGGER.info("STEP 8: ACTUAL: Successfully collected the phy rate of Ethernet connected client devices: "
						+ response);
			} else {
				LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 9 : COMPARE EHERNET MAC TOTAL COUNT WITH WEBPA COMMAND
			 */
			stepNumber = "s9";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Compare Ehernet mac total count with WebPa command");
			LOGGER.info(
					"STEP 9: ACTION : Execute command: Execute WebPa get command: Device.Ethernet.Interface.1.X_RDKCENTRAL-COM_AssociatedDeviceNumberOfEntries");
			LOGGER.info("STEP 9: EXPECTED : Response should be success and should be same with log message");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to get the Ethernet mac total count from WebPa parameter";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ETH_ASSOCIATED_CLIENT_NO_OF_ENTRIES
							.replace(BroadBandTestConstants.TR181_NODE_REF, macInterface),
					ethMacTotalCount, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL: Successfully compared the no.of associated number of entries are same as log message");
			} else {
				LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 10 : GET THE ETHERNET CLIENT MAC ADDRESS USING WEBPA OPERATION AND
			 * COMPARE THE VALUE WITH LOG MESSAGE
			 */
			stepNumber = "s10";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info(
					"STEP 10: DESCRIPTION : Get the Ethernet client mac address using Webpa operation and compare the value with log message");
			LOGGER.info(
					"STEP 10: ACTION : Execute WebPa get command: Device.Ethernet.Interface.1.X_RDKCENTRAL-COM_AssociatedDevice.1.MACAddress");
			LOGGER.info("STEP 10: EXPECTED : Response shoud be successful and should be same with log message");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to get the Ethernet mac from WebPa parameter";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ETH_CLIENT_MAC_ADDRESS
							.replace(BroadBandTestConstants.TR181_NODE_REF, macInterface),
					ethMac, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 10: ACTUAL: Successfully verified Ethernet client mac address from webpa and log");
			} else {
				LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 11: COLLECT CPU AND MEMORY USAGE STATS FOR 10 MINUTES WHEN FEATURE IS
			 * ENABLED
			 */
			stepNumber = "s11";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP 11: DESCRIPTION: Collect CPU and memory usage stats for 10 minutes when feature is enabled");
			LOGGER.info(
					"STEP 11: ACTION: a) execute the following command inside the RG console of the gateway for every one minute and collect the data for CPU and memory usage, "
							+ "\"top -n 1 |grep -i Mem |sed  's/^[^0-9]*//;s/[^0-9].*$//'\" and \"top -n 1 |grep CPU: |sed  's/^[^0-9]*//;s/[^0-9].*$//'\"\n b) Calculate the average for the data collected ");
			LOGGER.info("STEP 11: EXPECTED: Command execution on the device should be successful");
			LOGGER.info("******************************************************************************");
			errorMessage = "Unable to collect CPU and memory usage data";
			response = BroadBandWifiWhixUtils.calculateAverageCpuAndMemoryUtilisation(device, tapEnv,
					BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			LOGGER.info("STEP 11: Response for CPU and memory utilisation: " + response);
			status = CommonMethods.isNotNull(response);
			if (status) {
				afterEnablingFeature = response;
				LOGGER.info(
						"STEP 11: ACTUAL : Calculating the average CPU and Memory utilisation for 10 minutes after enabling log");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 12 : COMPARE THE RESULTS FROM STEP 3 AND STEP 11
			 */
			stepNumber = "s12";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION: Compare the results from Step 3 and Step 11");
			LOGGER.info("STEP 12: ACTION: Compare the averages calculated for CPU utilisation and memory utilisation");
			LOGGER.info(
					"STEP 12: EXPECTED: The difference in average should be within 10%, indicating that the feature doesn't have any negative impact on the device");
			LOGGER.info("******************************************************************************");
			errorMessage = "The feature causes negative impact on the device";
			BroadBandResultObject bandResultObject = null;
			bandResultObject = BroadBandWifiWhixUtils
					.validateCpuAndMemoryUtilisationForNegativeEffect(beforeEnablingFeature, afterEnablingFeature);
			if (bandResultObject.isStatus()) {
				LOGGER.info("STEP 12: ACTUAL : There is no negative impact on the device when this feature is enabled");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, bandResultObject.isStatus(),
					bandResultObject.getErrorMessage(), true);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Validating telemetry logging for Ethernet connected device:"
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 1 : DESCRIPTION : Disable ETH log enabled and log period to default using WebPA command");
			LOGGER.info(
					"POST-CONDITION 1 : ACTION : Execute webpa set command: parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMEthLogEnabled datatype: bool, Value: false "
							+ "Parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Logging.xOpsDMEthLogPeriod Datatype: uint, Value: 3600");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : Should disabled Ethernet log enabled & period using webpa");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to revert back to default value";
			status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ETH_LOG_ENABLED, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS)
					&& BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_ETH_LOG_PERIOD, BroadBandTestConstants.CONSTANT_2,
							BroadBandTestConstants.STRING_CONSTANT_3600,
							BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"POST-CONDITION 1 : ACTUAL : Successfully Disabled ETH log and log period to default using WebPA command");
			} else {
				LOGGER.info("POST-CONDITION 1 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRY-CC-1005");
	}

	/**
	 * Test to verify telemetry Logging for WiFi Connected devices
	 * <ol>
	 * <li>PRE-CONDITION : Get 2.4GHz WiFi connected device.</li>
	 * <li>1. Copy dcm.properties to /nvram folder and change the
	 * DCM_LOG_SERVER_URL</li>
	 * <li>2. Reboot the device and wait for IP acquisition</li>
	 * <li>3. Validate modified url in dcmscript.log file</li>
	 * <li>4. Connecting to 2.4Ghz again after Reboot</li>
	 * <li>5. Verify WiFi log interval is 3600 by default using WebPA command</li>
	 * <li>6. Change the WiFi log interval to 300sec using WebPA command</li>
	 * <li>7. Verify the number of Ethernet devices connected</li>
	 * <li>8. Compare WiFi mac total count with WebPa command</li>
	 * <li>9. Verify the 2.4GHz client device mac address</li>
	 * <li>10. Compare WiFi mac address with WebPa command</li>
	 * <li>11. Verify Telemetry logging for total no.of WiFi connected devices</li>
	 * <li>12. Change the WiFi log interval to default of 3600sec using WebPA
	 * command</li>
	 * </ol>
	 * 
	 * @author ArunKumar Jayachandran
	 * @refactor Athira
	 * 
	 * @param device
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SECURITY })
	@TestDetails(testUID = "TC-RDKB-TELEMETRY-CC-1006")
	public void testTelemetryMarkersForWiFiClient(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-TELEMETRY-CC-1006");
		LOGGER.info("TEST DESCRIPTION: Test to verify telemetry Logging for WiFi Connected devices");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE CONDITION : Get 2.4GHz WiFi connected device.");
		LOGGER.info("1. Copy dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL");
		LOGGER.info("2. Reboot the device and wait for IP acquisition");
		LOGGER.info("3. Validate modified url in dcmscript.log file");
		LOGGER.info("4. Connecting to 2.4Ghz again after Reboot");
		LOGGER.info("5. Verify WiFi log interval is 3600 by default using WebPA command");
		LOGGER.info("6. Change the WiFi log interval to 300sec using WebPA command");
		LOGGER.info("7. Verify the number of Ethernet devices connected");
		LOGGER.info("8. Compare WiFi mac total count with WebPa command");
		LOGGER.info("9. Verify the 2.4GHz client device mac address");
		LOGGER.info("10. Compare WiFi mac address with WebPa command");
		LOGGER.info("11. Verify Telemetry logging for total no.of WiFi connected devices");
		LOGGER.info("12. Change the WiFi log interval to default of 3600sec using WebPA command");
		LOGGER.info("#######################################################################################");

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-TELEMETRY-CC-006";
		String stepNumber = "s1";
		String errorMessage = null;
		boolean status = false;
		String response = null;
		Dut wifiDut = null;
		String wifiMacTotalCount = null;
		String wifiMacAddress = null;
		long startTime = BroadBandTestConstants.CONSTANT_0;
		String deviceDateTime = null;
		boolean isAtomSyncAvailable = false;
		isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
		String searchLogMessage = null;
		String pattenMatcher = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		LOGGER.info("PRE-CONDITION STEPS");
		LOGGER.info("#############################################################");
		LOGGER.info("PRE-CONDITION : DESCRIPTION : Get 2.4GHz WiFi connected device");
		LOGGER.info("PRE-CONDITION : ACTION : Get 2.4GHz WiFi connected client device");
		LOGGER.info("PRE-CONDITION : EXPECTED : Should get 2.4GHz WiFi connected device");
		LOGGER.info("#############################################################");
		wifiDut = BroadBandConnectedClientUtils.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device,
				tapEnv);
		if (wifiDut == null) {
			errorMessage = "Failed to get the 2.4GHz WiFi connected client device";
			throw new TestException(
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.PRE_CONDITION_ERROR,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER, errorMessage));
		}
		LOGGER.info("PRE-CONDITION : ACTUAL: 2.4GHz WiFi connected device is available: " + wifiDut);
		postTelemetryData(device);
		LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

		try {
			// telemetry configuration settings from step 1 to 3
			telemetryConfiguration(device, testCaseId);

			// STEP 4: Connecting to 2.4Ghz again after Reboot
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, wifiDut,
					BroadBandTestConstants.BAND_2_4GHZ, BroadBandTestConstants.CONSTANT_4);

			// STEP 5: Verify WiFi log interval is 3600 by default using WebPA command
			stepNumber = "s5";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify WiFi log interval is 3600 by default using WebPA command");
			LOGGER.info(
					"STEP 5: ACTION : Execute Command Device.Deviceinfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval");
			LOGGER.info(
					"STEP 5: EXPECTED : The webPA command should execute successfully and return default value as false");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Null Response is retrieved for webpa parameter: "
					+ BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_LOG_INTERVAL;
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_LOG_INTERVAL,
					String.valueOf(BroadBandTestConstants.INTERGER_CONSTANT_3600),
					BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 5" + " : ACTUAL : Successfully verified default value of WiFi log interval");
			} else {
				LOGGER.error("STEP 5" + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 6: Change the WiFi log interval to 300sec using WebPA command
			stepNumber = "s6";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Change the WiFi log interval to 300sec using WebPA command");
			LOGGER.info(
					"STEP 6: ACTION : Execute WebPa set command for Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval WebPa parameter");
			LOGGER.info("STEP 6: EXPECTED : WebPa set command should be successful");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to set the WebPa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval as 300";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_LOG_INTERVAL, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_300, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 6"
						+ " : ACTUAL : Successfully updated the WiFi log interval to 300second using WebPa set operation");
			} else {
				LOGGER.error("STEP 6" + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 7: Verify the number of 2.4GHz wifi client devices connected
			stepNumber = "s7";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify the number of Wifi Client connected");
			LOGGER.info(
					"STEP 7: ACTION : Execute command 1. grep -i \"WIFI_MAC_1_TOTAL_COUNT:\" /rdklogs/logs/wifihealth.txt");
			LOGGER.info("STEP 7: EXPECTED : Successfully verified the total no.of devices connected");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to get the total no.of 2.4GHz WiFi connected devices";
			startTime = System.currentTimeMillis();
			do {
				searchLogMessage = BroadBandCommonUtils
						.concatStringUsingStringBuffer(BroadBandTraceConstants.LOG_MESSAGE_WIFI_MAC_1_TOTAL_COUNT);
				broadBandResultObject = BroadBandCommonUtils.verifyLogsInAtomOrArmWithPatternMatcherWithLogTime(tapEnv,
						device, searchLogMessage, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG,
						BroadBandTestConstants.PATTERN_WIFI_MAC_1_TOTAL_COUNT,
						CommonMethods.isAtomSyncAvailable(device, tapEnv), deviceDateTime);
				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
				wifiMacTotalCount = broadBandResultObject.getOutput();
				if (status) {
					status = CommonMethods.isNotNull(wifiMacTotalCount) && BroadBandCommonUtils
							.convertStringToInteger(wifiMacTotalCount) != BroadBandTestConstants.CONSTANT_0;
				}
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.SIX_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info(
						"STEP 7" + " : ACTUAL : Successfully collected the total no.of WiFi connected client devices");
			} else {
				LOGGER.error("STEP 7" + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 8: Compare WiFi mac total count with WebPa command
			stepNumber = "s8";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Compare WiFi mac total count with WebPa command");
			LOGGER.info("STEP 8: ACTION : Execute Command Device.WiFi.AccessPoint.1.AssociatedDeviceNumberOfEntries");
			LOGGER.info(
					"STEP 8: EXPECTED : The webPA command should execute successfully and response should be equal from log");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Null Response is retrieved for webpa parameter: "
					+ BroadBandWebPaConstants.WEBPA_PARAM_WIFI_NO_OF_ENTRIES;
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WIFI_NO_OF_ENTRIES);
			LOGGER.info("STEP 8: WebPA Response from " + BroadBandWebPaConstants.WEBPA_PARAM_WIFI_NO_OF_ENTRIES
					+ " parameter is: " + response);
			if (CommonMethods.isNotNull(response)) {
				status = response.equalsIgnoreCase(wifiMacTotalCount);
				errorMessage = "WiFi total mac count is not same in webpa response and log file :" + response;
			}
			if (status) {
				LOGGER.info(
						"STEP 8" + " : ACTUAL : Successfully verified WiFi total mac count from log file and webpa");
			} else {
				LOGGER.error("STEP 8" + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 9: Verify the 2.4GHz client device mac address
			stepNumber = "s9";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify the 2.4GHz client device mac address");
			LOGGER.info("STEP 9: ACTION : Execute command 1. grep -i \"WIFI_MAC_1:\" /rdklogs/logs/wifihealth.txt");
			LOGGER.info(
					"STEP 9: EXPECTED : Successfully verified the total no.of devices connected through ethernet interface");
			LOGGER.info("*******************************************************************************");
			if (Integer.parseInt(wifiMacTotalCount) != BroadBandTestConstants.CONSTANT_0) {
				errorMessage = "Failed to get the 2.4GHz WiFi client mac address from /rdklogs/logs/wifihealth.txt";
				searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
						BroadBandTraceConstants.LOG_MESSAGE_WIFI_MAC, BroadBandTestConstants.STRING_CONSTANT_1,
						AutomaticsConstants.COLON);

				pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
						BroadBandTestConstants.REG_EXPRESSION_TO_GET_MAC_ADDRESS_SEMICOLON);

				broadBandResultObject = BroadBandCommonUtils.verifyLogsInAtomOrArmWithPatternMatcherWithLogTime(tapEnv,
						device, searchLogMessage, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG, pattenMatcher,
						isAtomSyncAvailable, deviceDateTime);

				status = broadBandResultObject.isStatus();
				errorMessage = broadBandResultObject.getErrorMessage();
				wifiMacAddress = broadBandResultObject.getOutput();
				if (status) {
					status = CommonMethods.isNotNull(wifiMacAddress);
				}
				if (status) {
					LOGGER.info(
							"STEP 9" + " : ACTUAL : Successfully 2.4GHz WiFi client mac address: " + wifiMacAddress);
				} else {
					LOGGER.error("STEP 9" + " : ACTUAL : " + errorMessage + wifiMacAddress);
				}
				LOGGER.info("*******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				errorMessage = "No clients are connected through 2.4GHz WiFi";
				LOGGER.info("STEP 9: " + errorMessage);
				LOGGER.info("*******************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			// STEP 10: Compare WiFi mac address with WebPa command
			stepNumber = "s10";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Compare WiFi mac address with WebPa command");
			LOGGER.info("STEP 10: ACTION : Execute Command Device.WiFi.AccessPoint.1.AssociatedDevice.1.MACAddress");
			LOGGER.info(
					"STEP 10: EXPECTED : The webPA command should execute successfully and response should be same as previous value from log");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Null Response is retrieved for webpa parameter: "
					+ BroadBandWebPaConstants.WEBPA_PARAM_WIFI_MAC_ADDRESS;
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WIFI_MAC_ADDRESS);
			LOGGER.info("STEP 10: WebPA Response from " + BroadBandWebPaConstants.WEBPA_PARAM_WIFI_MAC_ADDRESS
					+ " parameter is: " + response);
			if (CommonMethods.isNotNull(response)) {
				status = response.equalsIgnoreCase(wifiMacAddress);
				errorMessage = "WiFi total mac address is not same in webpa response and log file :" + response;
			}
			LOGGER.info("STEP 10:"
					+ (status ? "ACTUAL: Successfully verified WiFi mac address from log file and webpa: " + response
							: errorMessage));
			if (status) {
				LOGGER.info("STEP 10" + " : ACTUAL : Successfully verified WiFi mac address from log file and webpa: "
						+ response);
			} else {
				LOGGER.error("STEP 10" + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 11: Verify Telemetry logging for total no.of WiFi connected devices
			stepNumber = "s11";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Verify Telemetry logging for total no.of WiFi connected devices");
			LOGGER.info(
					"STEP 11: ACTION : Execute command: 1. grep -i \"WIFI_MAC_1_TOTAL_COUNT\" /rdklogs/logs/dcmscript.log");
			LOGGER.info(
					"STEP 11: EXPECTED : Response should contain the telemetry log message of WIFI_MAC_1_TOTAL_COUNT under /rdklogs/log/dcmscript.log");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to get the telemetry log message for WIFI_MAC_1_TOTAL_COUNT";
			startTime = System.currentTimeMillis();
			boolean isTelemetry2Enabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			if (!isTelemetry2Enabled) {
				do {
					status = BroadBandCommonUtils.verifyConsoleLogByPassingDateFormat(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_WIFI_MAC_1_TOTAL_COUNT_WITHOUT_COLON,
							BroadBandTestConstants.DCMSCRIPT_LOG_FILE, deviceDateTime,
							BroadBandTestConstants.PATTERN_MATCHER_TIMESTAMP_SPEEDTEST_LOG_MESSAGE,
							BroadBandTestConstants.TIMESTAMP_FORMAT_SPEEDTEST_LOG_MESSAGE);
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS
						&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			} else {
				if (BroadbandPropertyFileHandler.isSplunkEnabled()) {
					List<String> verifySplunkLog = new ArrayList<>();
					verifySplunkLog.add(BroadBandTelemetryConstants.WIFI_MAC_COUNT_TELEMETRY_MARKER);
					do {
						LOGGER.info("Waiting for device to verify telemetry status");

						response = null;

						response = BroadBandTelemetry2Utils.retrieveTelemetryLogsFromSplunk(device, tapEnv,
								BroadBandTelemetryConstants.WIFI_MAC_COUNT_TELEMETRY_MARKER);
						if (CommonMethods.isNotNull(response)) {
							LOGGER.info("Respons on step 11 is : " + response);
							status = true;
						}

					} while (!status
							&& (System.currentTimeMillis()
									- startTime) < BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS
							&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
									BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS));
				}

			}

			if (!BroadbandPropertyFileHandler.isSplunkEnabled()) {

				LOGGER.info("Splunk is disabled");
				LOGGER.info("Skipping the step :");
				errorMessage = "failed to get the log as Splunk is disabled";
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);

				LOGGER.info("STEP 11" + " : ACTUAL : " + errorMessage);
			} else {
				if (status) {
					LOGGER.info("STEP 11" + " : ACTUAL : Telemetry log message available for Wifi mac 1 total count");
				} else {
					LOGGER.info("STEP 11" + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("*******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			}

			// STEP 12: Change the WiFi log interval to default of 3600sec using WebPA
			// command
			stepNumber = "s12";
			status = false;
			LOGGER.info("*******************************************************************************");
			LOGGER.info(
					"STEP 12: DESCRIPTION : Change the WiFi log interval to default of 3600sec using WebPA command");
			LOGGER.info(
					"STEP 12: ACTION : Execute WebPa set command for Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval WebPa parameter");
			LOGGER.info("STEP 12: EXPECTED : WebPa set command should be successful");
			LOGGER.info("*******************************************************************************");
			errorMessage = "Failed to set the WebPa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval as 3600";
			status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_LOG_INTERVAL, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_CONSTANT_3600, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 12"
						+ " : ACTUAL : Successfully updated the WiFi log interval to 3600second using WebPa set operation");
			} else {
				LOGGER.error("STEP 12" + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("*******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"Exception Occurred while Validating telemetry logging for WiFi connected device:" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					true);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-TELEMETRY-CC-1006");
	}

	/**
	 * Verify enhanced Wifi logging in Telemetry for 2.4ghz public wifi parameters
	 * <ol>
	 * <li>Enable public wifi on the device</li>
	 * <li>disable prefer private via webpa</li>
	 * <li>Connect to Wifi client with 2.4Ghz public wifi SSID</li>
	 * <li>Verify wifihealth.txt file availability in /rdklogs/logs folder and clear
	 * the contents</li>
	 * <li>Check the whether wifi statistics are enabled</li>
	 * <li>Verify WiFi log interval is 3600 by default using WebPA command</li>
	 * <li>Change the WiFi log interval to 300sec using WebPA command" and wait for
	 * 5 minutes</li>
	 * <li>Run the script sh /usr/ccsp/wifi/aphealth_log.sh</li>
	 * <li>Verify the log print for RSSI value for client device connected with 2.4
	 * GHz wifi band</li>
	 * <li>Verify the log print for bytes of AP RX for client device connected with
	 * 2.4 GHz wifi band</li>
	 * <li>Verify the log print for RX DELTA of current and previous value for
	 * client device connected with 2.4 GHz wifi band</li>
	 * <li>Change the WiFi log interval to default of 3600sec using WebPA
	 * command</li>
	 * <li>enable prefer private via webpa</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author anandam.s
	 * @refactor Athira
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-WIFI-TELEMETRY-1008")
	public void verifyTelemetryMarkersFor2ghzClientsForPublicWifi(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-TELEMETRY-108";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		boolean statusOfEnablingWifi = false;
		boolean statusOfEnablePreferPrivate = false;
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1008");
		LOGGER.info("TEST DESCRIPTION: Verify enhanced Wifi logging in Telemetry for   2.4ghz public  wifi parameters");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Enable public wifi on the device");
		LOGGER.info("2. disable prefer private via webpa");
		LOGGER.info("3. Connect to Wifi client with 2.4Ghz public wifi SSID  ");
		LOGGER.info("4. Verify wifihealth.txt file availability in /rdklogs/logs folder  and clear the contents");
		LOGGER.info("5. Check the whether wifi statistics are enabled ");
		LOGGER.info("6. Verify WiFi log interval is 3600 by default using WebPA command");
		LOGGER.info("7. Change the WiFi log interval to 300sec using WebPA command\" and wait for 5 minutes ");
		LOGGER.info("8. Run the script sh /usr/ccsp/wifi/aphealth_log.sh");
		LOGGER.info("9. Verify the log print for  RSSI value for client device connected with 2.4 GHz wifi band");
		LOGGER.info("10. Verify the log print for bytes of AP RX for  client device connected with 2.4 GHz wifi band");
		LOGGER.info(
				"11. Verify the log print for RX DELTA of current and previous value for  client device connected with 2.4 GHz wifi band");
		LOGGER.info("12. Change the WiFi log interval to default of 3600sec using WebPA command");
		LOGGER.info("13. enable prefer private via webpa");

		LOGGER.info("#######################################################################################");

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			BroadBandPreConditionUtils.executePreConditionToFactoryResetAndReacitivateDevice(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1, true);

			stepNum = "s1";
			errorMessage = "Attempt to enable public wifi on device has failed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Enable public wifi on the device");
			LOGGER.info("STEP 1: ACTION : Enable public wifi on the device via webpa");
			LOGGER.info("STEP 1: EXPECTED : public wifi should get enabled successfully");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
					BroadBandTestConstants.DUAL_BAND);
			statusOfEnablingWifi = status;
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully enabled public wifi using webpa");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Attempt to disable prefer private has failed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : disable prefer private via webpa");
			LOGGER.info(
					"STEP 2: ACTION : Execute WebPa SET command on the object  Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate");
			LOGGER.info("STEP 2: EXPECTED : The prefer private should get disabled successfully");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PREFER_PRIVATE_FEATURE_STATUS,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Sucessfully disabled prefer private feature via webpa");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Failed to get a 2.4ghz public wifi SSID  connected client device";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Connect to Wifi client with 2.4Ghz public wifi SSID  ");
			LOGGER.info("STEP 3: ACTION : Connect the Wifi  client with Gateway:netsh wlan connect name=\"<ssid>\"");
			LOGGER.info("STEP 3: EXPECTED : Connected client should connect to public wifi SSID succesfully");
			LOGGER.info("**********************************************************************************");
			Dut clientDut = null;
			try {
				clientDut = BroadBandConnectedClientUtils.getWiFiCapableClientDeviceAndConnectToPublicSsid(device,
						tapEnv, BroadBandConnectedClientTestConstants.SECURITY_MODE_OPEN.toLowerCase(),
						WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				status = (null != clientDut);
			} catch (Exception e) {
				errorMessage = "Exception while trying to connect a client to 2.4ghz public WIFI";
				LOGGER.error(errorMessage + " . " + e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Successfully connected to public wifi 2.4ghz radio");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			/** step 4 to Step 12 */
			executeCommonStepsForTelemetryTestCases(testCaseId, stepNum, device, WiFiFrequencyBand.WIFI_BAND_2_GHZ,
					BroadBandTestConstants.STRING_VALUE_FIVE, clientDut);

			LOGGER.info("**********************************************************************************");

			stepNum = "s13";
			errorMessage = "Attempt to enable prefer private has failed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : enable prefer private via webpa");
			LOGGER.info(
					"STEP 13: ACTION : Execute WebPa SET command on the object  Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate");
			LOGGER.info("STEP 13: EXPECTED : The prefer private should get enabled successfully");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PREFER_PRIVATE_FEATURE_STATUS,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			statusOfEnablePreferPrivate = status;
			if (status) {
				LOGGER.info("STEP 13: ACTUAL : Sucessfully enabled prefer private feature via webpa");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			if (statusOfEnablingWifi) {
				LOGGER.info("POST-CONDITION : DESCRIPTION :Disable public wifi. ");
				LOGGER.info("POST-CONDITION : ACTION : Execute set on webpa parameter to enable public wifi");
				LOGGER.info("POST-CONDITION : EXPECTED :public wifi  should be in disabled state.  ");
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI, WebPaDataTypes.BOOLEAN.getValue(),
						BroadBandTestConstants.FALSE);
				if (status) {
					LOGGER.info("POST-CONDITION : Successfully disabled public wifi. ");
				} else {
					LOGGER.info("POST-CONDITION : Failed to disable public wifi. ");
				}
			}
			if (!statusOfEnablePreferPrivate) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PREFER_PRIVATE_FEATURE_STATUS,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);

				if (status) {
					LOGGER.info("POST-CONDITION : Successfully enabled prefer private. ");
				} else {
					LOGGER.info("POST-CONDITION : Failed to enabled prefer private. ");
				}
			}

		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1008");
	}

	/**
	 * Verify enhanced Wifi logging in Telemetry for 5ghz Public wifi parameters
	 * <ol>
	 * <li>Enable Public wifi on the device</li>
	 * <li>disable prefer private via webpa</li>
	 * <li>Connect to Wifi client with 2.4Ghz Public wifi SSID</li>
	 * <li>Verify wifihealth.txt file availability in /rdklogs/logs folder and clear
	 * the contents</li>
	 * <li>Check the whether wifi statistics are enabled</li>
	 * <li>Verify WiFi log interval is 3600 by default using WebPA command</li>
	 * <li>Change the WiFi log interval to 300sec using WebPA command" and wait for
	 * 5 minutes</li>
	 * <li>Run the script sh /usr/ccsp/wifi/aphealth_log.sh</li>
	 * <li>Verify the log print for RSSI value for client device connected with 2.4
	 * GHz wifi band</li>
	 * <li>Verify the log print for bytes of AP RX for client device connected with
	 * 2.4 GHz wifi band</li>
	 * <li>Verify the log print for RX DELTA of current and previous value for
	 * client device connected with 2.4 GHz wifi band</li>
	 * <li>Change the WiFi log interval to default of 3600sec using WebPA
	 * command</li></li> enable prefer private via webpa</li>
	 * <li>enable prefer private via webpa</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author anandam.s
	 * @refactor Athira
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-WIFI-TELEMETRY-1009")
	public void verifyTelemetryMarkersFor5ghzClientsForPublicWifi(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-TELEMETRY-109";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		boolean statusOfEnablingWifi = false;
		boolean statusOfEnablePreferPrivate = false;
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1009");
		LOGGER.info("TEST DESCRIPTION: Verify enhanced Wifi logging in Telemetry for   5ghz Public  wifi parameters");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Enable Public wifi on the device");
		LOGGER.info("2. disable prefer private via webpa");
		LOGGER.info("3. Connect to Wifi client with 5Ghz Public wifi SSID  ");
		LOGGER.info("4. Verify wifihealth.txt file availability in /rdklogs/logs folder  and clear the contents");
		LOGGER.info("5. Check the whether wifi statistics are enabled ");
		LOGGER.info("6. Verify WiFi log interval is 3600 by default using WebPA command");
		LOGGER.info("7. Change the WiFi log interval to 300sec using WebPA command\" and wait for 5 minutes ");
		LOGGER.info("8. Run the script sh /usr/ccsp/wifi/aphealth_log.sh");
		LOGGER.info("9. Verify the log print for  RSSI value for client device connected with 5 GHz wifi band");
		LOGGER.info("10. Verify the log print for bytes of AP RX for  client device connected with 5 GHz wifi band");
		LOGGER.info(
				"11. Verify the log print for RX DELTA of current and previous value for  client device connected with 5 GHz wifi band");
		LOGGER.info("12. Change the WiFi log interval to default of 3600sec using WebPA command");
		LOGGER.info("13. enable prefer private via webpa");

		LOGGER.info("#######################################################################################");

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			BroadBandPreConditionUtils.executePreConditionToFactoryResetAndReacitivateDevice(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1, true);

			stepNum = "s1";
			errorMessage = "Attempt to enable Public wifi on device has failed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Enable Public wifi on the device");
			LOGGER.info("STEP 1: ACTION : Enable Public wifi on the device via webpa");
			LOGGER.info("STEP 1: EXPECTED : Public wifi should get enabled successfully");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
					BroadBandTestConstants.DUAL_BAND);
			statusOfEnablingWifi = status;
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully enabled Public wifi using webpa");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Attempt to disable prefer private has failed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : disable prefer private via webpa");
			LOGGER.info(
					"STEP 2: ACTION : Execute WebPa SET command on the object  Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate");
			LOGGER.info("STEP 2: EXPECTED : The prefer private should get disabled successfully");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PREFER_PRIVATE_FEATURE_STATUS,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Sucessfully disabled prefer private feature via webpa");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Failed to get a 2.4ghz Public wifi SSID  connected client device";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Connect to Wifi client with 5Ghz Public wifi SSID  ");
			LOGGER.info("STEP 3: ACTION : Connect the Wifi  client with Gateway:netsh wlan connect name=\"<ssid>\"");
			LOGGER.info("STEP 3: EXPECTED : Connected client should connect to Public wifi SSID succesfully");
			LOGGER.info("**********************************************************************************");
			Dut clientDut = null;

			try {
				clientDut = BroadBandConnectedClientUtils.getWiFiCapableClientDeviceAndConnectToPublicSsid(device,
						tapEnv, BroadBandConnectedClientTestConstants.SECURITY_MODE_OPEN.toLowerCase(),
						WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				status = (null != clientDut);
			} catch (Exception e) {
				errorMessage = "Exception while trying to connect a client to 5ghz Public WIFI";
				LOGGER.error(errorMessage + " . " + e.getMessage());
			}

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Successfully connected to Public wifi 5ghz radio");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			/** step 4 to Step 12 */
			executeCommonStepsForTelemetryTestCases(testCaseId, stepNum, device, WiFiFrequencyBand.WIFI_BAND_5_GHZ,
					BroadBandTestConstants.STRING_VALUE_SIX, clientDut);

			LOGGER.info("**********************************************************************************");

			stepNum = "s13";
			errorMessage = "Attempt to enable prefer private has failed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : enable prefer private via webpa");
			LOGGER.info(
					"STEP 13: ACTION : Execute WebPa SET command on the object  Device.WiFi.X_RDKCENTRAL-COM_PreferPrivate");
			LOGGER.info("STEP 13: EXPECTED : The prefer private should get enabled successfully");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PREFER_PRIVATE_FEATURE_STATUS,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			statusOfEnablePreferPrivate = status;
			if (status) {
				LOGGER.info("STEP 13: ACTUAL : Sucessfully enabled prefer private feature via webpa");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");

			LOGGER.info("POST-CONDITION STEPS");
			if (statusOfEnablingWifi) {
				LOGGER.info("POST-CONDITION : DESCRIPTION :Disable Public wifi. ");
				LOGGER.info("POST-CONDITION : ACTION : Execute set operation to enable public wifi");
				LOGGER.info("POST-CONDITION : EXPECTED :public wifi  should be in disabled state.  ");
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI, WebPaDataTypes.BOOLEAN.getValue(),
						BroadBandTestConstants.FALSE);
				if (status) {
					LOGGER.info("POST-CONDITION : Successfully disabled Public wifi. ");
				} else {
					LOGGER.info("POST-CONDITION : Failed to disable Public wifi. ");
				}
			}
			if (!statusOfEnablePreferPrivate) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_PREFER_PRIVATE_FEATURE_STATUS,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);

				if (status) {
					LOGGER.info("POST-CONDITION : Successfully enabled prefer private. ");
				} else {
					LOGGER.info("POST-CONDITION : Failed to enabled prefer private. ");
				}
			}
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1009");
	}

	/**
	 * Common steps for wifi telemtery verification test cases
	 * 
	 * @param testCaseId        test case Id
	 * @param step              step number
	 * @param device            {@link Dut}
	 * @param band              {@link WiFiFrequencyBand}
	 * @param accessPointNumber Access Point Number for Wifi.
	 * @param connectedClient   connected client to be passed
	 * @refactor Athira
	 */
	private void executeCommonStepsForTelemetryTestCases(String testCaseId, String step, Dut device,
			WiFiFrequencyBand band, String accessPointNumber, Dut connectedClient) {

		String errorMessage = null;
		boolean status = false;
		String stepNum = BroadBandCommonUtils.stepAdder(step);
		String radio = band.equals(WiFiFrequencyBand.WIFI_BAND_2_GHZ) ? "2.4Ghz" : "5Ghz";

		errorMessage = "wifihealth.txt is not present in the logs folder";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNum
				+ " : DESCRIPTION : Verify wifihealth.txt file availability in /rdklogs/logs folder  and clear the contents");
		LOGGER.info("STEP " + stepNum + " : ACTION : execute commandls /rdklogs/logs");
		LOGGER.info("STEP " + stepNum + " : EXPECTED : wifihealth.txt file must be present in /rdklogs/logs folder");
		LOGGER.info("**********************************************************************************");
		if (CommonMethods.isAtomSyncAvailable(connectedClient, tapEnv) && BroadBandCommonUtils
				.isFilePresentOnDeviceAtom(tapEnv, device, BroadBandTestConstants.FILE_WIFIHEALTH)) {
			tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CLEAR_WIFI_HEALTH_LOG);
			status = true;
		} else if (CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG)) {
			status = CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG);
		}
		if (status) {
			LOGGER.info("STEP " + stepNum
					+ " : ACTUAL : wifihealth.txt is present in /rdklogs/logs directory and its contents are cleared");
		} else {
			LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		stepNum = BroadBandCommonUtils.stepAdder(stepNum);
		errorMessage = "Wifi statistics are not enabled.Hence wifi health reporting will not happen";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNum + " : DESCRIPTION : Check the whether wifi statistics are enabled ");
		LOGGER.info("STEP " + stepNum
				+ " : ACTION : Execute webpa Device.WiFi.X_RDKCENTRAL-COM_vAPStatsEnabledmcli eRT getv Device.WiFi.X_RDKCENTRAL-COM_vAPStatsEnable");
		LOGGER.info("STEP " + stepNum + " : EXPECTED : Wifi statistics should be enabled ");
		LOGGER.info("**********************************************************************************");
		// get the default value of AP statistics
		String response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_STATS_ENABLE);
		if (CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.TRUE)) {
			status = true;
		} else {
			LOGGER.info(
					"The value of webpa Device.WiFi.X_RDKCENTRAL-COM_vAPStatsEnable is obtained as false. Hence setting it to true");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_STATS_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
		}
		if (status) {
			LOGGER.info("STEP " + stepNum + " : ACTUAL :  Enabling vAP stats was successful");
		} else {
			LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		stepNum = BroadBandCommonUtils.stepAdder(stepNum);
		errorMessage = "The default value of wifi log interval is not 3600";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP " + stepNum + " : DESCRIPTION : Verify WiFi log interval is 3600 by default using WebPA command");
		LOGGER.info("STEP " + stepNum
				+ " : ACTION : Execute Command Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval");
		LOGGER.info("STEP " + stepNum
				+ " : EXPECTED : The webPA command should execute successfully and return default value as 3600 seconds(1 hour)");
		LOGGER.info("**********************************************************************************");
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_LOG_INTERVAL);
		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_3600);
		if (status) {
			LOGGER.info("STEP " + stepNum
					+ " : ACTUAL : Verify using WebPA command that WiFi log interval is 3600 by default");
		} else {
			LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		stepNum = BroadBandCommonUtils.stepAdder(stepNum);
		errorMessage = "Failed to set the wifi log interval as 300 seconds ";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNum
				+ " : DESCRIPTION : Change the WiFi log interval to 300sec using WebPA command\" and wait for 5 minutes ");
		LOGGER.info("STEP " + stepNum
				+ " : ACTION : Execute WebPa set command for Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval");
		LOGGER.info("STEP " + stepNum + " : EXPECTED : WebPa set command should be successful");
		LOGGER.info("**********************************************************************************");
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_LOG_INTERVAL, BroadBandTestConstants.CONSTANT_1,
				BroadBandTestConstants.FIVE_MINUTES_IN_SECONDS);
		if (status) {
			LOGGER.info("STEP " + stepNum + " : ACTUAL :Changed the WiFi log interval to 300sec");
		} else {
			LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		stepNum = BroadBandCommonUtils.stepAdder(stepNum);
		errorMessage = "Failed to execute the script /usr/ccsp/wifi/aphealth_log.sh ";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNum + " : DESCRIPTION : Run the script sh /usr/ccsp/wifi/aphealth_log.sh ");
		LOGGER.info("STEP " + stepNum + " : ACTION : Run the script sh /usr/ccsp/wifi/aphealth_log.sh");
		LOGGER.info("STEP " + stepNum + " : EXPECTED : Script should be executed successfully");
		LOGGER.info("**********************************************************************************");
		try {
			if (CommonMethods.isAtomSyncAvailable(connectedClient, tapEnv)) {
				response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_RUN_APHEALTH_SCRIPT);
			} else {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_RUN_APHEALTH_SCRIPT);
			}
			status = !(response.contains("can't open /usr/ccsp/wifi/aphealth_log.sh"));
		} catch (Exception e) {
			LOGGER.error(errorMessage + "Exception : " + e.getMessage());
		}
		if (status) {
			LOGGER.info(
					"STEP " + stepNum + " : ACTUAL :Sucessfully executed the script at /usr/ccsp/wifi/aphealth_log.sh");
		} else {
			LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		LOGGER.info("Waiting for 12 mniutes for the logs to populate");
		tapEnv.waitTill(BroadBandTestConstants.TWELVE_MINUTE_IN_MILLIS);
		stepNum = BroadBandCommonUtils.stepAdder(stepNum);
		errorMessage = "Logs are not in expected format";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNum
				+ " : DESCRIPTION : Verify the log print for  RSSI value for client device connected with " + radio
				+ " wifi band");
		LOGGER.info("STEP " + stepNum
				+ " : ACTION : execute command cat wifihealth.txt | grep -i \"WIFI_RSSI_<AccessPoint No>\"");
		LOGGER.info("STEP " + stepNum + " : EXPECTED : Log should be printed in below format:" + "  # RSSI of CLients "
				+ radio + " Band" + "      WIFI_RSSI_1:<RSSI Of client1>,<RSSIof client2>..." + "      ");
		LOGGER.info("**********************************************************************************");
		status = BroadBandWiFiUtils.verifyTelemetryMarkers(device, accessPointNumber,
				BroadBandTraceConstants.LOG_MESSAGE_WIFI_RSSI, BroadBandTestConstants.PATTERN_FOR_TELEMETRY_MARKER);
		if (status) {
			LOGGER.info("STEP " + stepNum + " : ACTUAL : WIFI_RSSI value for " + radio
					+ " client device is printed as expected");
		} else {
			LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		stepNum = BroadBandCommonUtils.stepAdder(stepNum);
		errorMessage = "Logs are not in expected format";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNum
				+ " : DESCRIPTION : Verify the log print for bytes of AP RX for  client device connected with " + radio
				+ " wifi band");
		LOGGER.info("STEP " + stepNum
				+ " : ACTION : execute command cat wifihealth.txt | grep -i \"WIFI_RX_<AccessPoint No>:\"");
		LOGGER.info("STEP " + stepNum + " : EXPECTED : Log should be printed in below format:" + " # RX of " + radio
				+ "" + "      WIFI_RX_<AccessPoint No>:<VALUE IN MB>");
		LOGGER.info("**********************************************************************************");
		if (accessPointNumber.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE)
				|| accessPointNumber.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO)) {
			status = BroadBandWiFiUtils.verifyTelemetryMarkers(device, accessPointNumber,
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_RX, BroadBandTestConstants.PATTERN_FOR_TELEMETRY_MARKER);

			if (status) {
				LOGGER.info("STEP " + stepNum + " : ACTUAL : WIFI_RX value for " + radio
						+ "client device is printed as expected");
			} else {
				LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		} else {
			errorMessage = "STEP " + stepNum
					+ ": ACTUAL : VERIFYING TELEMETRY MARKERS WIFI_RX  NOT APPLICABLE FOR  PUBLIC WIFI ";
			LOGGER.info(errorMessage);
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
					errorMessage, false);

		}

		stepNum = BroadBandCommonUtils.stepAdder(stepNum);
		errorMessage = "Logs are not in expected format";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNum
				+ " : DESCRIPTION : Verify the log print for RX DELTA of current and previous value for  client device connected with "
				+ radio + " wifi band");
		LOGGER.info("STEP " + stepNum
				+ " : ACTION : execute command cat wifihealth.txt | grep -i \"WIFI_RXDELTA_<AccessPoint No>\"");
		LOGGER.info("STEP " + stepNum + " : EXPECTED : Log should be printed in below format:"
				+ "     # RX DELTA of current and previous value " + radio + ""
				+ "      WIFI_RXDELTA_<AccessPoint No>:<VALUE IN MB>");
		LOGGER.info("**********************************************************************************");
		if (accessPointNumber.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE)
				|| accessPointNumber.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO)) {
			status = BroadBandWiFiUtils.verifyTelemetryMarkers(device, accessPointNumber,
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_RXDELTA,
					BroadBandTestConstants.PATTERN_FOR_TELEMETRY_MARKER);
			if (status) {
				LOGGER.info("STEP " + stepNum + " : ACTUAL : WIFI_RXDELTA value for " + radio
						+ "client device is printed as expected");
			} else {
				LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		} else {
			errorMessage = "STEP " + stepNum
					+ ": ACTUAL : VERIFYING TELEMETRY MARKERS WIFI_RXDELTA NOT APPLICABLE FOR PUBLIC WIFI ";
			LOGGER.info(errorMessage);
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
					errorMessage, false);

		}

		stepNum = BroadBandCommonUtils.stepAdder(stepNum);
		errorMessage = "Failed to revert the log interval using webpa";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNum
				+ " : DESCRIPTION : Change the WiFi log interval to default of 3600sec using WebPA command");
		LOGGER.info("STEP " + stepNum
				+ " : ACTION : Execute WebPa set command for Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval WebPa parameter");
		LOGGER.info("STEP " + stepNum + " : EXPECTED : WebPa set command should be successful");
		LOGGER.info("**********************************************************************************");
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_LOG_INTERVAL, BroadBandTestConstants.CONSTANT_1,
				BroadBandTestConstants.STRING_CONSTANT_3600);

		if (status) {
			LOGGER.info("STEP " + stepNum
					+ " : ACTUAL : Successfully reverted the WiFi log interval to default of 3600sec using WebPA command");
		} else {
			LOGGER.error("STEP " + stepNum + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	}

	/**
	 * Verify that Wi-Fi connectivity of 2.4GHz frequency is not affected when 5GHz
	 * frequency Radio is disabled
	 * <ol>
	 * <li>STEP 1:Verify WIFI Private Radio and wifi status</li>
	 * <li>STEP 2:Disable Private 2.4 GHz Radio via WebPA</li>
	 * <li>STEP 3:Verify the Private 2.4 GHz Radio Disabled status via WebPA</li>
	 * <li>STEP 4:Verify the Private 5GHz Radio status via WebPA</li>
	 * <li>STEP 5:Verify the Wifi status</li>
	 * <li>STEP 6:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error
	 * like \"Failed to get parameter value of\"</li>
	 * <li>STEP 7:Verify dmcli eRT getv Device.WiFi. - has to give more than 23
	 * params.</li>
	 * <li>STEP 8: Connect the device to 5 GHz SSID and verify connection
	 * status</li>
	 * <li>STEP 9:Verify whether interface got the correct IPv4 address.</li>
	 * <li>STEP 10:Verify whether interface got the correct IPv6 address.</li>
	 * <li>STEP 11: Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * <li>STEP 12: Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * <li>STEP 13: Enable Private 2.4 GHz Radio via WebPA</li>
	 * <li>STEP 14:Disable Private 5 GHz Radio via WebPA</li>
	 * <li>STEP 15:Verify the Private 5 GHz Radio Disabled status via WebPA</li>
	 * <li>STEP 16:Verify the Private 2.4GHz Radio status via WebPA</li>
	 * <li>STEP 17:Verify the Wifi status</li> LOGGER.info( "STEP 18:Verify in
	 * /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get
	 * parameter value of\"</li>
	 * <li>STEP 19: Verify dmcli eRT getv Device.WiFi. - has to give more than 23
	 * params.</li>
	 * <li>STEP 20: Connect the device to 2.4 GHz SSID and verify connection
	 * status</li>
	 * <li>STEP 21:Verify whether interface got the correct IPv4 address.</li>
	 * <li>STEP 22:Verify whether interface got the correct IPv6 address.</li>
	 * <li>STEP 23: Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * <li>STEP 24: Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * <li>STEP 25:Enable Private 5 GHz Radio via WebPA</li>
	 * <li>Postcondition1:Revert WIFI radio to its original state</li>
	 * </ol>
	 * 
	 * @author Deepika Sekar
	 * @param device {@link Dut}
	 * @Refactor Sruthi Santhosh
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-RADIO-5001")
	public void testVerifyConnectivityOfClientDeviceWhenEnableOrDisablingRadio(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-RADIO-501";
		// Test step number
		int step = 1;
		// String to store the error message
		String errorMessage = null;
		String radioStepNumber = "s1";
		String deviceDateTime = null;
		try {

			LOGGER.info("STARTING TEST CASE: " + testId);
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"TEST DESCRIPTION: Test toVerify that Wi-Fi connectivity of 2.4GHz frequency is not affected when 5GHz frequency radio is disabled");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("STEP 1:Verify WIFI Private Radio and wifi status");
			LOGGER.info("STEP 2:Disable Private 2.4 GHz Radio via WebPA ");
			LOGGER.info("STEP 3:Verify the Private 2.4 GHz Radio Disabled status via WebPA ");
			LOGGER.info("STEP 4:Verify the Private 5GHz Radio  status via WebPA");
			LOGGER.info("STEP 5:Verify the Wifi status");
			LOGGER.info(
					"STEP 6:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
			LOGGER.info("STEP 7:Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
			LOGGER.info("STEP 8: Connect the device to 5 GHz SSID and verify connection status");
			LOGGER.info("STEP 9:Verify whether interface got the correct IPv4  address.");
			LOGGER.info("STEP 10:Verify whether interface got the correct IPv6  address.");
			LOGGER.info("STEP 11: Verify whether you have connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP 12: Verify whether you have connectivity using that particular interface using IPV6 ");
			LOGGER.info("STEP 13: Enable Private 2.4 GHz Radio via WebPA");
			LOGGER.info("STEP 14:Disable Private 5 GHz Radio via WebPA");
			LOGGER.info("STEP 15:Verify the Private 5 GHz Radio Disabled status via WebPA ");
			LOGGER.info("STEP 16:Verify the Private 2.4GHz Radio  status via WebPA ");
			LOGGER.info("STEP 17:Verify the Wifi status");
			LOGGER.info(
					"STEP 18:Verify in /rdklogs/logs/WiFilog,txt.0 - should not observe error like \"Failed to get parameter value of\"");
			LOGGER.info("STEP 19: Verify  dmcli eRT getv Device.WiFi. - has to give more than 23 params.");
			LOGGER.info("STEP 20: Connect the device to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP 21:Verify whether interface got the correct IPv4  address.");
			LOGGER.info("STEP 22:Verify whether interface got the correct IPv6  address.");
			LOGGER.info("STEP 23: Verify whether you have connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP 24: Verify whether you have connectivity using that particular interface using IPV6 ");
			LOGGER.info("STEP 25:Enable Private 5 GHz Radio via WebPA");
			LOGGER.info("Postcondition1:Revert WIFI radio to its original state");
			LOGGER.info("######################################################################################");
			/**
			 * Steps 1
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + step + ": DESCRIPTION:Verify WIFI Private Radio and wifi status");
			LOGGER.info("STEP " + step + ": ACTION: Execute WebPA command to\r\n" + "verify Private radio  status\r\n"
					+ "Device.WiFi.Radio.10001.Status,Device.WiFi.Radio.10101.Status,Device.WiFi.Status");
			LOGGER.info("STEP " + step
					+ ": EXPECTED: Device should return the  Private Radioand wifi enabled status as \"Up\".");
			LOGGER.info("**********************************************************************************");

			errorMessage = "Either one radio or all status is not \"up\"";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_2_4GHZ,
					BroadBandConnectedClientTestConstants.RADIO_STATUS_UP, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
					&& BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_5GHZ,
							BroadBandConnectedClientTestConstants.RADIO_STATUS_UP,
							BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
					&& BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_STATUS,
							BroadBandConnectedClientTestConstants.RADIO_STATUS_UP,
							BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + step + ": ACTUAL :  WIFI Private Radio and wifi status are Up");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, true);
			// common steps to disable radio and verify wifi status
			/**
			 * Steps 2-13
			 */
			step = verifyRadioStatus(device, testId, tapEnv, step, deviceDateTime,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_2_4GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_5GHZ);
			/**
			 * Steps 14-25
			 */
			step = verifyRadioStatus(device, testId, tapEnv, step, deviceDateTime,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_5GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_2_4GHZ);

		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, radioStepNumber, status, errorMessage,
					true);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			BroadBandPostConditionUtils.executePostConditionToVerifyDefaultRadioStatus(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}

		LOGGER.info("ENDING TESTCASE :TC-RDKB-WIFI-RADIO-5001 ");

	}

	/**
	 * Common method to check the accessibility of the given websites
	 * 
	 * @param device             device instance
	 * @param testCaseId         test case id
	 * @param tapEnv             instance of {@link AutomaticsTapApi}
	 * @param step               current stepNumber
	 * @param disableParam       Radio to be disabled
	 * @param disableParamStatus param to verify disabled status
	 * @param enableParamStatus  param to verify enabled status
	 * @return int returns current step number
	 */
	public int verifyRadioStatus(Dut device, String testCaseId, AutomaticsTapApi tapEnv, int step,
			String deviceDateTime,

			String disableParam, String disableParamStatus, String enableParamStatus) {

		LOGGER.debug("STARTING METHOD : verifyRadioStatus()");
		BroadBandResultObject result = new BroadBandResultObject();
		result.setStatus(false);
		boolean status = false;
		String errorMessage = null;
		WebPaParameter webPaParameter = new WebPaParameter();
		Dut connectedDeviceActivated = null;
		try {

			step++;
			String radioStepNumber = "s" + step;
			status = false;
			errorMessage = "Failed to disable" + disableParam + " private wifi";
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + step + ": DESCRIPTION: Disable " + disableParam + " Radio via WebPA ");
			LOGGER.info("STEP " + step + " : ACTION: Execute WebPA command to disable " + disableParam);
			LOGGER.info("STEP " + step + " : Device should disable the Private " + disableParam
					+ "  and WebPA command return success response.");
			LOGGER.info("******************************************************");
			String applySetting = null;
			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv, disableParam,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
			if (disableParam.contains(String.valueOf(BroadBandTestConstants.CONSTANT_1000))) {
				applySetting = BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_APPLY_SETTING;
			} else {
				applySetting = BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING;
			}
			webPaParameter.setName(applySetting);
			webPaParameter.setValue(BroadBandTestConstants.TRUE);
			webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
			status = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);

			if (status) {
				LOGGER.info("STEP " + step + ": ACTUAL : " + disableParam + " is disabled");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, radioStepNumber, status, errorMessage, true);

			step++;
			radioStepNumber = "s" + step;
			status = false;
			errorMessage = disableParamStatus + " private  wifi status is not Down";
			LOGGER.info("******************************************************");
			LOGGER.info(
					"STEP " + step + ": DESCRIPTION:Verify the Private " + disableParamStatus + " status via WebPA ");
			LOGGER.info("STEP " + step + " : ACTION: Execute WebPA command to verify " + disableParamStatus);
			LOGGER.info("STEP " + step + ": EXPECTED: Device should return the  Private " + disableParamStatus
					+ "  as \"Down\"");
			LOGGER.info("******************************************************");
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv, disableParamStatus,
					BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + step + ": ACTUAL :  WIFI Private Radio " + disableParamStatus + " is Down");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, radioStepNumber, status, errorMessage, true);

			step++;
			radioStepNumber = "s" + step;
			status = false;
			errorMessage = enableParamStatus + " private wifi status is not Up";
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + step + ": DESCRIPTION: Verify the " + enableParamStatus + " via WebPA  ");
			LOGGER.info("STEP " + step + ": ACTION: Execute WebPA command to verify Private " + enableParamStatus
					+ "  \r\n" + enableParamStatus);
			LOGGER.info("STEP " + step + ": Device should return the  Private " + enableParamStatus
					+ " enabled  as \"Up\"");
			LOGGER.info("******************************************************");
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv, enableParamStatus,
					BroadBandConnectedClientTestConstants.RADIO_STATUS_UP, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP " + step + ": ACTUAL : Private  " + enableParamStatus + " is Up");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, radioStepNumber, status, errorMessage, true);

			if (!disableParam.contains("SSID")) {
				step++;
				radioStepNumber = "s" + step;
				status = false;
				errorMessage = "Wifi status is not Down";
				LOGGER.info("******************************************************");
				LOGGER.info("STEP " + step + ": DESCRIPTION:Verify the Wifi status ");
				LOGGER.info("STEP " + step + " : ACTION: Execute WebPA command to Device.WiFi.Status");
				LOGGER.info("STEP " + step + ": EXPECTED: Wifi status should be down");
				LOGGER.info("******************************************************");

				status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_STATUS,
						BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN,
						BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info("STEP " + step + ": ACTUAL :  wifi status is Down");
				} else {
					LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, radioStepNumber, status, errorMessage, true);
			}

			step = wifiDmcliParseCheck(device, tapEnv, testCaseId, deviceDateTime, step);

			step++;
			radioStepNumber = "s" + step;
			status = false;
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + step
					+ ": Description : Connect the device to broadcasted SSID and verify connection status");
			LOGGER.info("STEP " + step + ": ACTION: Connect A Client with broadcasted wifi network");
			LOGGER.info("STEP " + step + ": EXPECTED: Device should be connected with broadcasted wifi network");
			LOGGER.info("******************************************************");

			tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
			errorMessage = "Connection to broadcasted wifi failed";

			if (disableParam.contains(String.valueOf(BroadBandTestConstants.CONSTANT_1000))) {
				connectedDeviceActivated = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			} else {
				connectedDeviceActivated = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			}
			if (null != connectedDeviceActivated) {
				status = true;
			} else {
				errorMessage = "Unable to connect to broadcasted private SSID when both radios are enabled";
			}
			tapEnv.updateExecutionStatus(device, testCaseId, radioStepNumber, status, errorMessage, true);

			/** Steps */
			BroadBandConnectedClientUtils.checkIpAddressAndConnectivity(device, tapEnv, connectedDeviceActivated,
					testCaseId,
					new String[] { "s" + (step + 1), "s" + (step + 2), "s" + (step + 3), "s" + (step + 4) });

			step = step + 5;
			radioStepNumber = "s" + step;
			status = false;
			errorMessage = "Failed to enable private " + disableParam;
			LOGGER.info("******************************************************");
			LOGGER.info("STEP " + step + ": DESCRIPTION: Enable Private " + disableParam + " via WebPA ");
			LOGGER.info("STEP " + step + " : ACTION: Execute WebPA command to Enable Private " + disableParam);
			LOGGER.info("STEP " + step + " : Device should Enable the Private " + disableParam
					+ " and WebPA command return success response.");
			LOGGER.info("******************************************************");
			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv, disableParam,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			webPaParameter.setName(applySetting);
			webPaParameter.setValue(BroadBandTestConstants.TRUE);
			webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
			status = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
			if (status) {
				LOGGER.info("STEP " + step + ": ACTUAL : Private " + disableParam + " is enabled");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, radioStepNumber, status, errorMessage, true);

		} catch (Exception e) {
			LOGGER.error("Failed to check radio status" + e.getMessage());
			throw new TestException("Failed to check radio status" + errorMessage);

		}

		LOGGER.debug("ENDING METHOD : verifyRadioStatus()");
		return step;
	}

	/**
	 * Test to verify DCHP range assigning of cable modem with connected clients
	 * <ol>
	 * <li>1:Verify the Private Wi-Fi SSIDs are enabled using WebPA</li>
	 * <li>2:Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>3:Verify if the wireless connected client has an IPv4 address from the
	 * gateway</li>
	 * <li>4:Verify if the wireless connected client 1 has an IPv6 address.</li>
	 * <li>5:Connect the client 2 to 5 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>6:Verify if the wireless connected client has an IPv4 address from the
	 * gateway</li>
	 * <li>7:Verify if the wireless connected client 2 has an IPv6 address.</li>
	 * <li>8:Verify the client 3 connected to ethernet has IPv4 Address assigned
	 * from DHCP</li>
	 * <li>9:Verify if the ethernet connected client has an IPv6 address.</li>
	 * <li>10:Get DHCP beginning address ,DHCP ending address ,DHCP lease time Using
	 * Webpa</li>
	 * <li>11:Set DHCP beginning address to 10.0.0.2, DHCP ending address to
	 * 10.0.0.3, DHCP lease time to 2 minutes.</li>
	 * <li>12:Verify 2 of 3 Connected clients Should have IP address in Updated DHCP
	 * range with IP renew Operation ,if IP not renewed</li>
	 * </ol>
	 * 
	 * @refactor yamini.s
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-DHCP-CLIENTS-1001")

	public void verifyDHCPRangeAssignInConnectedClients(Dut device) {
		boolean status = false;// String to store the test case status
		String testId = "TC-RDKB-DHCP-CLIENTS-101";// Test case id
		String testStep = null;// Test step number
		String errorMessage = null;// String to store the error message
		String dhcpMinAddr = null;// String to store DHCP minimum address
		String dhcpMaxAddr = null;// String to store DHCP maximum address
		String leaseTime = null;// String to store lease time
		Integer statusCounter = 0;// String to store count of DHCP address obtained in clients
		String wifi5GhzStatus = null;// boolean to store wifi 2.4ghz status
		String wifi24GhzStatus = null;// boolean to store wifi 5ghz status
		Dut wifi5GhzClient = null;// Dut wifi client 5Ghz
		Dut wifi24GhzClient = null;// Dut wifi client 2.4Ghz
		String setDhcpMaxValue = null;// string to store dhcp max value to set
		String setDhcpMinValue = null;// string to store dhcp min value to set
		Dut ethernetClient = null;
		try {
			LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-DHCP-CLIENTS-1001#####################");
			LOGGER.info("TEST DESCRIPTION:Test to verify DCHP range assigning of cable modem with connected clients ");

			LOGGER.info("TEST STEPS : ");
			LOGGER.info("1.Verify the Private Wi-Fi SSIDs are enabled using WebPA");
			LOGGER.info("2.Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify connection status ");
			LOGGER.info("3.Verify if the wireless connected client has an IPv4 address from the gateway");
			LOGGER.info("4.Verify if the wireless connected client 1 has an  IPv6 address.");
			LOGGER.info("5.Connect the client 2 to 5 GHz Private Wi-Fi Network and verify connection status ");
			LOGGER.info("6.Verify if the wireless connected client has an IPv4 address from the gateway");
			LOGGER.info("7.Verify if the wireless connected client 2 has an  IPv6 address.");
			LOGGER.info("8.Verify the client 3 connected to ethernet has IPv4 Address assigned from DHCP");
			LOGGER.info("9.Verify if the ethernet connected client has an  IPv6 address.");
			LOGGER.info("10.Get DHCP beginning address ,DHCP ending  address ,DHCP lease time Using Webpa");
			LOGGER.info(
					"11.Set DHCP beginning address to 10.0.0.2, DHCP ending address to 10.0.0.3, DHCP lease time to 2 minutes");
			LOGGER.info(
					"12.Verify 2 of 3 Connected clients Should have IP address in Updated DHCP range with IP renew Operation ,if IP not renewed");
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"######################################### STARTING PRE-CONFIGURATIONS #########################################");
			LOGGER.info("PRE-CONFIGURATIONS TEST STEPS : ");
			LOGGER.info("1.Verify whether Private SSID 2.4Ghz can be enabled using webPA");
			LOGGER.info("2.Verify whether Private SSID 5Ghz can be enabled using webPA");
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION 1: Verify whether Private SSID 2.4Ghz can be enabled using webPA");
			LOGGER.info("PRE-CONDITION 1: EXPECTED: Private SSID 2.4Ghz should be enabled successfully");
			errorMessage = "Unable to enable 2.4Ghz private SSID using webpa";
			LOGGER.info("##########################################################################");
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (!status) {
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION 2: Verify whether Private SSID 5Ghz can be enabled using webPA");
			LOGGER.info("PRE-CONDITION 2: EXPECTED: Private SSID 5Ghz should be enabled successfully");
			LOGGER.info("##########################################################################");
			errorMessage = "Unable to enable 5Ghz private SSID using webpa";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (!status) {
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			/**
			 * STEP 1:Verify and enable the Private Wi-Fi SSIDs 2.4Ghz and 5Ghz using WebPA
			 */
			status = false;
			testStep = "s1";
			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 1 : DESCRIPTION :Verify the Private Wi-Fi SSIDs are enabled using WebPA");
			LOGGER.info("STEP 1 : ACTION:Enable 2.4Ghz and 5Ghz Radio SSID of the device ");
			LOGGER.info("STEP 1 : EXPECTED:Private SSID's 2.4Ghz and 5Ghz should be enabled successfully");
			LOGGER.info("##########################################################################");
			errorMessage = "Unable to enable 2.4Ghz radio SSID using webpa";
			wifi24GhzStatus = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS);
			wifi5GhzStatus = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS);
			if (wifi24GhzStatus.equals(
					BroadBandWebPaConstants.RdkBWifiParameters.SSID_ENABLE_STATUS_2GHZ_PRIVATE_WIFI.getDefaultValue())
					&& wifi5GhzStatus
							.equals(BroadBandWebPaConstants.RdkBWifiParameters.SSID_ENABLE_STATUS_5GHZ_PRIVATE_WIFI
									.getDefaultValue())) {
				status = true;
				LOGGER.info("STEP 1:ACTUAL:Private wifi SSID's 2.4Ghz and 5Ghz are enabled successfully");
			} else {
				LOGGER.error("STEP 1:ACTUAL:" + errorMessage);
			}

			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

			/**
			 * STEP 2:Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify
			 * connection status
			 */
			status = false;
			testStep = "s2";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 2 : DESCRIPTION :Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify connection status ");
			LOGGER.info("STEP 2 : ACTION:Connect connected client 1 to 2.4Ghz wifi of the devie");
			LOGGER.info(
					"STEP 2 : EXPECTED:Connection between connected client 1 to 2.4Ghz private SSID should be successful");
			LOGGER.info("##########################################################################");
			errorMessage = "Unable to establish connection with 2.4Ghz private SSID";
			wifi24GhzClient = BroadBandConnectedClientUtils
					.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			if (null != wifi24GhzClient) {
				status = true;
				LOGGER.info("STEP 2:ACTUAL:Connected establishment client 1 to 2.4 private SSID is successful");
			} else {
				LOGGER.error("STEP 2:ACTUAL:" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

			/**
			 * STEP 3:Verify if the wireless connected client has an IPv4 address from the
			 * gateway
			 */
			status = false;
			testStep = "s3";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 3 : DESCRIPTION :Verify if the wireless connected client has an IPv4 address from the gateway");
			LOGGER.info("STEP 3 : ACTION: Retrieve IP address from connected client and verfiy it");
			LOGGER.info("STEP 3 : EXPECTED:Connected client should have IP address in gateway range");
			LOGGER.info("##########################################################################");
			errorMessage = "Connected client IPv4 address in not in DHCP range";
			status = BroadBandConnectedClientUtils
					.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device, tapEnv, wifi24GhzClient).isStatus();
			if (status) {
				LOGGER.info("STEP 3:ACTUAL:Connected client 1 IP address is in DHCP range");
			} else {
				LOGGER.error("STEP 3:ACTUAL:" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

			/**
			 * STEP 4:Verify if the wireless connected client 1 has an IPv6 address.
			 * 
			 */
			testStep = "s4";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 4:DESCRIPTION: Verify if the wireless connected client 1 has an  IPv6 address.");
			LOGGER.info("STEP 4:ACTION : Connected client should get the IPV6 Interface");
			LOGGER.info("STEP 4:EXPECTED:Interface IPv6 address should  be shown");
			LOGGER.info("#####################################################################################");
			errorMessage = "Unable to verify IPv6 address in connected client";
			String osType = ((Device) wifi24GhzClient).getOsType();
			if (CommonMethods.isNotNull(osType)) {
				status = BroadBandConnectedClientUtils
						.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType, wifi24GhzClient, tapEnv);
			}
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Connected client IPv6 address is validated successfully");
			} else {
				LOGGER.error("STEP 4: ACTUAL :" + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

			/**
			 * STEP 5:Connect the client 2 to 5 GHz Private Wi-Fi Network and verify
			 * connection status
			 */
			status = false;
			testStep = "s5";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 5 : DESCRIPTION :Connect the client 2 to 5 GHz Private Wi-Fi Network and verify connection status ");
			LOGGER.info("STEP 5 : ACTION:Connect Connected client 2 to 5Ghz Wifi SSID");
			LOGGER.info(
					"STEP 5 : EXPECTED:Connection between connected client 2 to 5Ghz private SSID should be successful");
			LOGGER.info("##########################################################################");

			errorMessage = "Unable to establish connection with 5Ghz private SSID";
			wifi5GhzClient = BroadBandConnectedClientUtils.getOtherWiFiCapableClientDeviceAndConnect(device, tapEnv,
					wifi24GhzClient, BroadBandTestConstants.BAND_5GHZ);
			if (null != wifi5GhzClient) {
				status = true;
				LOGGER.info("STEP 5:ACTUAL:Connected client-client 2 connected to 5Ghz wifi ssid successfully");
			} else {
				LOGGER.error("STEP 5:ACTUAL:" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
			/**
			 * STEP 6:Verify if the wireless connected client 2 has an IPv4 address from the
			 * gateway
			 */
			status = false;
			testStep = "s6";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 6 : DESCRIPTION :Verify if the wireless connected client 2 has an IPv4 address from the gateway");
			LOGGER.info("STEP 6 : ACTION:Retrieve IP address from connected client 2 and verfiy it ");
			LOGGER.info("STEP 6 : EXPECTED:Connected client 2 should have IP address in gateway range");
			LOGGER.info("##########################################################################");
			errorMessage = "Connected client 2 IP address in not in DHCP range";
			status = BroadBandConnectedClientUtils
					.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device, tapEnv, wifi5GhzClient).isStatus();
			if (status) {
				LOGGER.info("STEP 6:ACTUAL:Connected client 2 IP address is in DHCP range");
			} else {
				LOGGER.error("STEP 6:ACTUAL:" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
			/**
			 * STEP 7:Verify if the wireless connected client 2 has an IPv6 address.
			 * 
			 */
			testStep = "s7";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 7:DESCRIPTION: Verify if the wireless connected client 2 has an  IPv6 address.");
			LOGGER.info("STEP 7:ACTION : Connected client should get the IPV6 Interface");
			LOGGER.info("STEP 7:EXPECTED:Interface IPv6 address should  be shown");
			LOGGER.info("#####################################################################################");
			errorMessage = "Unable to validate IPv6 address of connected client";
			osType = ((Device) wifi5GhzClient).getOsType();
			if (CommonMethods.isNotNull(osType)) {
				status = BroadBandConnectedClientUtils
						.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType, wifi5GhzClient, tapEnv);
			}
			if (status) {
				LOGGER.info("STEP 7: ACTUAL : IPv6 address of connected client 2 is validated successfully");
			} else {
				LOGGER.error("STEP 7: ACTUAL :" + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 8:Verify the client 3 connected to ethernet has IPv4 Address assigned
			 * from DHCP
			 */
			status = false;
			testStep = "s8";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 8 : DESCRIPTION :Verify the client 3 connected to ethernet has IP Address assigned from DHCP");
			LOGGER.info("STEP 8 : ACTION:Retrieve IP address of connected client 3 and verify it");
			LOGGER.info("STEP 8 : EXPECTED:Connected client 3 IP address should be in DHCP range");
			LOGGER.info("##########################################################################");
			errorMessage = "Ethernet client IP address is not in DHCP range";
			ethernetClient = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			if (null != ethernetClient) {
				status = BroadBandConnectedClientUtils
						.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device, tapEnv, ethernetClient).isStatus();
			}
			if (status) {
				LOGGER.info("STEP 8:ACTUAL:Connected client 3 IP address is in DHCP range");
			} else {
				LOGGER.error("STEP 8:ACTUAL:" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
			/**
			 * STEP 9:Verify if the ethernet connected client has an IPv6 address.
			 * 
			 */
			testStep = "s9";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 9:DESCRIPTION: Verify if the ethernet connected client has an  IPv6 address.");
			LOGGER.info("STEP 9:ACTION : Connected client should get the IPV6 Interface");
			LOGGER.info("STEP 9:EXPECTED:Interface IPv6 address should  be shown");
			LOGGER.info("#####################################################################################");
			errorMessage = "Unable to verify IPv6 address of connected client";
			osType = ((Device) ethernetClient).getOsType();
			status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
					ethernetClient, tapEnv);
			if (status) {
				LOGGER.info("STEP 9: ACTUAL : IPv6 address in connected client is validated successfully");
			} else {
				LOGGER.error("STEP 9: ACTUAL :" + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
			/**
			 * STEP 10:Get DHCP beginning address and Change the DHCP beginning address to
			 * 10.0.0.2
			 */
			status = false;
			testStep = "s10";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 10 : DESCRIPTION :Get DHCP beginning address ,DHCP ending  address ,DHCP lease time Using Webpa");
			LOGGER.info(
					"STEP 10 : ACTION:Execute Webpa command with Parameter :Device.DHCPv4.Server.Pool.1.MinAddress,Device.DHCPv4.Server.Pool.1.MaxAddress,Device.DHCPv4.Server.Pool.1.LeaseTime ");
			LOGGER.info("STEP 10 : EXPECTED:Webpa get request should be successful");
			LOGGER.info("##########################################################################");
			errorMessage = "Unable to perform webpa get request on device";
			String[] webPaParametersForDhcp = {
					BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_STARTING_IP_ADDRESS,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_ENDING_IP_ADDRESS,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME };
			Map<String, String> webPaGetResponse = tapEnv.executeMultipleWebPaGetCommands(device,
					webPaParametersForDhcp);
			if (!webPaGetResponse.isEmpty()) {
				dhcpMinAddr = webPaGetResponse
						.get(BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_STARTING_IP_ADDRESS);
				dhcpMaxAddr = webPaGetResponse
						.get(BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_ENDING_IP_ADDRESS);
				leaseTime = webPaGetResponse.get(BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME);
				if (CommonMethods.isIpv4Address(dhcpMinAddr) && CommonMethods.isIpv4Address(dhcpMaxAddr)
						&& CommonMethods.isNotNull(leaseTime)) {
					status = true;
				}
			}
			if (status) {
				LOGGER.info("STEP 10:ACTUAL:Webpa get request is performed successfully ");
			} else {
				LOGGER.error("STEP 10:ACTUAL:" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

			/**
			 * STEP 11:Set DHCP beginning address to 10.0.0.2, DHCP ending address to
			 * 10.0.0.3, DHCP lease time to 2 minutes using webpa
			 */
			status = false;
			testStep = "s11";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 11 : DESCRIPTION :Set DHCP beginning address to 10.0.0.2, DHCP ending address to 10.0.0.3, DHCP lease time to 2 minutes using webpa");
			LOGGER.info(
					"STEP 11 : ACTION:Execute Webpa Set command for DHCP Min address,Max Address and Lease time with values");
			LOGGER.info("STEP 11 : EXPECTED:Webpa set request should be successful with status code 200");
			LOGGER.info("##########################################################################");
			errorMessage = "Failed to change DHCP ending address 10.0.0.3 with webpa request";
			if (DeviceModeHandler.isBusinessClassDevice(device)) {
				setDhcpMinValue = AutomaticsPropertyUtility
						.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_STRING_DHCP_MIN_ADDRESS_BUSSI);
				setDhcpMaxValue = AutomaticsPropertyUtility
						.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_STRING_DHCP_MAX_ADDRESS_BUSSI);
			} else {
				setDhcpMinValue = AutomaticsPropertyUtility
						.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_STRING_DHCP_MIN_ADDRESS);
				setDhcpMaxValue = AutomaticsPropertyUtility
						.getProperty(BroadBandPropertyKeyConstants.PROP_KEY_STRING_DHCP_MAX_ADDRESS);
			}

			Boolean status1 = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_STARTING_IP_ADDRESS, 0, setDhcpMinValue);
			Boolean status2 = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_ENDING_IP_ADDRESS, 0, setDhcpMaxValue);
			Boolean status3 = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME, 1,
					BroadBandTestConstants.STRING_LEASE_TIME_VALUE);
			status = status1 && status2 && status3;
			if (status) {
				LOGGER.info("STEP 11:ACTUAL:DHCP ending address is changed successfully ");
			} else {
				LOGGER.error("STEP 11:ACTUAL:" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
			/**
			 * STEP 12:Verify 2 of 3 Connected clients Should have IP address in Updated
			 * DHCP range with IP renew Operation ,if IP not renewed.
			 */
			status = false;
			testStep = "s12";
			LOGGER.info("##########################################################################");
			LOGGER.info(
					"STEP 12 : DESCRIPTION :Verify 2 of 3 Connected clients Should have IP address in Updated DHCP range with IP renew Operation ,if IP not renewed");
			LOGGER.info("STEP 12 : ACTION:Retrieve IPv4 from connected clients and Verfiy it is in DHCP range");
			LOGGER.info("STEP 12 : EXPECTED:IPv4 address should be retrieved successfully and validated");
			LOGGER.info("##########################################################################");

			if (BroadBandConnectedClientUtils
					.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device, tapEnv, ethernetClient).isStatus()) {
				LOGGER.info("Ethernet client has IP address in DHCP range");
				statusCounter++;
			} else {
				errorMessage = "Failure in validating Ethernet Client IP address in DHCP range \n";
			}
			BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv, wifi24GhzClient,
					WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			if (BroadBandConnectedClientUtils
					.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device, tapEnv, wifi24GhzClient).isStatus()) {
				LOGGER.info("Wifi Connected client 1 has IP address in DHCP range");
				statusCounter++;
			} else {
				errorMessage += "Failure in validating Wifi Connected Client 1 IP address in DHCP range \n";
			}
			BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv, wifi5GhzClient,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			if (BroadBandConnectedClientUtils
					.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device, tapEnv, wifi5GhzClient).isStatus()) {
				LOGGER.info("Wifi Connected client 2 has IP address in DHCP range");
				statusCounter++;
			} else {
				errorMessage += "Failure in validating Wifi Connected Client 2 IP address in DHCP range \n";
			}

			errorMessage += "Unable to validate connected clients is in DHCP range " + statusCounter
					+ " Client/s are in DHCP range";
			if (statusCounter == 2) {
				status = true;
				LOGGER.info(
						"STEP 12:ACTUAL : Validation is success - 2 of 3 Connected clients IPv4 address are in DHCP range  ");
			} else {
				LOGGER.error("STEP 12:ACTUAL :" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Failed in executing verifyDHCPRangeAssignInConnectedClients \n" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
		} finally {
			LOGGER.info("###############################STARTING POST-CONFIGURATIONS###############################");
			LOGGER.info("POST-CONDITION 1: DESCRIPTION:RESTORE VALUES OF DHCP MIN ADDRESS");
			LOGGER.info(
					"POST CONDITION 1: ACTION:EXECUTE WEBPA COMMAND PARAMETER:Device.DHCPv4.Server.Pool.1.MinAddress with Value retrieved");
			LOGGER.info("POST-CONDITION 1: EXPECTED:VALUES OF DHCP MIN ADDRESS SHOULD BE RESTORED SUCCESSFULLY");
			LOGGER.info("##########################################################################");
			status = false;
			try {
				if (CommonMethods.isNotNull(dhcpMinAddr)) {
					status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_STARTING_IP_ADDRESS,
							BroadBandTestConstants.CONSTANT_0, dhcpMinAddr,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				}
				LOGGER.info("POST CONDITION 1:ACTUAL:DHCP MIN ADDRESS RESTORATION STATUS: " + status);
			} catch (Exception e) {
				LOGGER.error("EXCEPTION OCCURED WHILE CONFIGURING POST CONDITION 1:\n" + e.getMessage());
			}
			LOGGER.info("##########################################################################");
			LOGGER.info("POST-CONDITION 2: DESCRIPTION:RESTORE VALUES OF DHCP MAX ADDRESS");
			LOGGER.info(
					"POST CONDITION 2: ACTION:EXECUTE WEBPA COMMAND PARAMETER:Device.DHCPv4.Server.Pool.1.MaxAddress with Value retrieved");
			LOGGER.info("POST-CONDITION 2: EXPECTED:VALUES OF DHCP MAX ADDRESS SHOULD BE RESTORED SUCCESSFULLY");
			LOGGER.info("##########################################################################");
			status = false;
			try {
				if (CommonMethods.isNotNull(dhcpMaxAddr)) {
					status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_TO_RETRIEVE_DHCP_ENDING_IP_ADDRESS,
							BroadBandTestConstants.CONSTANT_0, dhcpMaxAddr,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					LOGGER.info("Waiting for two minutes");
					tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				}
				LOGGER.info("POST CONDITION 2:ACTUAL:DHCP MAX ADDRESS RESTORATION STATUS: " + status);
			} catch (Exception e) {
				LOGGER.error("EXCEPTION OCCURED WHILE CONFIGURING POST CONDITION 2:\n" + e.getMessage());
			}

			LOGGER.info("##########################################################################");
			LOGGER.info("POST-CONDITION 3: DESCRIPTION:VERIFY THAT ETHERNET CONNECTED CLIENT IS IN DHCP RANGE");
			LOGGER.info("POST CONDITION 3: ACTION:EXECUTE ipconfig/ifconfig AND VERIFY IP ADDRESS");
			LOGGER.info("POST-CONDITION 3: EXPECTED:ETHERNET CLIENT IP ADDRESS SHOULD BE IN DHCP RANGE ");
			LOGGER.info("##########################################################################");
			status = false;
			try {
				if (ethernetClient != null) {
					BroadBandResultObject broadBandResultObject = BroadBandConnectedClientUtils
							.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device, tapEnv, ethernetClient);
					LOGGER.info("POST CONDITION 3:ACTUAL:ETHERNET CLIENT IP ADDRESS DHCP RANGE STATUS: "
							+ broadBandResultObject.isStatus());
				}
			} catch (Exception e) {
				LOGGER.error("EXCEPTION OCCURED WHILE CONFIGURING POST CONDITION 3:\n" + e.getMessage());
			}
			LOGGER.info("##########################################################################");
			LOGGER.info("POST-CONDITION 4: DESCRIPTION:VERIFY THAT WIFI CONNECTED CLIENT 1 IS IN DHCP RANGE");
			LOGGER.info("POST CONDITION 4: ACTION:EXECUTE ipconfig/ifconfig AND VERIFY IP ADDRESS");
			LOGGER.info("POST-CONDITION 4: EXPECTED:CLIENT IP ADDRESS SHOULD BE IN DHCP RANGE");
			LOGGER.info("##########################################################################");
			status = false;
			try {
				if (wifi24GhzClient != null) {
					BroadBandResultObject broadBandResultObject = BroadBandConnectedClientUtils
							.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device, tapEnv, wifi24GhzClient);
					LOGGER.info("POST CONDITION 4:ACTUAL:WIFI CONNECTED CLIENT 1 IP ADDRESS DHCP RANGE : STATUS: "
							+ broadBandResultObject.isStatus());
				}
			} catch (Exception e) {
				LOGGER.error("EXCEPTION OCCURED WHILE CONFIGURING POST CONDITION 4:\n" + e.getMessage());
			}
			LOGGER.info("##########################################################################");
			LOGGER.info("POST-CONDITION 5: DESCRIPTION:VERIFY THAT WIFI CONNECTED CLIENT 2 IS IN DHCP RANGE");
			LOGGER.info("POST CONDITION 5: ACTION:EXECUTE ipconfig/ifconfig AND VERIFY IP ADDRESS");
			LOGGER.info("POST-CONDITION 5: EXPECTED:CLIENT IP ADDRESS SHOULD BE IN DHCP RANGE");
			LOGGER.info("##########################################################################");
			status = false;
			try {
				if (wifi5GhzClient != null) {
					BroadBandResultObject broadBandResultObject = BroadBandConnectedClientUtils
							.verifyConnectedClientIpv4AddressInDhcpAfterRenew(device, tapEnv, wifi5GhzClient);

					LOGGER.info("POST CONDITION 5:ACTUAL:WIFI CONNECTED CLIENT 2 IP ADDRESS DHCP RANGE : STATUS: "
							+ broadBandResultObject.isStatus());
				}
			} catch (Exception e) {
				LOGGER.error("EXCEPTION OCCURED WHILE CONFIGURING POST CONDITION 5:\n" + e.getMessage());
			}
			LOGGER.info("##########################################################################");
			LOGGER.info("POST-CONDITION 6: DESCRIPTION:RESTORE VALUES OF DHCP  LEASE TIME");
			LOGGER.info(
					"POST CONDITION 6: ACTION:EXECUTE WEBPA COMMAND PARAMETER:Device.DHCPv4.Server.Pool.1.LeaseTime with Value retrieved");
			LOGGER.info("POST-CONDITION 6: EXPECTED:VALUES OF LEASE SHOULD BE RESTORED SUCCESSFULLY");
			LOGGER.info("##########################################################################");
			status = false;
			try {
				if (CommonMethods.isNotNull(leaseTime)) {
					status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_DHCP_LEASETIME,
							BroadBandTestConstants.CONSTANT_1, leaseTime, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				}
				LOGGER.info("POST CONDITION 6:ACTUAL:DHCP LEASE TIME RESTORATION STATUS: " + status);
			} catch (Exception e) {
				LOGGER.error("EXCEPTION OCCURED WHILE CONFIGURING POST CONDITION 6:\n" + e.getMessage());
			}
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 7 : DESCRIPTION : Disconnect Wifi Radio 2.4Ghz SSID from the device");
			LOGGER.info("POST-CONDITION 7 : ACTION :Disconnect wifi radio 2.4Ghz SSID ");
			LOGGER.info("POST-CONDITION 7 : EXPECTED : Wifi radio 2.4Ghz SSID should be disconnected successfully");
			LOGGER.info("#######################################################################################");
			try {
				String ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				LOGGER.info("SSIDNAME:" + ssidName);
				boolean connectionStatus = ConnectedNattedClientsUtils.disconnectSSID(wifi24GhzClient, tapEnv,
						ssidName);
				LOGGER.info("POST CONDITION 7:ACTUAL: WIFI SSID 2.4GHZ Disconnect status:" + connectionStatus);
			} catch (Exception exception2) {
				LOGGER.error("EXCEPTION OCCURRED WHILE PERFORMING POST CONFIGURATION 7" + exception2.getMessage());
			}
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 8 : DESCRIPTION : Disconnect Wifi Radio 5Ghz SSID from the device");
			LOGGER.info("POST-CONDITION 8 : ACTION :Disconnect wifi radio 5Ghz SSID ");
			LOGGER.info("POST-CONDITION 8 : EXPECTED : Wifi radio 5Ghz SSID should be disconnected successfully");
			LOGGER.info("#######################################################################################");
			try {
				String ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				LOGGER.info("SSIDNAME:" + ssidName);
				boolean connectionStatus = ConnectedNattedClientsUtils.disconnectSSID(wifi5GhzClient, tapEnv, ssidName);
				LOGGER.info("POST CONDITION 8:ACTUAL: WIFI SSID 5GHZ Disconnect status:" + connectionStatus);
			} catch (Exception exception2) {
				LOGGER.error("EXCEPTION OCCURRED WHILE PERFORMING POST CONFIGURATION 8" + exception2.getMessage());
			}
			LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");

		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-DHCP-CLIENTS-1001");
	}

	/**
	 * Helper method to post telemetry profile to xconf
	 * 
	 * @param device {@link Dut}
	 * @author ArunKumar Jayachandran
	 * @refactor Athira
	 */
	public static final void postTelemetryData(Dut device) {
		String errorMessage = null;
		LOGGER.info("STEP 2: Upload telemetry profile to Xconf");
		LOGGER.info("PRE-CONDITION : DESCRIPTION : Upload telemetry profile to Xconf ");
		LOGGER.info("PRE-CONDITION : EXPECTED : Post request to Xconf should respond with HTTP 200 code ");
		if (BroadBandTelemetryUtils.postDataToProxyDcmServer(device, tapEnv, false,
				true) != BroadBandTestConstants.CONSTANT_200) {
			errorMessage = "Unable to Post telemetry data to XConf";
			LOGGER.error(errorMessage);
			throw new TestException(
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.PRE_CONDITION_ERROR,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER, errorMessage));
		}
		LOGGER.info("PRE-CONDITION : ACTUAL: Successfully Uploaded telemetry profile to Xconf");
		LOGGER.info("PRE-CONFIGURATIONS : FINAL STATUS -  " + true);
	}

	/**
	 * Helper method to configure telemetry settings in device
	 * 
	 * @param device     {@link Dut}
	 * @param testCaseId test case id
	 * @author ArunKumar Jayachandran
	 * @refactor Athira
	 */
	public static void telemetryConfiguration(Dut device, String testCaseId) {
		boolean status = false;
		String errorMessage = null;
		String stepNumber = null;
		// Step 1 : Copy dcm.properties to /nvram folder and change the
		// DCM_LOG_SERVER_URL.
		stepNumber = "s1";
		status = false;
		LOGGER.info("*******************************************************************************");
		LOGGER.info("STEP 1: DESCRIPTION : Copy dcm.properties to /nvram folder and change the DCM_LOG_SERVER_URL");
		LOGGER.info("STEP 1: ACTION : Execute Command cp /etc/dcm.properties /nvram and update xconf url");
		LOGGER.info("STEP 1: EXPECTED : The file should be copied and updated");
		LOGGER.info("*******************************************************************************");
		int validationStatus = BroadBandTelemetryUtils.copyAndUpdateDcmProperties(device, tapEnv);
		// updating the test case based on the return value from the helper
		// method
		if (BroadBandTestConstants.CONSTANT_0 == validationStatus) {
			errorMessage = "dcm.properties is not present, missing, in /etc/ folder";
		} else if (BroadBandTestConstants.CONSTANT_2 == validationStatus) {
			errorMessage = "Successfuly copied /etc/dcm.properties to /nvram folder, But Failed to update Xconf url!!!!";
		} else {
			status = true;
		}
		LOGGER.info("STEP 1: " + (status
				? " ACTUAL: Successfully copied dcm.properties file to /nvram and verified the DCM_LOG_SERVER_URL"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

		// Step 2 : Reboot the device and wait for IP acquisition.
		stepNumber = "s2";
		status = false;
		LOGGER.info("*******************************************************************************");
		LOGGER.info("STEP 2: DESCRIPTION : Reboot the device and wait for IP acquisition");
		LOGGER.info("STEP 2: ACTION : Execute Command /sbin/reboot");
		LOGGER.info("STEP 2: EXPECTED : Device should be rebooted");
		LOGGER.info("*******************************************************************************");
		status = BroadBandTelemetryUtils.rebootAndWaitForDeviceAccessible(tapEnv, device,
				BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
		LOGGER.info("STEP 2: Device reboot status is - " + status);
		errorMessage = "Failed to reboot the device. Checked for 10 min to access the device after reboot.";
		LOGGER.info("STEP 2: "
				+ (status ? "ACTUAL: Successfully reboot the STB and able to access the device " : errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		// Step 3 : Validate modified url in dcmscript.log file
		stepNumber = "s3";
		status = false;
		LOGGER.info("*******************************************************************************");
		LOGGER.info("STEP 3: DESCRIPTION : Validate modified url in dcmscript.log file");
		LOGGER.info("STEP 3: ACTION : Verify Xconf url present in dcmscript.log file");
		LOGGER.info("STEP 3: EXPECTED : File should have modified xconf url");
		LOGGER.info("*******************************************************************************");

		try {
			BroadBandTelemetryUtils.verifyXconfDcmConfigurationUrlAndDownloadStatusFromDcmScriptLogs(device, tapEnv,
					BroadBandTestConstants.PROP_KEY_PROXY_XCONF_URL);
			status = true;
		} catch (TestException exception) {
			errorMessage = exception.getMessage();
		}

		LOGGER.info("STEP 3: " + (status ? "ACTUAL:  Modified URL is present in dcmscript.log file "
				: " Modified URL is not present in dcmscript.log. Error Message - " + errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	}

	/**
	 * Validate telemetry markers for Maximum Tx&Rx rate for each wifi clients
	 * 
	 * <li>1. Update the telemetry log interval using webpa</li>
	 * <li>2. Verify vAP enable status for telemetry logging</li>
	 * <li>3. Update the telemetry txrxRatelist using webpa</li>
	 * <li>4. Verify 5GHz client is active using ping</li>
	 * <li>5. Verify maximum tx & rx rate for 5GHz using wifi_api command</li>
	 * <li>6. Verify max tx & rate for 5GHz using wifi_api command and compare with
	 * diagnostic result</li>
	 * <li>7. Verify maximum tx & rx rate for 5GHz in wifihealth.txt with wifi_api
	 * command response</li>
	 * <li>8. Connect and verify 2.4GHz client is active using ping</li>
	 * <li>9. Verify maximum tx & rx rate for 2.4GHz using wifi_api command</li>
	 * <li>10. Verify max tx & rate for 2.4GHz using wifi_api command and compare
	 * with diagnostic result</li>
	 * <li>11. Get maximum tx & rx rate for 2.4GHz in wifihealth.txt</li>
	 * <li>12. update the TxRxRateList as empty for AtomSyncDevice device 1,2 for
	 * other device model</li>
	 * 
	 * @author ArunKumar Jayachandran
	 * @refactor Govardhan
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			TestGroup.NEW_FEATURE, TestGroup.TELEMETRY })
	@TestDetails(testUID = "TC-RDKB-WIFI-TELEMETRY-1001")
	public void testToVerifyMaxTxRxRatePerClient(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1001 ");
		LOGGER.info("TEST DESCRIPTION: Test to verify telemetry markers for Maximum TX_RATE, RX_RATE per client ");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update the telemetry log interval using webpa");
		LOGGER.info("2. Verify vAP enable status for telemetry logging");
		LOGGER.info("3. Update the telemetry txrxRatelist using webpa");
		LOGGER.info("4. Verify 5GHz client is active using ping");
		LOGGER.info("5. Verify maximum tx & rx rate for 5GHz using wifi_api command");
		LOGGER.info("6. Verify max tx & rate for 5GHz using wifi_api command and compare with diagnostic result");
		LOGGER.info("7. Verify maximum tx & rx rate for 5GHz in wifihealth.txt with wifi_api command response");
		LOGGER.info("8. Connect and verify 2.4GHz client is active using ping");
		LOGGER.info("9. Verify maximum tx & rx rate for 2.4GHz using wifi_api command");
		LOGGER.info("10. Verify max tx & rate for 2.4GHz using wifi_api command and compare with diagnostic result");
		LOGGER.info("11. Get maximum tx & rx rate for 2.4GHz in wifihealth.txt");
		LOGGER.info("12. update the TxRxRateList as empty for AtomSyncDevice device 1,2 for other device model");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-WIFI-TELEMETRY-001";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store response
		String response = null;
		// Dut object to store the 2.4GHz client device
		Dut clientSettop2Ghz = null;
		// Dut object to store the 5GHz client device
		Dut clientSettop5Ghz = null;
		// String to store the TxRate response from log file
		String txRateLog = null;
		// String to store the RxRate response from log file
		String rxRateLog = null;
		// Array list of string to store all clients mac address
		ArrayList<String> clientMacList = null;
		// Array list of string to store all clients rxRate
		ArrayList<String> rxRateList = null;
		// Array list of string to store all clients rxRate
		ArrayList<String> txRateList = null;
		// String to store client mac address
		String clientMac = null;
		// integer to store the index
		int index = BroadBandTestConstants.CONSTANT_0;
		// variable declaration ends

		boolean isTxRxDevice = BroadbandPropertyFileHandler.isTxRxRateListDevices(device);

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("PRE-CONDITION : DESCRIPTION : Verify and connect 5GHz support connected client device");
			LOGGER.info("PRE-CONDITION : ACTION : connect the clients with 5GHz ssid");
			LOGGER.info("PRE-CONDITION : EXPECTED : Client device should connect with 5GHz ssid");
			BroadBandCommonUtils.deleteFileAndVerifyIfAtomPresentElseArm(tapEnv, device,
					BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG,
					(CommonMethods.isAtomSyncAvailable(device, tapEnv)));
			clientSettop5Ghz = BroadBandConnectedClientUtils
					.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			status = (clientSettop5Ghz != null);
			if (!status) {
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
						+ "Failed to get the client device connected with 5GHz private ssid");
			}
			LOGGER.info("PRE-CONDITION : ACTUAL: Successfully connected the 5GHz client device");
			LOGGER.info("PRE-CONFIGURATIONS : FINAL STATUS -  " + status);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: Update the telemetry log interval using webpa");
			LOGGER.info(
					"STEP 1: ACTION: Execute webpa set command: Parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval as 300");
			LOGGER.info("STEP 1: EXPECTED: Webpa set operation should success and value should be 300");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the telemetry log interval for webpa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.LogInterval";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELEMETRY_LOG_INTERVAL, BroadBandTestConstants.CONSTANT_1,
					BroadBandTestConstants.STRING_300, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL: Successfully updated telemetry log interval using webpa set operation");
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s2";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION: Verify vAP enable status for telemetry logging");
			LOGGER.info(
					"STEP 2: ACTION: Execute webpa set command: Parameter: Device.WiFi.X_RDKCENTRAL-COM_vAPStatsEnable as true");
			LOGGER.info("STEP 2: EXPECTED: webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for webpa parameter Device.WiFi.X_RDKCENTRAL-COM_vAPStatsEnable";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_VAP_STATS_ENABLE, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL: Successfully verified vAP stats enable as true using webpa");
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s3";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION: Update the telemetry txrxRatelist using webpa");
			LOGGER.info(
					"STEP 3: ACTION: Execute webpa set command: Parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.TxRxRateList as 1,2");
			LOGGER.info("STEP 3: EXPECTED: Webpa set operation should success and value should be 1,2");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to set webpa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.TxRxRateList";
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELE_TXRX_RATE_LIST);
			if (isTxRxDevice) {
				errorMessage = "Telemetry logging is not enabled by default for private vAP's";
				status = CommonMethods.isNotNull(response)
						&& response.equalsIgnoreCase(BroadBandTestConstants.PRIVATE_XH_AP_LIST);
			} else if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELE_TXRX_RATE_LIST, WebPaDataTypes.STRING.getValue(),
						BroadBandTestConstants.PRIVATE_XH_AP_LIST, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL: Successfully verified TxRxRateList using webpa operation");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s4";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION: Verify 5GHz client is active using ping");
			LOGGER.info("STEP 4: ACTION: Execute command in client: ping -c 30 www.google.com");
			LOGGER.info("STEP 4: EXPECTED: ping operation should be successful");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for ping request in 5GHz client device";
			if (!ConnectedNattedClientsUtils.verifyConnectToSSID(clientSettop5Ghz, tapEnv, tapEnv.executeWebPaCommand(
					device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID), true)) {
				clientSettop5Ghz = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			}
			status = ConnectedNattedClientsUtils.verifyPingConnection(clientSettop5Ghz, tapEnv,
					BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS, BroadBandTestConstants.STRING_30);
			if (status) {
				LOGGER.info("STEP 4: ACTUAL: Successfully verified ping response for 5GHz client");
			} else {
				LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s5";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION: Verify maximum tx & rx rate for 5GHz using wifi_api command");
			LOGGER.info("STEP 5: ACTION: Execute command: wifi_api wifi_getApAssociatedDeviceDiagnosticResult3 1");
			LOGGER.info("STEP 5: EXPECTED: Response should contain the maximum uplink & downlink value");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response from wifi_api for max tx rate";
			response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
					BroadBandCommandConstants.CMD_WIFI_API_5G_STATS);
			if (CommonMethods.isNotNull(response)) {
				clientMacList = CommonMethods.patternFinderToReturnAllMatchedString(response,
						(BroadBandTestConstants.PATTERN_CLI_MAC_ADDRSS
								+ BroadBandTestConstants.REG_EXPRESSION_TO_GET_MAC_ADDRESS_SEMICOLON));
				LOGGER.info("clientMacList" + clientMacList);
				rxRateList = CommonMethods.patternFinderToReturnAllMatchedString(response,
						BroadBandTestConstants.PATTERN_MAX_DOWNLINK_RATE);
				LOGGER.info("rxRateList" + rxRateList);
				txRateList = CommonMethods.patternFinderToReturnAllMatchedString(response,
						BroadBandTestConstants.PATTERN_MAX_UPLINK_RATE);
				LOGGER.info("txRateList" + txRateList);
				if (clientMacList != null && !clientMacList.isEmpty()) {
					for (int counter = BroadBandTestConstants.CONSTANT_0; counter < clientMacList.size(); counter++) {
						clientMac = BroadBandCommonUtils.wifiApiMacAddressFormat(clientMacList.get(counter));
						clientMacList.set(counter, clientMac);
					}
				}
				status = clientMacList != null && rxRateList != null && txRateList != null && !clientMacList.isEmpty()
						&& !rxRateList.isEmpty() && !txRateList.isEmpty();
			}
			if (status) {
				LOGGER.info("STEP 5: Client mac list: " + clientMacList + "\n Tx rate list: " + txRateList
						+ "\n Rx rate list: " + rxRateList);
				LOGGER.info("STEP 5: ACTUAL: Successfully verified the Max TX rate & RX rate for each client");
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s6";
			status = false;
			if (isTxRxDevice) {
				LOGGER.info("******************************************************************************");
				LOGGER.info(
						"STEP 6: DESCRIPTION: Verify max tx & rate for 5GHz using wifi_api command and compare with diagnostic result");
				LOGGER.info("STEP 6: ACTION: Execute command: wifi_api getAssociatedDeviceMaxTxRxRate 1 <client mac>");
				LOGGER.info("STEP 6: EXPECTED: Response should contain the max tx & rx rate value & should be same ");
				LOGGER.info("******************************************************************************");
				errorMessage = "Failed to get the response for wifi_api getAssociatedDeviceMaxTxRxRate";
				response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
						BroadBandCommonUtils.concatStringUsingStringBuffer(
								BroadBandCommandConstants.CMD_WIFI_MAX_TX_RX_RATE_1,
								clientMacList.get(BroadBandTestConstants.CONSTANT_0)));
				if (CommonMethods.isNotNull(response)) {
					txRateLog = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_MAX_TX_RATE);
					rxRateLog = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_MAX_RX_RATE);
					LOGGER.info("TxRateLog is : " + txRateLog + " TxRateList is : "
							+ txRateList.get(BroadBandTestConstants.CONSTANT_0));
					LOGGER.info("RxRateLog is : " + rxRateLog + " RxRateList is : "
							+ rxRateList.get(BroadBandTestConstants.CONSTANT_0));
					status = CommonMethods.isNotNull(txRateLog) && CommonMethods.isNotNull(rxRateLog)
							&& txRateLog.equalsIgnoreCase(txRateList.get(BroadBandTestConstants.CONSTANT_0).trim())
							&& rxRateLog.equalsIgnoreCase(rxRateList.get(BroadBandTestConstants.CONSTANT_0).trim());
				}
				if (status) {
					LOGGER.info("STEP 6: ACTUAL: Successfully verified Tx & Rx rate for 5GHz client");
				} else {
					LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
				}
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("Not Applicable for " + device.getManufacturer() + "_" + device.getModel());
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			// ##################################################################################################//

			stepNumber = "s7";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP 7: DESCRIPTION: Verify maximum tx & rx rate for 5GHz in wifihealth.txt with wifi_api command response");
			LOGGER.info(
					"STEP 7: ACTION: Execute command:\n1. cat /rdklogs/logs/wifihealth.txt | grep -i mac_2: | tail -1\n2. cat /rdklogs/logs/wifihealth.txt | grep -i max | grep -i clients_2 | tail -2");
			LOGGER.info("STEP 7: EXPECTED: Response should contain the log message for 5GHz client");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the log message MAX_TXCLIENTS_2, MAX_RXCLIENTS_2 in wifihealth.txt file";
			response = BroadBandCommonUtils.executeCommandInAtomConsoleByPolling(tapEnv, device,
					BroadBandCommandConstants.CMD_5GHZ_MAC_LIST, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			String clientMacList1 = CommonMethods.patternFinder(response, (BroadBandTestConstants.PATTERN_WIFI_MAC_2
					+ BroadBandTestConstants.REG_EXPRESSION_TO_GET_MAC_ADDRESS_SEMICOLON));

			LOGGER.info("clientMacList1 is : " + clientMacList1);
			if (CommonMethods.isNotNull(clientMacList1)) {
				response = BroadBandCommonUtils.executeCommandInAtomConsoleByPolling(tapEnv, device,
						BroadBandCommandConstants.CMD_MAX_TX_RX_CLIENT_2, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				if (CommonMethods.isNotNull(response)) {
					errorMessage = "Failed to get the max tx & rx rate values for 5GHz client in wifihealth.txt file";
					txRateLog = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_MAX_TXCLIENTS_2);
					rxRateLog = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_MAX_RXCLIENTS_2);
					LOGGER.info("TxRateLog is : " + txRateLog);
					LOGGER.info("RxRateLog is : " + rxRateLog);
					if (CommonMethods.isNotNull(txRateLog) && CommonMethods.isNotNull(rxRateLog)) {
						LOGGER.info("TxRateList is : " + txRateList.get(index));
						LOGGER.info("RxRateList is : " + rxRateList.get(index));
						index = clientMacList.indexOf(clientMacList1.trim());
						status = txRateLog.equalsIgnoreCase(txRateList.get(index).trim())
								&& rxRateLog.equalsIgnoreCase(rxRateList.get(index).trim());

						if (!status) {
							index = clientMacList.indexOf(clientMacList1.toUpperCase());
							status = txRateLog.equalsIgnoreCase(txRateList.get(index).trim())
									&& rxRateLog.equalsIgnoreCase(rxRateList.get(index).trim());
						}
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 7: ACTUAL: Successfully verified telemetry log message for 5G max tx & rx rate as"
						+ " txRate = " + txRateLog + ", rxRate = " + rxRateLog);
			} else {
				LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s8";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION: Connect and verify 2.4GHz client is active using ping");
			LOGGER.info("STEP 8: ACTION: Execute command in client: ping -c 30 www.google.com");
			LOGGER.info("STEP 8: EXPECTED: ping operation should be successful");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for ping request in 2.4GHz client device";
			BroadBandCommonUtils.deleteFileAndVerifyIfAtomPresentElseArm(tapEnv, device,
					BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG,
					(CommonMethods.isAtomSyncAvailable(device, tapEnv)
							|| DeviceModeHandler.isBusinessClassDevice(device)));
			clientSettop2Ghz = BroadBandConnectedClientUtils
					.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			if ((clientSettop2Ghz == null)
					|| !ConnectedNattedClientsUtils
							.verifyConnectToSSID(clientSettop2Ghz, tapEnv,
									tapEnv.executeWebPaCommand(device,
											BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID),
									true)) {
				clientSettop2Ghz = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			}
			status = ConnectedNattedClientsUtils.verifyPingConnection(clientSettop2Ghz, tapEnv,
					BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS, BroadBandTestConstants.STRING_30);
			if (status) {
				LOGGER.info("STEP 8: ACTUAL: Successfully verified ping response for 2.4GHz client");
			} else {
				LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s9";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION: Verify maximum tx & rx rate for 2.4GHz using wifi_api command");
			LOGGER.info("STEP 9: ACTION: Execute command: wifi_api wifi_getApAssociatedDeviceDiagnosticResult3 0");
			LOGGER.info("STEP 9: EXPECTED: Response should contain the maximum up and downlink value");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response from wifi_api for max tx & rx rate";
			response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
					BroadBandCommandConstants.CMD_WIFI_API_24G_STATS);
			if (CommonMethods.isNotNull(response)) {
				clientMacList = CommonMethods.patternFinderToReturnAllMatchedString(response,
						(BroadBandTestConstants.PATTERN_CLI_MAC_ADDRSS
								+ BroadBandTestConstants.REG_EXPRESSION_TO_GET_MAC_ADDRESS_SEMICOLON));
				rxRateList = CommonMethods.patternFinderToReturnAllMatchedString(response,
						BroadBandTestConstants.PATTERN_MAX_DOWNLINK_RATE);
				txRateList = CommonMethods.patternFinderToReturnAllMatchedString(response,
						BroadBandTestConstants.PATTERN_MAX_UPLINK_RATE);
				if (clientMacList != null && !clientMacList.isEmpty()) {
					for (int counter = BroadBandTestConstants.CONSTANT_0; counter < clientMacList.size(); counter++) {
						clientMac = BroadBandCommonUtils.wifiApiMacAddressFormat(clientMacList.get(counter));
						clientMacList.set(counter, clientMac);
					}
				}
				status = clientMacList != null && rxRateList != null && txRateList != null && !clientMacList.isEmpty()
						&& !rxRateList.isEmpty() && !txRateList.isEmpty();
			}
			if (status) {
				LOGGER.info("STEP 9: Client mac list: " + clientMacList + "\n Tx rate list: " + txRateList
						+ "\n Rx rate list: " + rxRateList);
				LOGGER.info("STEP 9: ACTUAL: Successfully verified the Max TX rate & RX rate for each client");
			} else {
				LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s10";
			status = false;
			if (isTxRxDevice) {
				LOGGER.info("******************************************************************************");
				LOGGER.info(
						"STEP 10: DESCRIPTION: Verify max tx & rate for 2.4GHz using wifi_api command and compare with diagnostic result");
				LOGGER.info("STEP 10: ACTION: Execute command: wifi_api getAssociatedDeviceMaxTxRxRate 0 <client mac>");
				LOGGER.info("STEP 10: EXPECTED: Response should contain the max tx & rx rate value & should be same ");
				LOGGER.info("******************************************************************************");
				errorMessage = "Failed to get the response for wifi_api getAssociatedDeviceMaxTxRxRate";
				response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
						BroadBandCommonUtils.concatStringUsingStringBuffer(
								BroadBandCommandConstants.CMD_WIFI_MAX_TX_RX_RATE_0,
								clientMacList.get(BroadBandTestConstants.CONSTANT_0)));
				if (CommonMethods.isNotNull(response)) {
					txRateLog = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_MAX_TX_RATE);
					rxRateLog = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_MAX_RX_RATE);
					status = CommonMethods.isNotNull(txRateLog) && CommonMethods.isNotNull(rxRateLog)
							&& txRateLog.equalsIgnoreCase(txRateList.get(BroadBandTestConstants.CONSTANT_0).trim())
							&& rxRateLog.equalsIgnoreCase(rxRateList.get(BroadBandTestConstants.CONSTANT_0).trim());
				}
				if (status) {
					LOGGER.info("STEP 10: ACTUAL: Successfully verified Max Tx & Rx rate for 2.4GHz client");
				} else {
					LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
				}
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("Not Applicable for " + device.getManufacturer() + "_" + device.getModel());
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			// ##################################################################################################//

			stepNumber = "s11";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION: Get maximum tx & rx rate for 2.4GHz in wifihealth.txt");
			LOGGER.info(
					"STEP 11: ACTION: Execute command:\n1. cat /rdklogs/logs/wifihealth.txt | grep -i mac_1: | tail -1\n2. cat /rdklogs/logs/wifihealth.txt | grep -i max | grep -i clients_1 | tail -2");
			LOGGER.info("STEP 11: EXPECTED: Response should contain the log message");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the log message MAX_TXCLIENTS_1, MAX_RXCLIENTS_1 in wifihealth.txt file";
			response = BroadBandCommonUtils.executeCommandInAtomConsoleByPolling(tapEnv, device,
					BroadBandCommandConstants.CMD_2_4GHZ_MAC_LIST, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			clientMacList1 = CommonMethods.patternFinder(response, (BroadBandTestConstants.PATTERN_WIFI_MAC_1
					+ BroadBandTestConstants.REG_EXPRESSION_TO_GET_MAC_ADDRESS_SEMICOLON));
			if (CommonMethods.isNotNull(clientMacList1)) {
				response = BroadBandCommonUtils.executeCommandInAtomConsoleByPolling(tapEnv, device,
						BroadBandCommandConstants.CMD_MAX_TX_RX_CLIENT_1, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				if (CommonMethods.isNotNull(response)) {
					LOGGER.info("Client mac list " + clientMacList1);
					errorMessage = "Failed to get the max tx & rx rate values for 2.4GHz client in wifihealth.txt file";
					txRateLog = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_MAX_TXCLIENTS_1);
					rxRateLog = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_MAX_RXCLIENTS_1);

					LOGGER.info("TxRateLog is : " + txRateLog);
					LOGGER.info("RxRateLog is : " + rxRateLog);

					if (CommonMethods.isNotNull(txRateLog) && CommonMethods.isNotNull(rxRateLog)) {
						LOGGER.info("TxRateList is : " + txRateList.get(index));
						LOGGER.info("RxRateList is : " + rxRateList.get(index));
						index = clientMacList.indexOf(clientMacList1);
						status = txRateLog.equalsIgnoreCase(txRateList.get(index).trim())
								&& rxRateLog.equalsIgnoreCase(rxRateList.get(index).trim());

						if (!status) {
							index = clientMacList.indexOf(clientMacList1.toUpperCase());
							status = txRateLog.equalsIgnoreCase(txRateList.get(index).trim())
									&& rxRateLog.equalsIgnoreCase(rxRateList.get(index).trim());
						}
					}
				}
			}

			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL: Successfully verified telemetry log message for 2.4GHz max tx & rx rate as"
								+ " txRate = " + txRateLog + ", rxRate = " + rxRateLog);
			} else {
				LOGGER.error("STEP 11: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s12";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION: update the TxRxRateList empty for Atom Sync Devices and 1,2 for BWG's");
			LOGGER.info(
					"STEP 12: ACTION: Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.TxRxRateList as empty or 1,2");
			LOGGER.info(
					"STEP 12: EXPECTED: Should be reverted back to empty/1,2 for TxRxRateList parameter for Atom Based / Bussiness Class device");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the parameter Device.DeviceInfo.X_RDKCENTRAL-COM_WIFI_TELEMETRY.TxRxRateList";
			if (isTxRxDevice) {
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELE_TXRX_RATE_LIST, BroadBandTestConstants.CONSTANT_0,
						BroadBandTestConstants.PRIVATE_XH_AP_LIST, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			} else {
				status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELE_TXRX_RATE_LIST, BroadBandTestConstants.CONSTANT_0,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
				if (status) {
					status = false;
					response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_WIFI_TELE_TXRX_RATE_LIST);
					status = CommonMethods.isNull(response);
				}
			}
			if (status) {
				LOGGER.info("STEP 12: ACTUAL: Successfully updated the TxRxRateList using webpa");
			} else {
				LOGGER.error("STEP 12: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying telemetry logging for max tx & rx rate" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1001");
		// ###############################################################//
	}

	/**
	 * This test case is to Verify the controlled user access (Deny) for 2.4GHZ
	 * against different MAC addresses by way of MAC ACLs
	 * 
	 * <ol>
	 * <li>STEP 1: Getting the Wifi Mac address of Connected client having 2.4GHZ
	 * wifi Capability.</li>
	 * <li>STEP 2: Verify and add the device wifi Mac address in the MAC Filter list
	 * by using WEBPA command.</li>
	 * <li>STEP 3: Verify the TR-181 parameter
	 * 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as true using
	 * Webpa.</li>
	 * <li>STEP 4: Verify whether the MAC Filtering mode is configured as 'Deny' by
	 * setting TR-181 parameter
	 * 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList' as
	 * 'true' using WebPA for 2.4GHZ.</li>
	 * <li>STEP 5:verify getting the Configured MAC Filter Mode by using this
	 * TR-181parameter
	 * 'Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode'</li>
	 * <li>STEP 6: Connect the connected client device whose wifi Mac address is
	 * added in the MAC Filter to 2.4 GHz SSID and verify connection status</li>
	 * <li>STEP 7: Verify whether interface did'nt get the correct IPv4
	 * address.</li>
	 * <li>STEP 8 : Verify whether interface did'nt get the correct IPv6
	 * address.</li>
	 * <li>STEP 9: Verify whether there is no connectivity using that particular
	 * interface using IPV4</li>
	 * <li>STEP 10: Verify whether there is no connectivity using that particular
	 * interface using IPV6</li>
	 * <li>STEP 11: Connect the connected client device whose wifi Mac address is
	 * not added in the MAC Filter to 2.4 GHz SSID and verify connection status</li>
	 * <li>STEP 12 :Verify whether interface got the correct IPv4 address</li>
	 * <li>STEP 13 :Verify whether interface got the correct IPv6 address</li>
	 * <li>STEP 14 :Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * <li>STEP 15 :Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * </ol>
	 * 
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor yamini.s
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			TestGroup.WEBPA, TestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-5013")

	public void testVerifyMacFilterAsDenyFor2_4Ghz(Dut device) {

		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-513";
		// Test step number
		int stepNumber = 1;
		String stepNum = "S" + stepNumber;
		// String to store the error message
		String errorMessage = null;
		// server response
		String response = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;
		// String to store the Wifi Macaddress for 2GHZ connected client
		String wifiMacAddress = null;
		// String to store the added table row number
		String tableRowNumber = null;
		// string to store the webpaserver response
		WebPaServerResponse webPaServerResponse = null;
		// string to store the ssid name
		String ssidName = null;
		// string to store the password
		String passPhraseName = null;
		// string to store the command
		String command = null;
		// String to store the device name
		String getDeviceName = null;

		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TESTCASE: TC-RDKB-WIFI-5013");
			LOGGER.info(
					"TEST DESCRIPTION: Verify controlled user access (Deny) for 2.4 GHZ against different MAC addresses by way of MAC ACLs");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"Pre condition 1: Verify whether Private 2.4 GHz SSID 'Device.WiFi.SSID.10001.Enable' is enabled using WebPA.");
			LOGGER.info("Step 1 : Getting the Wifi Mac address of Connected client having 2.4GHZ wifi Capability.");
			LOGGER.info(
					"Step 2 : Verify and add the device wifi Mac address in the MAC Filter list by using WEBPA command.");
			LOGGER.info(
					"Step 3 : Verify the TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as true using Webpa.");
			LOGGER.info(
					"Step 4 : Verify whether the MAC Filtering mode is configured as 'Deny' by setting TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList' as 'true' using WebPA for 2.4GHZ.");
			LOGGER.info(
					"Step 5 : Verify getting the Configured MAC Filter Mode by using  this TR-181parameter 'Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode'");
			LOGGER.info(
					"Step 6 : Connect the connected client device whose wifi Mac address is  added in the MAC Filter to 2.4 GHz SSID and verify connection status");
			LOGGER.info("Step 7 : Verify whether the interface did'nt get the correct IPv4  address.");
			LOGGER.info("Step 8 : Verify whether interface did'nt get the correct IPv6  address.");
			LOGGER.info("Step 9 : Verify whether there is no connectivity using that particular interface using IPV4");
			LOGGER.info(
					"Step 10 : Verify whether there is no  connectivity using that particular interface using IPV6");
			LOGGER.info(
					"Step 11 : Connect the connected client device whose wifi Mac address is not added in the MAC Filter to 2.4 GHz SSID and verify connection status");
			LOGGER.info("Step 12 : Verify whether interface got the correct IPv4  address.");
			LOGGER.info("Step 13 : Verify whether interface got the correct  IPv6 address.");
			LOGGER.info("Step 14 : Verify whether you have connectivity using that particular interface using IPV4");
			LOGGER.info("Step 15 : Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info(
					"Post Condition 1 : Verify whether MAC Filtering mode is configured as 'Allow-All'(default Mode) by setting TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as 'false' using WebPA");
			LOGGER.info(
					"Post Condition 2 : Verify and  delete  the device wifi Mac address in the MAC Filter list by using WEBPA DELETE command.");
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			/**
			 * PRE-CONDITION :Enable Private 2.4 GHz SSID via WebPA
			 */
			errorMessage = null;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION 1 : DESCRIPTION : Verify whether private 2.4 GHz ssid 'Device.WiFi.SSID.10001.Enable' is enabled, if not enable the private 2.4 GHz ssid ");
			LOGGER.info(
					"PRE-CONDITION 1 : ACTION : Verify whether private 2.4 GHz ssid 'Device.WiFi.SSID.10001.Enable' is enabled,if not enable the private 2.4 GHz ssid using webpa ");
			LOGGER.info(
					"PRE-CONDITION 1 : EXPTECTED : Device should be enabled with private 2.4 GHz ssid and response should be true");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to enable the 2.4 GHz private ssid on this device";
			try {
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED,
						BroadBandTestConstants.TRUE);
			} catch (TestException exception) {
				LOGGER.error(errorMessage + " : " + exception.getMessage());
			}
			if (!status) {
				errorMessage = "Unable to set the private 2.4 GHz ssid status as 'true'.";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : Private 2.4 GHz ssid verified/enabled in gateway device.");
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1:Getting the Wifi Mac address of Connected client having 2.4GHZ wifi
			 * Capability.
			 */
			status = false;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Getting the Wifi Mac address of Connected client having 2.4GHZ wifi Capability.");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Execute webpa command: Device.WiFi.SSID.10001.Enable");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should be able to get the Wifi Mac address of the connected client having 2.4GHZ wifi Capability");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Unable to retrieve the Wifi Mac address of the connected client having 2.4GHZ wifi Capability";
			List<Dut> lockedDevices = ((Device) device).getConnectedDeviceList();
			try {
				connectedDeviceActivated = BroadBandConnectedClientUtils.getConnectedClientBasedOnTypeAndBand(device,
						tapEnv, lockedDevices, BroadBandTestConstants.WIFI, BroadBandTestConstants.BAND_2_4GHZ);
				wifiMacAddress = ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress();
				getDeviceName = ((Device) connectedDeviceActivated).getName();
				status = CommonMethods.isNotNull(wifiMacAddress);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Successfully retrieved the Wifi Mac address of the connected client having 2.4GHZ wifi Capability ");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 2:Verify and add the device wifi Mac address in the MAC Filter list by
			 * using WEBPA command.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify and add the device wifi Mac address in the MAC Filter list by using WEBPA command.");
			LOGGER.info("STEP : " + stepNumber
					+ " : ACTION : Execute wepa post command : Device.WiFi.AccessPoint.10001.X_CISCO_COM_MacFilterTable.");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should add the device wifi Mac address in the MAC Filter list");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Unable to add the wifi Mac Address to the  MAC Filter filter by Webpa POST command";
			Map<String, List<String>> macFilterTable = new HashMap<String, List<String>>();
			List<String> MacAddressList = new ArrayList<String>();
			MacAddressList.add(wifiMacAddress);
			LOGGER.info("Wifi Mac Address of the Connected client obtained is:-" + wifiMacAddress);
			List<String> DeviceList = new ArrayList<String>();
			DeviceList.add(getDeviceName);
			// adding to the Map.
			macFilterTable.put(BroadBandTestConstants.MAC_ADDRESS, MacAddressList);
			macFilterTable.put(BroadBandTestConstants.DEVICE_NAME, DeviceList);
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_TABLE,
					macFilterTable);
			if (null != webPaServerResponse) {
				tableRowNumber = webPaServerResponse.getRow();
				status = CommonMethods.isNotNull(webPaServerResponse.getMessage())
						&& webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Successfully added the wifi Mac Address to the  MAC Filter filter by Webpa POST command");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 3:Verify the TR-181 parameter
			 * "Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable" as true using
			 * Webpa.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify the TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as true using Webpa.");
			LOGGER.info("STEP : " + stepNumber
					+ " : ACTION : Execute webpa get command : Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should set the TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as true");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Unable to Set the the TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as true";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_ENABLE,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Successfully Set TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as true via webpa");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 4:Verify whether MAC Filtering mode is configured as "Deny" by setting
			 * TR-181 parameter
			 * "Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList" as
			 * "true" using WebPA for 2.4GHZ.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify whether the MAC Filtering mode is configured as 'Deny' by setting TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList' as 'true' using WebPA for 2.4GHZ.");
			LOGGER.info("STEP : " + stepNumber
					+ " : ACTION : Execute webpa set command : Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should have the MAC filtering mode as 'Deny' when we set 'true' to the TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList'");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Unable to Set the MAC Filter mode as Deny by setting true to the  TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList' by using Webpa commands.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_BLACK_LIST,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Successfully Set the MAC Filter mode as  Deny by setting the TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.FilterAsBlackList' as true via webpa");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 5:verify getting the Configured MAC Filter Mode by using this
			 * TR-181parameter
			 * "Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode" .
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : verify getting the Configured MAC Filter Mode by using  this TR-181parameter 'Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode'");
			LOGGER.info("STEP : " + stepNumber
					+ " : ACTION : Execute webpa get command: Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : The configured MAC Filter mode for the device should be as Deny");
			LOGGER.info("**********************************************************************************");
			errorMessage = " MAC Filter mode is not configured as Deny";
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_MODE);
			status = response.equalsIgnoreCase(BroadBandTestConstants.MAC_FILTER_DENY);
			if (status) {
				LOGGER.info("STEP : " + stepNumber + ": ACTUAL : MAC Filter mode for the device is configured as Deny");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 6:Connect the connected client device whose wifi Mac address is added in
			 * the MAC Filter to 2.4 GHz SSID and verify connection status
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Connect the connected client device whose wifi Mac address is  added in the MAC Filter to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Connect to 2.4 GHz wifi using below commands"
					+ "Linux :nmcli dev wifi connect <ssid> password <passwd>"
					+ "Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should not be connected with 2.4 GHz wifi network since the MAC Filter mode is configured as 'Deny'");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Connection to 2.4Ghz device is successful even though the MAC Filter mode is configured as 'Deny'";
			LOGGER.info("Going to wait for 1 minute before connecting the client to the wifi network");
			// wait for 1 min for before connecting to the ssid
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			LOGGER.info(
					"Wifi Mac Address of the Connected client whose wifi Mac address is  added in the MAC Filter is:-"
							+ ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress());
			try {
				ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
						tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);

				status = !ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
						passPhraseName);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Connected client device whose wifi Mac address is  added in the MAC Filter is not connected to 2.4GHZ wifi network since the MAC Filter mode is configured as 'Deny'");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, false);

			/**
			 * STEP 7:Verify whether the interface did'nt get the correct IPv4 address.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify whether the interface did'nt get the correct IPv4  address.");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Get the device IPv4 address using below command"
					+ "Linux : ifconfig wlan0 |grep -i " + "inet addr:" + "Windows: ipconfig |grep -A 10 "
					+ "Wireless LAN adapter Wi-Fi" + " |grep -i " + "Pv4 Address");
			LOGGER.info("STEP : " + stepNumber + " : EXPECTED : Interface IP address should not be shown");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Interface  got the correct IPV4 address";
			LOGGER.info("Going to wait for 1.5 minutes after connecting the client to the wifi network");
			// wait for 1.5 min after connecting
			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
			String osType = ((Device) connectedDeviceActivated).getOsType();
			status = !BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
					osType, connectedDeviceActivated, tapEnv);
			if (status) {
				LOGGER.info("STEP : " + stepNumber + ": ACTUAL : Interface did'nt got the correct IPv4  address");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, false);

			/**
			 * STEP 8:Verify whether interface did'nt get the correct IPv6 address.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;

			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP : " + stepNumber
						+ " : DESCRIPTION : Verify whether interface did'nt get the correct IPv6  address.");
				LOGGER.info("STEP : " + stepNumber + " : ACTION : Get the device IPv4 address using below command"
						+ "Linux : ifconfig wlan0 |grep -i " + "inet6 addr:" + "Windows:ipconfig |grep -A 10 "
						+ "Wireless LAN adapter Wi-Fi" + " |grep -i " + "Pv6 Address");
				LOGGER.info("STEP : " + stepNumber + " : EXPECTED : Interface IP address should not be shown");
				LOGGER.info("**********************************************************************************");
				errorMessage = " Interface  got the correct IPV6 address";
				status = !BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
						osType, connectedDeviceActivated, tapEnv);
				if (status) {
					LOGGER.info("STEP : " + stepNumber + ": ACTUAL : Interface did'nt got the correct IPv6  address");
				} else {
					LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 8 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			/**
			 * STEP 9:Verify whether there is no connectivity using that particular
			 * interface using IPV4.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify whether there is no connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Execute command :"
					+ "Linux :  curl -4 -f --interface <interface name> www.google.com"
					+ "Windows:ping www.google.com -4");
			LOGGER.info(
					"STEP : " + stepNumber + " : EXPECTED : Connectivity check should'nt  return the status as 200");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Connectivty check using IPV4 address success";
			command = ((Device) connectedDeviceActivated).getOsType()
					.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
							? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
									.replace("<INTERFACE>", BroadbandPropertyFileHandler.getLinuxClientWifiInterface())
							: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
			response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
			status = !(CommonMethods.isNotNull(response)
					&& response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK));
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Connectivity using that particular interface using IPV4 Failed");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, false);

			/**
			 * STEP 10:Verify whether there is no connectivity using that particular
			 * interface using IPV6.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP : " + stepNumber
						+ " : DESCRIPTION : Verify whether there is no  connectivity using that particular interface using IPV6");
				LOGGER.info("STEP : " + stepNumber + " : ACTION : Execute command :"
						+ "Linux :  curl -6 -f --interface <interface name> www.google.com"
						+ "Windows: ping www.google.com -6");
				LOGGER.info("STEP : " + stepNumber + " : EXPECTED : Connectivity check should'nt return status as 200");
				LOGGER.info("**********************************************************************************");
				errorMessage = " Connectivty check using IPV6 address success";
				command = ((Device) connectedDeviceActivated).getOsType()
						.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
								? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV6_ADDRESS
								: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV6_ADDRESS;
				response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
				status = !(CommonMethods.isNotNull(response)
						&& response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK));
				if (status) {
					LOGGER.info("STEP : " + stepNumber
							+ ": ACTUAL : Connectivity using that particular interface using IPV6 Failed");
				} else {
					LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, false);

			} else {
				LOGGER.info("IPv6 is not available/disabled : Skipping Step 10 ...");
				tapEnv.updateExecutionForAllStatus(device, testId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			/**
			 * STEP 11:Connect the connected client device whose wifi Mac address is not
			 * added in the MAC Filter to 2.4 GHz SSID and verify connection status
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Connect the connected client device whose wifi Mac address is not added in the MAC Filter to 2.4 GHz SSID and verify connection status");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Connect to 2.4 GHz wifi using below commands"
					+ "Linux :nmcli dev wifi connect <ssid> password <passwd>"
					+ "Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should be connected with 2.4 GHz wifi network since the MAC Filter mode is configured as 'Deny'");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Connection to 2.4Ghz device failed";
			LOGGER.info("Going to wait for 1 minute before connecting the client to the wifi network");
			// wait for 1 min for before connecting to the ssid
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			try {
				lockedDevices = ((Device) device).getConnectedDeviceList();
				connectedDeviceActivated = BroadBandConnectedClientUtils.getOtherConnectedClient(
						connectedDeviceActivated, tapEnv, lockedDevices, BroadBandTestConstants.WIFI,
						BroadBandTestConstants.BAND_2_4GHZ);
				LOGGER.info(
						"Wifi Mac Address of the Connected client whose wifi Mac address is not added in the MAC Filter is:-"
								+ ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress());
				ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				LOGGER.info("ssidName is " + ssidName);
				passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
						tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				LOGGER.info("passPhraseName is " + ssidName);

				status = ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
						passPhraseName);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Connected client device whose Macaddress is not added in the MAC Filter is connected to 2.4GHZ wifi network since the MAC Filter mode is configured as 'Deny'");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 12-15 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH PRIVATE WIFI 5 GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6
			 * INTERFACE.
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, connectedDeviceActivated,
					stepNumber);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Following exception occured during execution : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNum, status, errorMessage, true);
		} finally {
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"POST CONDITION 1: Verify whether MAC Filtering mode is configured as 'Allow-All'(default Mode) by setting TR-181 parameter 'Device.WiFi.AccessPoint.10001.X_CISCO_COM_MACFilter.Enable' as 'false' using WebPA");
			LOGGER.info("POST-CONDITION 1 : ACTION : Execute webpa command:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_ENABLE);
			LOGGER.info(
					"EXPECTED:Device should set the  MAC filtering mode as 'Allow All' when we disable the MAC Filter.");
			LOGGER.info("#####################################################################################");
			errorMessage = "Failed to disable the Mac filter for 2.4 GHz ";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_MAC_FILTER_ENABLE,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("POST-CONDITION 1 : ACTUAL : Successfully disabled the Mac filter for 2.4 GHz");
			} else {
				LOGGER.error("POST-CONDITION 1 : ACTUAL :" + errorMessage);
			}
			if (null != tableRowNumber) {
				LOGGER.info("#####################################################################################");
				LOGGER.info(
						"POST CONDITION 2: Verify and  delete  the device wifi Mac address in the MAC Filter list.");
				LOGGER.info("POST-CONDITION 2 : ACTION : Delete the MAC address in mac filter lise using rest service");
				LOGGER.info(
						"POST-CONDITION 2 :EXPECTED:Should be able to delete the added wifi Mac address in MAC Filter list using webpa Delete command");
				LOGGER.info("#####################################################################################");
				errorMessage = "Failed to delete  the device wifi Mac address in the MAC Filter list ";
				status = false;
				webPaServerResponse = null;
				webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRowNumber);
				if (null != webPaServerResponse) {
					status = CommonMethods.isNotNull(webPaServerResponse.getMessage())
							&& webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
				}
				if (status) {
					LOGGER.info(
							"POST-CONDITION 2 : ACTUAL : Successfully delete  the device wifi Mac address in the MAC Filter list");
				} else {
					LOGGER.error("POST-CONDITION 2 : ACTUAL :" + errorMessage);
				}
			}
			LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-5013");
		}
	}

	/**
	 * This test case is to Verify the controlled user access (Deny) for 5GHZ
	 * against different MAC addresses by way of MAC ACLs
	 * 
	 * <ol>
	 * <li>STEP 1: Getting the Wifi Mac address of Connected client having 5GHZ wifi
	 * Capability.</li>
	 * <li>STEP 2: Verify and add the device wifi Mac address in the MAC Filter list
	 * by using WEBPA command.</li>
	 * <li>STEP 3: Verify the TR-181 parameter
	 * 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as true using
	 * Webpa.</li>
	 * <li>STEP 4: Verify whether the MAC Filtering mode is configured as 'Deny' by
	 * setting TR-181 parameter
	 * 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList' as
	 * 'true' using WebPA for 5GHZ.</li>
	 * <li>STEP 5:verify getting the Configured MAC Filter Mode by using this
	 * TR-181parameter
	 * 'Device.WiFi.AccessPoint.10101.X_COMCAST-COM_MAC_FilteringMode'</li>
	 * <li>STEP 6: Connect the connected client device whose wifi Mac address is
	 * added in the MAC Filter to 5GHz SSID and verify connection status</li>
	 * <li>STEP 7: Verify whether interface did'nt get the correct IPv4
	 * address.</li>
	 * <li>STEP 8 : Verify whether interface did'nt get the correct IPv6
	 * address.</li>
	 * <li>STEP 9: Verify whether there is no connectivity using that particular
	 * interface using IPV4</li>
	 * <li>STEP 10: Verify whether there is no connectivity using that particular
	 * interface using IPV6</li>
	 * <li>STEP 11: Connect the connected client device whose wifi Mac address is
	 * not added in the MAC Filter to 5GHz SSID and verify connection status</li>
	 * <li>STEP 12 :Verify whether interface got the correct IPv4 address</li>
	 * <li>STEP 13 :Verify whether interface got the correct IPv6 address</li>
	 * <li>STEP 14 :Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * <li>STEP 15 :Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * </ol>
	 * 
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor yamini.s
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			TestGroup.WEBPA, TestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-5015")
	public void testVerifyMacFilterAsDenyFor5Ghz(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-WIFI-515";
		// Test step number
		int stepNumber = 1;
		String stepNum = "S" + stepNumber;
		// String to store the error message
		String errorMessage = null;
		// server response
		String response = null;
		// connected device to be verified
		Dut connectedDeviceActivated = null;
		// String to store the Wifi Macaddress for 5GHZ connected client
		String wifiMacAddress = null;
		// String to store the added table row number
		String tableRowNumber = null;
		// string to store the webpaserver response
		WebPaServerResponse webPaServerResponse = null;
		// string to store the ssid name
		String ssidName = null;
		// string to store the password
		String passPhraseName = null;
		// string to store the command
		String command = null;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TESTCASE: TC-RDKB-WIFI-5015");
			LOGGER.info(
					"TEST DESCRIPTION: Verify controlled user access (Deny) for 5GHZ against different MAC addresses by way of MAC ACLs");

			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"Pre condition 1: Verify whether Private 5 GHz SSID 'Device.WiFi.SSID.10101.Enable' is enabled using WebPA.");
			LOGGER.info("Step 1 : Getting the Wifi Mac address of Connected client having 5GHZ wifi Capability.");
			LOGGER.info(
					"Step 2 : Verify and add the device wifi Mac address in the MAC Filter list by using WEBPA command.");
			LOGGER.info(
					"Step 3 : Verify the TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as true using Webpa.");
			LOGGER.info(
					"Step 4 : Verify whether the MAC Filtering mode is configured as 'Deny' by setting TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList' as 'true' using WebPA for 5GHZ.");
			LOGGER.info(
					"Step 5 : verify getting the Configured MAC Filter Mode by using  this TR-181parameter 'Device.WiFi.AccessPoint.10101.X_COMCAST-COM_MAC_FilteringMode'");
			LOGGER.info(
					"Step 6 : Connect the connected client device whose wifi Mac address is  added in the MAC Filter to 5 GHz SSID and verify connection status");
			LOGGER.info("Step 7 : Verify whether the interface did'nt get the correct IPv4  address.");
			LOGGER.info("Step 8 : Interface IP address should not be shown");
			LOGGER.info("Step 9 : Verify whether there is no connectivity using that particular interface using IPV4");
			LOGGER.info(
					"Step 10 : Verify whether there is no  connectivity using that particular interface using IPV6");
			LOGGER.info(
					"Step 11 : Connect the connected client device whose wifi Mac address is not added in the MAC Filter to 5GHz SSID and verify connection status");
			LOGGER.info("Step 12 : Verify whether interface got the correct IPv4  address.");
			LOGGER.info("Step 13 : Verify whether interface got the correct  IPv6 address.");
			LOGGER.info("Step 14 : Verify whether you have connectivity using that particular interface using IPV4");
			LOGGER.info("Step 15 : Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info(
					"Post Condition 1 : Verify whether MAC Filtering mode is configured as 'Allow-All'(default Mode) by setting TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as 'false' using WebPA");
			LOGGER.info(
					"Post Condition 2 : Verify and  delete  the device wifi Mac address in the MAC Filter list by using WEBPA DELETE command.");
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			/**
			 * PRE-CONDITION :Enable Private 5 GHz SSID via WebPA
			 */
			errorMessage = null;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION 1 : DESCRIPTION : Verify whether private 5 GHz ssid 'Device.WiFi.SSID.10101.Enable' is enabled, if not enable the private 5 GHz ssid ");
			LOGGER.info(
					"PRE-CONDITION 1 : ACTION : Verify whether private 5 GHz ssid 'Device.WiFi.SSID.10101.Enable' is enabled,if not enable the private 5 GHz ssid using webpa ");
			LOGGER.info(
					"PRE-CONDITION 1 : EXPTECTED : Device should be enabled with private 5 GHz ssid and response should be true");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to enable the 5 GHz private ssid on this device";
			try {
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						BroadBandTestConstants.TRUE);
			} catch (TestException exception) {
				LOGGER.error(errorMessage + " : " + exception.getMessage());
			}
			if (!status) {
				errorMessage = "Unable to set the private 5 GHz ssid status as 'true'.";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : Private 5 GHz ssid verified/enabled in gateway device.");
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1:Getting the Wifi Mac address of Connected client having 5GHZ wifi
			 * Capability.
			 */
			status = false;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Getting the Wifi Mac address of Connected client having 5GHZ wifi Capability.");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Execute WebPA command : Device.WiFi.SSID.10101.Enable");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should be able to get the Wifi Mac address of the connected client having 5GHZ wifi Capability");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Unable to retrieve the Wifi Mac address of the connected client having 5GHZ wifi Capability";
			List<Dut> lockedDevices = ((Device) device).getConnectedDeviceList();
			try {
				connectedDeviceActivated = BroadBandConnectedClientUtils.getConnectedClientBasedOnTypeAndBand(device,
						tapEnv, lockedDevices, BroadBandTestConstants.WIFI, BroadBandTestConstants.BAND_5GHZ);
				wifiMacAddress = ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress();
				LOGGER.info("Obtained mac address of the connected client is:" + wifiMacAddress);
				status = CommonMethods.isNotNull(wifiMacAddress);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}

			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Successfully retrieved the Wifi Mac address of the connected client having 5GHZ wifi Capability ");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 2:Verify and add the device wifi Mac address in the MAC Filter list by
			 * using WEBPA command.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify and add the device wifi Mac address in the MAC Filter list by using WEBPA command.");
			LOGGER.info("STEP : " + stepNumber
					+ " : ACTION : Execute webpa post command : Device.WiFi.AccessPoint.10101.X_CISCO_COM_MacFilterTable.");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should add the device wifi Mac address in the MAC Filter list");
			LOGGER.info("**********************************************************************************");
			Map<String, List<String>> macFilterTable = new HashMap<String, List<String>>();
			List<String> MacAddressList = new ArrayList<String>();
			MacAddressList.add(wifiMacAddress);
			LOGGER.info("Wifi Mac Address of the Connected client obtained is:-" + wifiMacAddress);
			List<String> DeviceList = new ArrayList<String>();
			DeviceList.add(BroadBandTestConstants.CONNECTED_DEVICE);
			// adding to the Map.
			macFilterTable.put(BroadBandTestConstants.MAC_ADDRESS, MacAddressList);
			macFilterTable.put(BroadBandTestConstants.DEVICE_NAME, DeviceList);
			errorMessage = "Unable to add the wifi Mac Address to the  MAC Filter filter by Webpa POST command";
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_TABLE, macFilterTable);
			if (null != webPaServerResponse) {
				tableRowNumber = webPaServerResponse.getRow();
				LOGGER.info("tableRowNumber is" + tableRowNumber);
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Successfully added the wifi Mac Address to the  MAC Filter filter by Webpa POST command");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 3:Verify the TR-181 parameter
			 * "Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable" as true using
			 * Webpa.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify the TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as true using Webpa.");
			LOGGER.info("STEP : " + stepNumber
					+ " : ACTION : Execute webpa set command : Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should set the TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as true");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to Set the the TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as true";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_ENABLE,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Successfully Set TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as true via webpa");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 4:Verify whether MAC Filtering mode is configured as "Deny" by setting
			 * TR-181 parameter
			 * "Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList" as
			 * "true" using WebPA for 2.4GHZ.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify whether the MAC Filtering mode is configured as 'Deny' by setting TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList' as 'true' using WebPA for 5GHZ.");
			LOGGER.info("STEP : " + stepNumber
					+ " : ACTION : Execute webpa set command : Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should have the MAC filtering mode as 'Deny' when we set 'true' to the TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList'");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to Set the MAC Filter mode as Deny by setting true to the  TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList' by using Webpa commands.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_BLACK_LIST,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Successfully Set the MAC Filter mode as  Deny by setting the TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.FilterAsBlackList' as true via webpa");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 5:verify getting the Configured MAC Filter Mode by using this
			 * TR-181parameter Device.WiFi.AccessPoint.10101.X_COMCAST-COM_MAC_FilteringMode
			 * .
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : verify getting the Configured MAC Filter Mode by using  this TR-181parameter 'Device.WiFi.AccessPoint.10101.X_COMCAST-COM_MAC_FilteringMode'");
			LOGGER.info("STEP : " + stepNumber
					+ " : ACTION : Execute webpa get command : Device.WiFi.AccessPoint.10001.X_COMCAST-COM_MAC_FilteringMode");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : The configured MAC Filter mode for the device should be as Deny");
			LOGGER.info("**********************************************************************************");
			errorMessage = "MAC Filter mode is not configured as Deny";
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_MODE);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.MAC_FILTER_DENY);
			if (status) {
				LOGGER.info("STEP : " + stepNumber + ": ACTUAL : MAC Filter mode for the device is configured as Deny");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 6:Connect the connected client device whose wifi Mac address is added in
			 * the MAC Filter to 2.4 GHz SSID and verify connection status
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Connect the connected client device whose wifi Mac address is  added in the MAC Filter to 5 GHz SSID and verify connection status");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Connect to 5GHz wifi using below commands "
					+ "Linux :nmcli dev wifi connect <ssid> password <passwd>"
					+ "Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should not be connected with 5 GHz wifi network since the MAC Filter mode is configured as 'Deny'");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Connection to 5Ghz device is successful even though the MAC Filter mode is configured as 'Deny'";
			LOGGER.info("Going to wait for 1 minute before connecting the client to the wifi network");
			// wait for 1 min for before connecting to the ssid
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			try {
				LOGGER.info(
						"Wifi Mac Address of the Connected client whose wifi Mac address is  added in the MAC Filter is:-"
								+ ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress());
				ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
						tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);

				status = !ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
						passPhraseName);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL : Connected client device whose wifi Mac address is  added in the MAC Filter is not connected to 5GHZ wifi network since the MAC Filter mode is configured as 'Deny'");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, false);

			/**
			 * STEP 7:Verify whether the interface did'nt get the correct IPv4 address.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify whether the interface did'nt get the correct IPv4  address.");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Get the device IPv4 address using below command"
					+ "Linux : ifconfig wlan0 |grep -i " + "inet addr:" + "Windows: ipconfig |grep -A 10 "
					+ "Wireless LAN adapter Wi-Fi" + " |grep -i " + "Pv4 Address");
			LOGGER.info("STEP : " + stepNumber + " : EXPECTED : Interface IP address should not be shown");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Interface  got the correct IPV4 address";
			LOGGER.info("Going to wait for 1.5 minutes after connecting the client to the wifi network");
			// wait for 1.5 min after connecting
			tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
			String osType = ((Device) connectedDeviceActivated).getOsType();
			status = !BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
					osType, connectedDeviceActivated, tapEnv);
			if (status) {
				LOGGER.info("STEP : " + stepNumber + ": ACTUAL : Interface did'nt got the correct IPv4  address");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, false);

			/**
			 * STEP 8:Verify whether interface did'nt get the correct IPv6 address.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify whether interface did'nt get the correct IPv6  address.");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Get the device IPv4 address using below command"
					+ "Linux : ifconfig wlan0 |grep -i " + "inet6 addr:" + "Windows:ipconfig |grep -A 10 "
					+ "Wireless LAN adapter Wi-Fi" + " |grep -i " + "Pv6 Address");
			LOGGER.info("STEP : " + stepNumber + " : EXPECTED : Interface IP address should not be shown");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Interface  got the correct IPV6 address";
			status = !BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
					osType, connectedDeviceActivated, tapEnv);
			if (status) {
				LOGGER.info("STEP : " + stepNumber + ": ACTUAL: Interface did'nt got the correct IPv6  address");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, false);

			/**
			 * STEP 9:Verify whether there is no connectivity using that particular
			 * interface using IPV4.
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify whether there is no connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Execute command :"
					+ "Linux :  curl -4 -f --interface <interface name> www.google.com"
					+ "Windows:ping www.google.com -4");
			LOGGER.info(
					"STEP : " + stepNumber + " : EXPECTED : Connectivity check should'nt  return the status as 200");
			LOGGER.info("**********************************************************************************");
			status = false;
			errorMessage = "Connectivty check using IPV4 address success";
			command = ((Device) connectedDeviceActivated).getOsType()
					.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
							? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
									.replace("<INTERFACE>", BroadbandPropertyFileHandler.getLinuxClientWifiInterface())
							: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV4_ADDRESS;
			response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
			status = !(CommonMethods.isNotNull(response)
					&& response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK));
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL: Connectivity using that particular interface using IPV4 Failed");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, false);

			/**
			 * STEP 10:Verify whether there is no connectivity using that particular
			 * interface using IPV6.
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Verify whether there is no  connectivity using that particular interface using IPV6");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Execute command :"
					+ "Linux :  curl -6 -f --interface <interface name> www.google.com"
					+ "Windows: ping www.google.com -6");
			LOGGER.info("STEP : " + stepNumber + " : EXPECTED : Connectivity check should'nt return status as 200");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Connectivty check using IPV6 address success";
			command = ((Device) connectedDeviceActivated).getOsType()
					.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
							? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV6_ADDRESS
							: BroadBandConnectedClientTestConstants.COMMAND_CURL_WINDOWS_IPV6_ADDRESS;
			response = tapEnv.executeCommandOnOneIPClients(connectedDeviceActivated, command);
			status = !(CommonMethods.isNotNull(response)
					&& response.contains(BroadBandConnectedClientTestConstants.RESPONSE_STATUS_OK));
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL: Connectivity using that particular interface using IPV6 Failed");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, false);

			// wait for 1 min for before connecting to the ssid
			tapEnv.waitTill(60000);
			/**
			 * STEP 11:Connect the connected client device whose wifi Mac address is not
			 * added in the MAC Filter to 5 GHz SSID and verify connection status
			 * 
			 */
			status = false;
			stepNumber++;
			stepNum = "S" + stepNumber;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP : " + stepNumber
					+ " : DESCRIPTION : Connect the connected client device whose wifi Mac address is not added in the MAC Filter to 5GHz SSID and verify connection status");
			LOGGER.info("STEP : " + stepNumber + " : ACTION : Connect to 5 GHz wifi using below commands"
					+ "Linux :nmcli dev wifi connect <ssid> password <passwd>"
					+ "Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP : " + stepNumber
					+ " : EXPECTED : Device should be connected with  5GHz wifi network since the MAC Filter mode is configured as 'Deny'");
			LOGGER.info("**********************************************************************************");
			errorMessage = " Connection to 5Ghz device failed";
			LOGGER.info("Going to wait for 1 minute before connecting the client to the wifi network");
			// wait for 1 min for before connecting to the ssid
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			try {
				lockedDevices = ((Device) device).getConnectedDeviceList();
				connectedDeviceActivated = BroadBandConnectedClientUtils.getOtherConnectedClient(
						connectedDeviceActivated, tapEnv, lockedDevices, BroadBandTestConstants.WIFI,
						BroadBandTestConstants.BAND_5GHZ);
				LOGGER.info(
						"Wifi Mac Address of the Connected client whose wifi Mac address is not added in the MAC Filter is:-"
								+ ((Device) connectedDeviceActivated).getConnectedDeviceInfo().getWifiMacAddress());
				ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				LOGGER.info("ssidName is " + ssidName);
				passPhraseName = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
						tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				LOGGER.info("passPhraseName is " + ssidName);

				status = ConnectedNattedClientsUtils.connectToSSID(connectedDeviceActivated, tapEnv, ssidName,
						passPhraseName);
			} catch (TestException exception) {
				errorMessage = exception.getMessage();
				throw new TestException(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP : " + stepNumber
						+ ": ACTUAL: Connected client device whose Macaddress is not added in the MAC Filter is connected to 5GHZ wifi network since the MAC Filter mode is configured as 'Deny'");
			} else {
				LOGGER.error("STEP : " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

			/**
			 * STEP 12-15 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH PRIVATE WIFI 5 GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6
			 * INTERFACE.
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, connectedDeviceActivated,
					stepNumber);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Following exception occured during execution : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNum, status, errorMessage, true);
		} finally {
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"POST CONDITION 1: Verify whether MAC Filtering mode is configured as 'Allow-All'(default Mode) by setting TR-181 parameter 'Device.WiFi.AccessPoint.10101.X_CISCO_COM_MACFilter.Enable' as 'false' using WebPA");
			LOGGER.info("POST-CONDITION 1 : ACTION : Execute webpa command:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_ENABLE);
			LOGGER.info(
					"EXPECTED:Device should set the  MAC filtering mode as 'Allow All' when we disable the MAC Filter.");
			LOGGER.info("#####################################################################################");
			errorMessage = "Failed to disable the Mac filter for 5 GHz ";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_ENABLE,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("POST-CONDITION 1 : ACTUAL : Successfully disabled the Mac filter for 5 GHz");
			} else {
				LOGGER.error("POST-CONDITION 1 : ACTUAL :" + errorMessage);
			}
			if (null != tableRowNumber) {
				LOGGER.info("#####################################################################################");
				LOGGER.info(
						"POST CONDITION 2: Verify and  delete  the device wifi Mac address in the MAC Filter list.");
				LOGGER.info("POST-CONDITION 2 : ACTION : Delete the MAC address in mac filter lise using rest service");
				LOGGER.info(
						"POST-CONDITION 2 :EXPECTED:Should be able to delete the added wifi Mac address in MAC Filter list using webpa Delete command");
				LOGGER.info("#####################################################################################");
				errorMessage = "Failed to delete  the device wifi Mac address in the MAC Filter list ";
				status = false;
				webPaServerResponse = null;
				webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRowNumber);
				if (null != webPaServerResponse) {
					status = CommonMethods.isNotNull(webPaServerResponse.getMessage())
							&& webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
				}
				if (status) {
					LOGGER.info(
							"POST-CONDITION 2 : ACTUAL : Successfully delete  the device wifi Mac address in the MAC Filter list");
				} else {
					LOGGER.error("POST-CONDITION 2 : ACTUAL :" + errorMessage);
				}
			}
			LOGGER.info("ENDING TESTCASE : TC-RDKB-WIFI-5015");
		}
	}

	/**
	 * Verify enhanced Wifi logging in Telemetry for 2.4ghz private wifi parameters
	 * <ol>
	 * <li>Get 2.4GHz WiFi connected client device</li>
	 * <li>Verify wifihealth.txt file availability in /rdklogs/logs folder and clear
	 * the contents</li>
	 * <li>Check the whether wifi statistics are enabled</li>
	 * <li>Verify WiFi log interval is 3600 by default using WebPA command</li>
	 * <li>Change the WiFi log interval to 300sec using WebPA command" and wait for
	 * 5 minutes</li>
	 * <li>Run the script sh /usr/ccsp/wifi/aphealth_log.sh</li>
	 * <li>Verify the log print for RSSI value for client device connected with 2.4
	 * GHz wifi band</li>
	 * <li>Verify the log print for bytes of AP RX for client device connected with
	 * 2.4 GHz wifi band</li>
	 * <li>Verify the log print for RX DELTA of current and previous value for
	 * client device connected with 2.4 GHz wifi band</li>
	 * <li>Change the WiFi log interval to default of 3600sec using WebPA
	 * command</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author anandam.s
	 * @refactor Said Hisham
	 * 
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-WIFI-TELEMETRY-1000")
	public void verifyTelemetryMarkersFor24ghzClients(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-TELEMETRY-100";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1000");
		LOGGER.info("TEST DESCRIPTION: Verify enhanced Wifi logging in Telemetry for 2.4ghz  private wifi parameters");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Get 2.4GHz WiFi connected client device");
		LOGGER.info("2. Verify wifihealth.txt file availability in /rdklogs/logs folder  and clear the contents");
		LOGGER.info("3. Check the whether wifi statistics are enabled ");
		LOGGER.info("4. Verify WiFi log interval is 3600 by default using WebPA command");
		LOGGER.info("5. Change the WiFi log interval to 300sec using WebPA command\" and wait for 5 minutes ");
		LOGGER.info("6. Run the script sh /usr/ccsp/wifi/aphealth_log.sh");
		LOGGER.info("7. Verify the log print for  RSSI value for client device connected with 2.4 GHz wifi band");
		LOGGER.info("8. Verify the log print for bytes of AP RX for  client device connected with 2.4 GHz wifi band");
		LOGGER.info(
				"9. Verify the log print for RX DELTA of current and previous value for  client device connected with 2.4 GHz wifi band");
		LOGGER.info("10. Change the WiFi log interval to default of 3600sec using WebPA command");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			int preConStepNumber = 1;
			BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv, preConStepNumber);

			preConStepNumber++;
			errorMessage = null;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION " + preConStepNumber + " : DESCRIPTION : REACTIVATE THE ROUTER DEVICE");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTION : SET VALUES TO 2.4GHz AND 5GHz - PRIVATE SSID AND PASSWORD");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : EXPECTED : THE ROUTER DEVICE SHOULD BE REACTIVATED SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO REACTIVATE THE ROUTER DEVICE";
			status = false;
			try {
				BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
				status = true;
			} catch (TestException e) {
				errorMessage = e.getMessage();
			}
			if (status) {
				LOGGER.info(
						"PRE-CONDITION " + preConStepNumber + " : ACTUAL: THE ROUTER DEVICE REACTIVATED SUCCESSFULLY.");
			} else {
				LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL: " + errorMessage);
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : "
						+ preConStepNumber + " FAILED : " + errorMessage);
			}

			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 3: DESCRIPTION :Check whether 2.4ghz private wifi is enabled. ");
			LOGGER.info("PRE-CONDITION 3: ACTION : Execute get on webpa Device.WiFi.SSID.10001.Enable");
			LOGGER.info("PRE-CONDITION 3: EXPECTED :2.4ghz private wifi  should be in enabled state.  ");
			LOGGER.info("#######################################################################################");
			String response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS);
			status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info(
						"PRE-CONDITION 3: ACTUAL : Pre condition executed successfully.2.4ghz private wifi is in enabled state.");
			} else {
				LOGGER.error(
						"PRE-CONDITION 3: ACTUAL : Pre condition failed. Trying to eanble the 2.4ghz private WIFI");

				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
				if (!status) {
					errorMessage = "Could not enable 2.4ghz private wifi.So skipping the tests";
					throw new TestException(errorMessage);
				}

			}

			LOGGER.info("**********************************************************************************");

			stepNum = "s1";
			errorMessage = "Failed to get a 2.4ghz connected client device";
			status = false;
			Dut twoGhzWifiSettop = null;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Get a 2.4GHz private WiFi connected client device");
			LOGGER.info("STEP 1: ACTION : Get 2.4GHz WiFi connected client device");
			LOGGER.info("STEP 1: EXPECTED :  Should get 2.4GHz WiFi connected device");
			LOGGER.info("**********************************************************************************");

			try {
				twoGhzWifiSettop = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
				status = (twoGhzWifiSettop != null);
			} catch (Exception e) {
				errorMessage = "Exception while trying to connect a client to 2.4ghz private WIFI";
				LOGGER.error(errorMessage + " . " + e.getMessage());
			}

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully connected a client to 2.4ghz private wifi");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/** Step 2 to Step 10 */
			executeCommonStepsForTelemetryTestCases(testCaseId, stepNum, device, WiFiFrequencyBand.WIFI_BAND_2_GHZ,
					BroadBandTestConstants.STRING_VALUE_ONE, twoGhzWifiSettop);

		} catch (

		Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-TELEMETRY-1000");
	}

	/**
	 * Test Case : Verify Connecting Multiple Clients to the Gateway device via
	 * Wi-Fi network and check the internet connectivity and Validate the status of
	 * Wi-Fi Radio and current Wi-Fi channel for 2.4 and 5 Ghz.
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Perform factory reset on a device</li>
	 * <li>PRE-CONDITION 2 : Reactivate the Device</li>
	 * <li>S1 Verify the status of Wi-Fi Radio for 2.4</li>
	 * <li>S2 Verify the status of Wi-Fi Radio for 5 Ghz</li>
	 * <li>S3 Verify the status of current Wi-Fi channel for 2.4ghz</li>
	 * <li>S4 Verify the status of current Wi-Fi channel for 5ghz</li>
	 * <li>S5 Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>S6 Verify number of clients associated with 2.4 GHz using webpa</li>
	 * <li>S7 Verify the client mac address and connection type connected with 2.4
	 * GHz in LM.txt.0 log file</li>
	 * <li>S8 Verify the connected clients details are IP and MAC in Gateway</li>
	 * <li>S9 Verify the correct IPv4 address for client connected with 2.4 GHz
	 * SSID</li>
	 * <li>S10 Verify the correct IPv6 address for client connected with 2.4 GHz
	 * SSID</li>
	 * <li>S11 Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>S12 Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz</li>
	 * <li>S13 Connect the client 2 to 5 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>S14 Verify number of clients associated with 5 GHz using webpa</li>
	 * <li>S15 Verify the client mac address and connection type connected with 5
	 * GHz in LM.txt.0 log file</li>
	 * <li>S16 Verify the connected clients details are IP and MAC in Gateway</li>
	 * <li>S17 Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>S18 Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>S19 Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 5 Ghz</li>
	 * <li>S20 Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 5 Ghz</li>
	 * <li>S21 Verify client 3 to ethernet with ethernet</li>
	 * <li>S22 Verify the client mac address and connection type connected with
	 * ethernet in LM.txt.0 log file</li>
	 * <li>S23 Verify the IPv4 Address is retrieved from the client connected with
	 * Ethernet</li>
	 * <li>S24 Verify the IPv6 Address is retrieved from the client connected with
	 * Ethernet</li>
	 * <li>S25 Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with ethernet</li>
	 * <li>S26 Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with ethernet</li>
	 *
	 * @author Muthukumar
	 * @refactor Athira
	 * @param device instance of {@link Dut}
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-5016")
	public void testToVerifyWifiConnectivityOnMultipleClients(Dut device) {
		String testId = "TC-RDKB-WIFI-516";
		int stepNumber = 1;
		String testStepNumber = "S" + stepNumber;
		String errorMessage = null;
		boolean status = false;
		String deviceDateTime = null;
		boolean isReactivated = false;
		String webPaResponse = null;
		int deviceCount = 0;

		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-5016");
			LOGGER.info(
					"DESCREPTION : Verify Connecting Multiple Clients to the Gateway device via Wi-Fi network and check the internet connectivity");
			LOGGER.info("S1 Verify the status of Wi-Fi Radio  for 2.4ghz ");
			LOGGER.info("S2 Verify the status of Wi-Fi Radio  for 5 ghz");
			LOGGER.info("S3 Verify the status of current Wi-Fi channel for 2.4 ghz");
			LOGGER.info("S4 Verify the status of current Wi-Fi channel for 5 ghz");
			LOGGER.info("S5 Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify connection status");
			LOGGER.info("S6 Verify number of clients associated with 2.4 GHz using webpa");
			LOGGER.info(
					"S7 Verify the client mac address and connection type connected with 2.4 GHz in LM.txt.0 log file");
			LOGGER.info("S8 Verify the connected clients details are IP and MAC in Gateway");
			LOGGER.info("S9 Verify the correct IPv4 address for client connected with 2.4 GHz SSID");
			LOGGER.info("S10 Verify the correct IPv6 address for client connected with 2.4 GHz SSID");
			LOGGER.info(
					"S11 Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4GHz");
			LOGGER.info(
					"S12 Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4GHz");
			LOGGER.info("S13 Connect the client 2 to 5 GHz Private Wi-Fi Network and verify connection status");
			LOGGER.info("S14 Verify number of clients associated with 5 GHz using webpa");
			LOGGER.info(
					"S15 Verify the client mac address and connection type connected with 5 GHz in LM.txt.0 log file");
			LOGGER.info("S16 Verify the connected clients details are IP and MAC in Gateway");
			LOGGER.info("S17 Verify the correct IPv4 address for client connected with 5 GHz SSID");
			LOGGER.info("S18 Verify the correct IPv6 address for client connected with 5 GHz SSID");
			LOGGER.info(
					"S19 Verify whether have connectivity using that particular interface using IPV4 for client connected with 5Ghz");
			LOGGER.info(
					"S20 Verify whether have connectivity using that particular interface using IPV6 for client connected with 5Ghz");
			LOGGER.info("S21 Verify client 3 to ethernet with ethernet");
			LOGGER.info(
					"S22 Verify the client mac address and connection type connected with ethernet in LM.txt.0 log file");
			LOGGER.info("S23 Verify the IPv4 Address is retrieved from the client connected with Ethernet");
			LOGGER.info("S24 Verify the IPv6 Address is retrieved from the client connected with Ethernet");
			LOGGER.info(
					"S25 Verify whether have connectivity using that particular interface using IPV4 for client connected with ethernet");
			LOGGER.info(
					"S26 Verify whether have connectivity using that particular interface using IPV6 for client connected with ethernet");
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : FACTORY RESET AND VERIFY IF THE DEVICE COMES UP");
			LOGGER.info(
					"PRE-CONDITION 1 : ACTION : FACTORY RESET THE DEVICE USING WEBPA PARAM Device.X_CISCO_COM_DeviceControl.FactoryReset");
			LOGGER.info("PRE-CONDITION 1 : EXPECTED : DEVICE SHOULD COME UP AFTER FACTORY RESET");
			LOGGER.info("#######################################################################################");
			String ethernetDeviceTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);

			status = false;
			stepNumber = 1;
			testStepNumber = "S" + stepNumber;
			status = false;
			try {
				errorMessage = "FAILED TO FACTORY RESET THE DEVICE";
				if (BroadBandCommonUtils.performFactoryResetWebPaByPassingTriggerTime(tapEnv, device,
						BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS)) {
					// Error message
					errorMessage = "WEBPA SERVICE DIDN'T COMEUP AFTER FACTORY RESETTING THE DEVICE";
					status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
				}
			} catch (TestException e) {
				errorMessage = e.getMessage();
			}
			if (status) {
				LOGGER.info("PRE-CONDITION 1 - ACTUAL: DEVICE CAME UP SUCCESSFULLY AFTER FACTORY RESET");
			} else {
				LOGGER.error("PRE-CONDITION 1 - ACTUAL: " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 FAILED : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 2: DESCRIPTION : REACTIVATE THE ROUTER DEVICE");
			LOGGER.info("PRE-CONDITION 2: ACTION : SET VALUES TO 2.4GHz AND 5GHz - PRIVATE SSID AND PASSWORD");
			LOGGER.info("PRE-CONDITION 2: EXPECTED : THE ROUTER DEVICE SHOULD BE REACTIVATED SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			errorMessage = "FAILED TO REACTIVATE THE ROUTER DEVICE";
			status = false;
			try {
				BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
				status = true;
				isReactivated = status;
			} catch (TestException e) {
				errorMessage = e.getMessage();
			}
			if (status) {
				LOGGER.info("PRE-CONDITION 2 - ACTUAL: THE ROUTER DEVICE REACTIVATED SUCCESSFULLY.");
			} else {
				LOGGER.error("PRE-CONDITION 2 - ACTUAL: " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-2 FAILED : " + errorMessage);
			}

			LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");

			/**
			 * STEP 1 : Verify the status of Wi-Fi Radio for 2.4 ghz
			 */
			status = false;
			stepNumber = 1;
			testStepNumber = "S" + stepNumber;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE STATUS OF WI-FI RADIO  FOR 2.4 GHZ");
			LOGGER.info("STEP " + stepNumber + " : ACTION : DEVICE.WIFI.RADIO.10000.STATUS");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : MUST RETURN THE WIFI RADIO STATUS AS UP");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO GET THE WIFI RADIO STATUS FOR 2.4GHZ ";
			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_2_4GHZ);
			status = CommonMethods.isNotNull(webPaResponse) && CommonMethods.patternMatcher(webPaResponse,
					BroadBandConnectedClientTestConstants.RADIO_STATUS_UP);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE STATUS OF WI-FI RADIO FOR 2.4 GHZ");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 2 : Verify the status of Wi-Fi Radio for 5 ghz
			 */
			status = false;
			stepNumber = 2;
			testStepNumber = "S" + stepNumber;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE STATUS OF WI-FI RADIO  FOR 5 GHZ");
			LOGGER.info("STEP " + stepNumber + " : ACTION : DEVICE.WIFI.RADIO.10100.STATUS");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : MUST RETURN THE WIFI RADIO STATUS AS UP");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO GET THE WIFI RADIO STATUS FOR 5 GHZ ";
			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_5GHZ);
			status = CommonMethods.isNotNull(webPaResponse) && CommonMethods.patternMatcher(webPaResponse,
					BroadBandConnectedClientTestConstants.RADIO_STATUS_UP);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY VERIFIED THE STATUS OF WI-FI RADIO FOR 5 GHZ");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 3 : Verify the status of current Wi-Fi channel for 2.4 ghz
			 */
			status = false;
			stepNumber = 3;
			testStepNumber = "S" + stepNumber;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"STEP " + stepNumber + " : DESCRIPTION : VERIFY THE STATUS OF CURRENT WI-FI CHANNEL FOR 2.4 GHZ");
			LOGGER.info("STEP " + stepNumber + " : ACTION : DEVICE.WIFI.RADIO.10000.CHANNEL");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : MUST RETURN THE CURRENT WI-FI CHANNEL FOR 2.4 GHZ");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO GET THE CURRENT WI-FI CHANNEL FOR 2.4GHZ ";
			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ);
			String possibleChannels = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_POSSIBLECHANNELS
							.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.RADIO_24_GHZ_INDEX));
			status = CommonMethods.isNotNull(webPaResponse) && CommonMethods.isNotNull(possibleChannels)
					&& CommonMethods.patternMatcher(possibleChannels, webPaResponse);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE STATUS OF CURRENT WI-FI CHANNEL FOR 2.4 GHZ");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 4 : Verify the status of current Wi-Fi channel for 5 ghz
			 */
			status = false;
			stepNumber = 4;
			testStepNumber = "S" + stepNumber;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY THE STATUS OF CURRENT WI-FI CHANNEL FOR 5 GHZ");
			LOGGER.info("STEP " + stepNumber + " : ACTION : DEVICE.WIFI.RADIO.10100.CHANNEL");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : MUST RETURN THE CURRENT WI-FI CHANNEL FOR 5 GHZ");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO GET THE CURRENT WI-FI CHANNEL FOR 5 GHZ ";
			webPaResponse = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
			possibleChannels = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_POSSIBLECHANNELS
							.replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.RADIO_5_GHZ_INDEX));
			status = CommonMethods.isNotNull(webPaResponse) && CommonMethods.isNotNull(possibleChannels)
					&& CommonMethods.patternMatcher(possibleChannels, webPaResponse);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE STATUS OF CURRENT WI-FI CHANNEL FOR 5 GHZ");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 5 : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID
			 */
			stepNumber = 5;
			testStepNumber = "S" + stepNumber;
			Dut wifi2GhzCnnClient = null;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 2.4GHz SSID";
			try {
				deviceCount = BroadBandConnectedClientUtils.getAssociatedDeviceCount(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_ASSOCIATEDDEVICENUMBEROFENTRIES);
				wifi2GhzCnnClient = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (wifi2GhzCnnClient != null);
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 2.4GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * SETP 6 VERIFY NUMBER OF CONNECTED CLIENTS ASSOCIATED WITH 2.4 GHz .
			 */
			stepNumber = 6;
			testStepNumber = "S" + stepNumber;
			status = false;
			verifyCnnclientCountUsingWebpa(device, testId, wifi2GhzCnnClient, BroadBandTestConstants.BAND_2_4GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_ASSOCIATEDDEVICENUMBEROFENTRIES,
					stepNumber, deviceCount + BroadBandTestConstants.CONSTANT_1);
			/**
			 * SETP 7 : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
			 * /rdklogs/logs/LM.txt.0 LOG
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			verifyConnectedclientDetails(device, testId, wifi2GhzCnnClient, deviceDateTime, stepNumber,
					BroadBandTestConstants.CONSTANT_1, false);

			/**
			 * STEP 8 : VERIFY 2.4GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber = 8;
			testStepNumber = "S" + stepNumber;
			status = false;
			verifyClientMacInGateway(device, testId, wifi2GhzCnnClient, WiFiFrequencyBand.WIFI_BAND_2_GHZ, stepNumber);

			/**
			 * SETP 9 - 12
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			verifyIpv4AndIpV6ConnectionInterface(device, testId, wifi2GhzCnnClient, stepNumber);

			/**
			 * STEP 13 : VERIFY CONNECTING THE WI-FI CLIENT 2 IN THE SETTOP TO 5GHz SSID
			 */
			Dut wifi5GhzCnnClient = null;
			status = false;
			stepNumber = 13;
			testStepNumber = "S" + stepNumber;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT 2 IN THE SETTOP TO 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT 2 WITH 5GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT 2 TO 5GHz SSID";
			try {
				deviceCount = BroadBandConnectedClientUtils.getAssociatedDeviceCount(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_ASSOCIATEDDEVICENUMBEROFENTRIES);
				wifi5GhzCnnClient = BroadBandConnectedClientUtils.getOtherWiFiCapableClientDeviceAndConnect(device,
						tapEnv, wifi2GhzCnnClient, BroadBandTestConstants.BAND_5GHZ);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = wifi5GhzCnnClient != null;
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT 2 TO 5GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * SETP 14 VERIFY NUMBER OF CONNECTED CLIENTS ASSOCIATED WITH 5 GHz .
			 */
			stepNumber = 14;
			testStepNumber = "S" + stepNumber;
			status = false;
			verifyCnnclientCountUsingWebpa(device, testId, wifi5GhzCnnClient, BroadBandTestConstants.BAND_5GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_ASSOCIATEDDEVICENUMBEROFENTRIES,
					stepNumber, deviceCount + BroadBandTestConstants.CONSTANT_1);

			/**
			 * SETP 15 VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
			 * /rdklogs/logs/LM.txt.0 LOG
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			verifyConnectedclientDetails(device, testId, wifi5GhzCnnClient, deviceDateTime, stepNumber,
					BroadBandTestConstants.CONSTANT_1, false);

			/**
			 * STEP 16 : VERIFY 5GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber = 16;
			testStepNumber = "S" + stepNumber;
			status = false;
			verifyClientMacInGateway(device, testId, wifi5GhzCnnClient, WiFiFrequencyBand.WIFI_BAND_5_GHZ, stepNumber);

			/**
			 * SETP 17 - 20
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			verifyIpv4AndIpV6ConnectionInterface(device, testId, wifi5GhzCnnClient, stepNumber);

			/**
			 * STEP 21 : OBTAIN A ETHERNET CLIENT ASSOSIATED WITH THE SETTOP
			 */
			Dut ethernetCnnClient = null;
			status = false;
			stepNumber = 21;
			testStepNumber = "S" + stepNumber;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"STEP :  " + stepNumber + " : DESCRIPTION : OBTAIN A ETHERNET CLIENT ASSOSIATED WITH THE SETTOP");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : OBTAIN A ETHERNET CLIENT ASSOSIATED WITH THE SETTOP");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO OBTAIN A ETHERNET CLIENT ASSOSIATED WITH THE SETTOP";
			try {
				ethernetCnnClient = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = ethernetCnnClient != null;
			if (status) {
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL: OBTAINED A ETHERNET CLIENT ASSOSIATED WITH THE SETTOP SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * SETP 22 VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
			 * /rdklogs/logs/LM.txt.0 LOG
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			verifyConnectedclientDetails(device, testId, ethernetCnnClient, ethernetDeviceTime, stepNumber,
					BroadBandTestConstants.CONSTANT_1, true);

			/**
			 * SETP 23 - 26
			 */
			stepNumber = 23;
			testStepNumber = "S" + stepNumber;
			status = false;
			verifyIpv4AndIpV6ConnectionInterface(device, testId, ethernetCnnClient, stepNumber);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE EXECUTING TEST CASE 'TC-RDKB-WIFI-5016' : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : REACTIVATE THE ROUTER DEVICE");
			LOGGER.info("POST-CONDITION 1 : ACTION : SET VALUES TO 2.4GHz AND 5GHz - PRIVATE SSID AND PASSWORD");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : THE ROUTER DEVICE SHOULD BE REACTIVATED SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			if (!isReactivated) {
				errorMessage = "FAILED TO REACTIVATE THE ROUTER DEVICE";
				status = false;
				try {
					BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
					status = true;
				} catch (TestException e) {
					errorMessage = e.getMessage();
				}
				if (status) {
					LOGGER.info("POST-CONDITION 1 : ACTUAL : THE ROUTER DEVICE REACTIVATED SUCCESSFULLY.");
				} else {
					LOGGER.error("POST-CONDITION 1 :  ACTUAL : " + errorMessage);
				}
			} else {
				LOGGER.info(
						"POST-CONDITION 1 : ACTUAL : SKIPPING REACTIVATION STEP AS THE DEVICE WAS ALREADY REACTIVATED SUCCESSFULLY IN PRE-CONDITION 1.");
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-5016");
	}

	/**
	 * Method to verify the connected client details in LMLog.txt.0 file
	 * 
	 * @param device         instance of {@link Dut}
	 * @param tapEnv         instance of {@link AutomaticsTapApi}
	 * @param connectionType Client Connection Type
	 * @param hostMacAddress Client Mac Address
	 * @param pollDuration   Duration to verify the logs
	 * @param deviceDateTime Device Date and Time
	 * @return true if expected log message available
	 * 
	 * @refactor Athira
	 * 
	 */

	public static boolean verifyConnectedClientDetailsInLMlog(Dut device, AutomaticsTapApi tapEnv,
			String connectionType, String hostMacAddress, long pollDuration, String deviceDateTime) {
		LOGGER.debug("STARTING METHOD : verifyConnectedClientDetailsInLMlog () ");
		String connectionTypeLog = null;
		boolean logStatus = false;
		String searchString = null;
		String alternateString = null;
		boolean isCnnTypeValid = true;
		if (connectionType
				.equalsIgnoreCase(BroadBandConnectedClientTestConstants.STRING_CLIENT_DEVICE_CONNECTION_TYPE_WIFI)) {
			LOGGER.info("OBTAINED CONNECTION TYPE IS : "
					+ BroadBandConnectedClientTestConstants.STRING_CLIENT_DEVICE_CONNECTION_TYPE_WIFI);
			connectionTypeLog = BroadBandConnectedClientTestConstants.LOG_PARAM_CLIENT_TYPE_WIFI;

			searchString = BroadBandTraceConstants.LOG_MESSAGE_TO_VERIFY_CONNECTED_CLIENT_ONLINE
					.replace(BroadBandTestConstants.STRING_REPLACE, hostMacAddress);

			alternateString = BroadBandTraceConstants.LOG_MESSAGE_TO_VERIFY_CONNECTED_CLIENT_CONNECTED
					.replace(BroadBandTestConstants.STRING_REPLACE, hostMacAddress);

		} else if (connectionType
				.equalsIgnoreCase(BroadBandConnectedClientTestConstants.CLIENT_DEVICE_CONNECTION_TYPE_ETHERNET)) {
			LOGGER.info("OBTAINED CONNECTION TYPE IS : "
					+ BroadBandConnectedClientTestConstants.CLIENT_DEVICE_CONNECTION_TYPE_ETHERNET);
			connectionTypeLog = BroadBandConnectedClientTestConstants.LOG_PARAM_CLIENT_TYPE_ETHERNET;
			searchString = BroadBandTraceConstants.LOG_MESSAGE_TO_VERIFY_ETHERNET_CONNECTED_CLIENT_ONLINE
					.replaceAll("##MACADDRESS##", hostMacAddress);
			alternateString = BroadBandTraceConstants.LOG_MESSAGE_TO_VERIFY_CONNECTED_ETHERNET_CLIENT_CONNECTED
					.replaceAll("##MACADDRESS##", hostMacAddress);
		} else {
			LOGGER.error("verifyConnectedClientDetailsInLMlog API IS ONLY APPLICABLE FOR "
					+ BroadBandConnectedClientTestConstants.STRING_CLIENT_DEVICE_CONNECTION_TYPE_WIFI + " AND "
					+ BroadBandConnectedClientTestConstants.CLIENT_DEVICE_CONNECTION_TYPE_ETHERNET + ""
					+ "CONNECTION TYPES BUT OBTAINED CONNECTION TYPE IS " + connectionType);
			isCnnTypeValid = false;
		}
		if (isCnnTypeValid) {
			String command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
					searchString, BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.LOG_FILE_LM,
					BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.CMD_TAIL_1);
			logStatus = verifyTheLogInformationWithPoolduration(device, tapEnv, connectionTypeLog, hostMacAddress,
					pollDuration, deviceDateTime, command);

			if (!logStatus) {
				command = command.replaceAll(searchString, alternateString);
				logStatus = verifyTheLogInformationWithPoolduration(device, tapEnv, connectionTypeLog, hostMacAddress,
						pollDuration, deviceDateTime, command);
			}
			LOGGER.info("SEARCH STATUS OF LOG '" + connectionTypeLog + "' IS : " + logStatus);
		} else {
			LOGGER.error(
					"SKIPPING LOG VALIDATION AS THE CONNECTION TYPE OBATAINED : " + connectionType + " IS INVALID");
		}
		LOGGER.debug("ENDING METHOD : verifyConnectedClientDetailsInLMlog () ");
		return logStatus;
	}

	/**
	 * Method to VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
	 * /rdklogs/logs/LM.txt.0 LOG
	 * 
	 * @param device              {@link Dut}
	 * @param testId              Test case ID
	 * @param deviceConnected
	 * @param deviceDateTime
	 * @param stepNumber
	 * @param expectedNoOfClients
	 * @param isEthernetClient
	 * 
	 * @refactor Athira
	 */

	public static void verifyConnectedclientDetails(Dut device, String testId, Dut deviceConnected,
			String deviceDateTime, int stepNumber, int expectedNoOfClients, boolean isEthernetClient)
			throws TestException {
		LOGGER.debug("STARTING METHOD : verifyConnectedclientDetails()");
		String testStep = null;
		boolean status = false;
		String errorMessage = null;
		try {
			String connectionType = ((Device) deviceConnected).getConnectedDeviceInfo().getConnectionType();
			String ethernetMacAddr = ((Device) deviceConnected).getConnectedDeviceInfo().getEthernetMacAddress();
			String wifiMacAddress = ((Device) deviceConnected).getConnectedDeviceInfo().getWifiMacAddress();

			LOGGER.info("connectionType RETRIEVED FROM CLIENT IS : " + connectionType);
			LOGGER.info("EthernetMacAddress RETRIEVED FROM CLIENT IS : " + ethernetMacAddr);
			LOGGER.info(" WifiMacAddress RETRIEVED FROM CLIENT IS : " + wifiMacAddress);

			String hostMacAddress = isEthernetClient
					? ((Device) deviceConnected).getConnectedDeviceInfo().getEthernetMacAddress()
					: ((Device) deviceConnected).getConnectedDeviceInfo().getWifiMacAddress();

			LOGGER.info("CONNECTION TYPE RETRIEVED FROM CLIENT IS : " + connectionType);
			LOGGER.info((isEthernetClient ? "ETHERNET " : "Wi-Fi") + " MACADDRESS RETRIEVED FROM CLIENT IS : "
					+ hostMacAddress);
			LOGGER.info("HOST MACADDRESS RETRIEVED FROM CLIENT IS : " + hostMacAddress);
			/**
			 * STEP : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH GIVEN
			 * GHZ .
			 */
			testStep = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION :VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN /rdklogs/logs/LM.txt.0 LOG");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND :grep -i 'RDKB_CONNECTED_CLIENTS: Client type is <CLIENT_TYPE>, MacAddress is <MAC_ADDRESS> and HostName is <HOST_NAME> appeared online' /rdklogs/logs/LM.txt.0 ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : MUST RETURN THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED FROM /rdklogs/logs/LM.txt.0 LOG");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE IN /rdklogs/logs/LM.txt.0 LOG";
			status = BroadBandConnectedClientUtils.verifyConnectedClientDetailsInLMlog(device, tapEnv, connectionType,
					hostMacAddress, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, deviceDateTime);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE CLIENT MAC ADDRESS AND CONNECTION TYPE IN /rdklogs/logs/LM.txt.0 LOG");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
			throw new TestException(errorMessage);
		}
		LOGGER.debug("ENDING METHOD : verifyConnectedclientDetails()");
	}

	/**
	 * Method to verify the connected client details in LMLog.txt.0 file
	 * 
	 * @param device            instance of {@link Dut}
	 * @param tapEnv            instance of {@link AutomaticsTapApi}
	 * @param connectionTypeLog Client Connection Type log message
	 * @param hostMacAddress    Client Mac Address
	 * @param pollDuration      Duration to verify the logs
	 * @param deviceDateTime    Device Date and Time
	 * @param command           command to execute
	 * @return true if expected log message available
	 * 
	 * @refactor Athira
	 * 
	 */
	public static boolean verifyTheLogInformationWithPoolduration(Dut device, AutomaticsTapApi tapEnv,
			String connectionTypeLog, String hostMacAddress, long pollDuration, String deviceDateTime, String command) {
		LOGGER.debug("ENDING METHOD : verifyTheLogInformationWithPoolduration () ");
		boolean logStatus = false;
		String response = null;
		long startTime = System.currentTimeMillis();
		do {
			response = tapEnv.executeCommandUsingSsh(device, command);
			if (BroadBandCommonUtils.verifyLogUsingTimeStamp(deviceDateTime, response)) {
				logStatus = CommonUtils.patternSearchFromTargetString(response, connectionTypeLog)
						&& (hostMacAddress.equalsIgnoreCase(CommonMethods
								.patternFinder(response, BroadBandConnectedClientTestConstants.LOG_PARAM_MAC_ADDRESS)
								.trim()));
			}
		} while ((System.currentTimeMillis() - startTime) < pollDuration && !logStatus
				&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
		LOGGER.debug("ENDING METHOD : verifyTheLogInformationWithPoolduration () ");
		return logStatus;
	}

	/**
	 * Method to verify the IPv4 and IPv6 connection interface & Internet
	 * connectivity for 2.4/5 GHz .
	 * 
	 * @param device          {@link Dut}
	 * @param testId          Test case ID
	 * @param deviceConnected Device Connected
	 * @param stepNumber      Step Number
	 */
	public static void verifyIpv4AndIpV6ConnectionInterface(Dut device, String testId, Dut deviceConnected,
			int stepNumber) throws TestException {
		LOGGER.debug("STARTING METHOD : verifyIpv4AndIpV6ConnectionInterface()");
		String step = null;
		boolean status = false;
		String errorMessage = null;
		BroadBandResultObject result = null;
		//
		boolean isSystemdPlatforms = false;
		isSystemdPlatforms = DeviceModeHandler.isFibreDevice(device);
		LOGGER.info("Gateway device model is:" + isSystemdPlatforms);
		//
		try {
			String osType = ((Device) deviceConnected).getOsType();
			String model = ((Device) deviceConnected).getModel();
			/**
			 * STEP : VERIFY THE CORRECT IPV4 ADDRESS FOR CONNECTED CLIENT
			 */
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION :VERIFY THE CORRECT IPV4 ADDRESS FOR CONNECTED CLIENT DEVICE MODEL " + model);
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv4 Address' or LINUX : ifconfig | grep 'inet' or ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : IT SHOULD RETURN THE CORRECT IPV4 ADDRESS FOR DEVICE MODEL " + model);
			LOGGER.info("***************************************************************************************");
			errorMessage = "UNABLE TO GET THE CORRECT IPV4 ADDRESS FROM CLIENT MODEL " + model;
			status = BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
					deviceConnected, tapEnv);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFYLLY VERIFIED CORRECT IPV4 ADDRESS FROM CLIENT DEVICE MODEL : " + model);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * STEP : VERIFY THE CORRECT IPV6 ADDRESS FOR CONNECTED CLIENT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION :VERIFY THE CORRECT IPV6 ADDRESS FOR CONNECTED CLIENT DEVICE MODEL " + model);
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv6 Address' or LINUX : ifconfig | grep 'inet6 ' ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : IT SHOULD RETURN THE CORRECT IPV4 ADDRESS FOR DEVICE MODEL " + model);
			LOGGER.info("***************************************************************************************");
			errorMessage = "UNABLE TO GET THE CORRECT IPV6 ADDRESS FROM CLIENT MODEL " + model;
			// IPV6 NA for Fibre Device
			if (!isSystemdPlatforms) {
				status = BroadBandConnectedClientUtils
						.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType, deviceConnected, tapEnv);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : SUCCESSFYLLY VERIFIED CORRECT IPV6 ADDRESS FROM CLIENT DEVICE MODEL : "
							+ model);
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
			} else {
				tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE,
						BroadBandTestConstants.FIBRE_NOT_APPLICABLE_IPV6, false);
			}
			LOGGER.info("***************************************************************************************");

			/**
			 * STEP : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH GIVEN GHZ SSID
			 * INTERFACE USING IPV4 .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT CONNECTED USING IPV4 FOR DEVICE MODEL : "
					+ model);
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -4 -n 5 google.com ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV4 ");
			LOGGER.info("***************************************************************************************");
			errorMessage = "NOT ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT WITH USING IPV4 FOR DEVICE MODEL : "
					+ model;
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					deviceConnected,
					BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
					BroadBandTestConstants.IP_VERSION4);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (!status) {
				errorMessage = "PIGN OPERATION FAILED TO ACCESS THE SITE 'www.google.com' USING IPV4 ";
				status = ConnectedNattedClientsUtils.verifyPingConnectionForIpv4AndIpv6(deviceConnected, tapEnv,
						BroadBandTestConstants.PING_TO_GOOGLE, BroadBandTestConstants.IP_VERSION4);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTED CLIENT HAS INTERNET CONNECTIVITY USING IPV4 FOR DEVICE MODEL : "
						+ model);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * STEP : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH GIVEN GHZ SSID
			 * INTERFACE USING IPV6 .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("***************************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT CONNECTED USING IPV6 FOR DEVICE MODEL : "
					+ model);
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -6 -v 'www.google.com' | grep '200 OK' OR ping -6 -n 5 google.com , LINUX : curl -6 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -6 -n 5 google.com ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV4 ");
			LOGGER.info("***************************************************************************************");
			// IPV6 NA for Fibre Device
			if (!isSystemdPlatforms) {
				errorMessage = "NOT ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT WITH USING IPV6 FOR DEVICE MODEL : "
						+ model;
				result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
						deviceConnected,
						BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
						BroadBandTestConstants.IP_VERSION6);
				status = result.isStatus();
				errorMessage = result.getErrorMessage();
				if (!status) {
					errorMessage = "PIGN OPERATION FAILED TO ACCESS THE SITE 'www.google.com' USING IPV6 ";
					status = ConnectedNattedClientsUtils.verifyPingConnectionForIpv4AndIpv6(deviceConnected, tapEnv,
							BroadBandTestConstants.PING_TO_GOOGLE, BroadBandTestConstants.IP_VERSION6);
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : CONNECTED CLIENT HAS INTERNET CONNECTIVITY USING IPV6 FOR DEVICE MODEL : "
							+ model);
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
			} else {
				tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE,
						BroadBandTestConstants.FIBRE_NOT_APPLICABLE_IPV6, false);
			}
			LOGGER.info("***************************************************************************************");

		} catch (TestException e) {
			LOGGER.error(errorMessage);
			throw new TestException(errorMessage);
		}
		LOGGER.debug("ENDING METHOD : verifyIpv4AndIpV6ConnectionInterface()");
	}

	/**
	 * Method to VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE
	 * 
	 * @param device          {@link Dut}
	 * @param testId          Test case ID
	 * @param deviceConnected Device Connected
	 * @param wifiBand
	 * @param stepNumber      Step Number
	 */
	public static void verifyClientMacInGateway(Dut device, String testId, Dut deviceConnected,
			WiFiFrequencyBand wifiBand, int stepNumber) throws TestException {
		String testStepNumber = "S" + stepNumber;
		boolean status = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : VERIFY " + wifiBand
				+ " CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY");
		LOGGER.info("STEP :  " + stepNumber + " : ACTION : EXECUTE COMMAND arp -n IN THE GATEWAY DEVICE");
		LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: MUST RETURN HAVE CONNECTED CLIENT MAC ADDRESS");
		LOGGER.info("#######################################################################################");
		String errorMessage = "THE GATEWAY DOESN'T HAVE THE CLIENT'S MAC ADDRESS WHILE EXECUTING COMMAND : '/sbin/arp -n | grep -i <MAC ADDRESS>'";
		try {
			// getConnectedClientIpOrMacFromTheDevice API GETS THE MAC ADDRESS FROM THE
			// CLIENT AND VERIFIES
			// IF THE GATEWAY HAS THE SAME MAC ADDRESS USING COMMAND : "/sbin/arp -n | grep
			// -i <MAC ADDRESS>"
			String macAddressRetrievedFromClient = BroadBandConnectedClientUtils
					.getConnectedClientIpOrMacFromTheDevice(device, deviceConnected, tapEnv, false);
			status = CommonMethods.isNotNull(macAddressRetrievedFromClient);
		} catch (TestException exp) {
			errorMessage = exp.getMessage();
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ " : ACTUAL : SUCCESSFULLY VERIFIED THE MAC ADDRESS OF THE CLIENT IN THE GATEWAY");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	}

	/**
	 * Method to VERIFY NUMBER OF CONNECTED CLIENTS ASSOCIATED WITH 2.4/5 GHz .
	 * 
	 * @param device              {@link Dut}
	 * @param testId              Test case ID
	 * @param deviceConnected
	 * @param wifiFrequencyBand
	 * @param webParameter
	 * @param stepNumber
	 * @param expectedNoOfClients
	 * 
	 * @refactor Athira
	 */
	public static void verifyCnnclientCountUsingWebpa(Dut device, String testId, Dut deviceConnected,
			String wifiFrequencyBand, String webParameter, int stepNumber, int expectedNoOfClients)
			throws TestException {

		String wifiBand = wifiFrequencyBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
				? BroadBandTestConstants.BAND_2_4GHZ
				: BroadBandTestConstants.BAND_5GHZ;
		/**
		 * STEP : VERIFY NUMBER OF CONNECTED CLIENTS ASSOCIATED WITH GIVEN GHZ .
		 */
		String testStep = "S" + stepNumber;
		boolean status = false;
		LOGGER.info("#####################################################################################");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION :VERIFY NUMBER OF CONNECTED CLIENTS ASSOCIATED WITH "
				+ wifiBand + " USING WEBPA");
		LOGGER.info("STEP " + stepNumber + ": ACTION : GET NUMBER OF CONNECTED CLIENTS ASSOCIATED WITH " + wifiBand
				+ " SSID USING WEBPA PARAM " + webParameter);
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : IT SHOULD RETURN THE ASSOCIATED DEVICE COUNT FOR " + wifiBand
				+ " SSID");
		LOGGER.info("#####################################################################################");
		String errorMessage = "UNABLE TO GET THE ASSOCIATED DEVICE COUNT FOR " + wifiBand + " SSID";
		status = BroadBandConnectedClientUtils.verifyAssociatedDeviceCount(device, tapEnv, webParameter,
				BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, expectedNoOfClients);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : ASSOCIATED DEVICE COUNT FOR " + wifiBand + " SSID IS :"
					+ expectedNoOfClients);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#####################################################################################");
		tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	}

	/**
	 * Test to verify single client harvester report for 5GHz client
	 * 
	 * <li>1. Update wifiClient mac address using webpa</li>
	 * <li>2. Update reporting period value as 5 using webpa</li>
	 * <li>3. Update override TTL value as 900 using webpa</li>
	 * <li>4. Create WiFi debug monitor log in nvram to capture the client
	 * report</li>
	 * <li>5. Update wifiClient enable status as true using webpa</li>
	 * <li>6. Get client report from wifiMon file</li>
	 * <li>7. Verify single client report log message in wifilog</li>
	 * <li>8. Verify cloud url to upload the client report in parodus log file</li>
	 * <li>9. Update wifiClient enable status as false using webpa</li>
	 * 
	 * @author ArunKumar Jayachandran
	 * 
	 * @Refactor Sruthi Santhosh
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			TestGroup.NEW_FEATURE, TestGroup.SECURITY })
	@TestDetails(testUID = "TC-RDKB-HARVESTER-REPORT-1003")
	public void testToVerifySingleClientHarvester(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-HARVESTER-REPORT-1003");
		LOGGER.info("TEST DESCRIPTION: Test to verify single client harvester report for 5GHz client");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update wifiClient mac address using webpa");
		LOGGER.info("2. Update reporting period value as 5 using webpa");
		LOGGER.info("3. Update override TTL value as 900 using webpa");
		LOGGER.info("4. Create WiFi debug monitor log in nvram to capture the client report");
		LOGGER.info("5. Update wifiClient enable status as true using webpa");
		LOGGER.info("6. Get client report from wifiMon file");
		LOGGER.info("7. Verify single client report log message in wifilog");
		LOGGER.info("8. Verify cloud url to upload the client report in parodus log file");
		LOGGER.info("9. Update wifiClient enable status as false using webpa");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-HARVESTER-REPORT-103";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store response
		String response = null;
		String macAddress = null;
		Dut clientDevice = null;
		boolean preStatus = false;
		// variable declaration ends

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("PRE-CONDITION : DESCRIPTION : Get the 5GHz WiFi client");
			LOGGER.info("PRE-CONDITION : ACTION : Connect client with 5GHz SSID & Passphrase");
			LOGGER.info("PRE-CONDITION : EXPECTED : Client should connect with 5GHz WiFi");
			errorMessage = "Failed to connect the client using 5GHz WiFi band";
			clientDevice = BroadBandConnectedClientUtils
					.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			preStatus = null != clientDevice;
			if (!preStatus) {
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR
						+ "PRE_CONDITION_FAILED: Unable to get 5GHz client device");
			}
			LOGGER.info("PRE-CONDITION : ACTUAL: Successfully connected to 5GHz wifi client device");
			LOGGER.info("PRE-CONFIGURATIONS : FINAL STATUS -  " + preStatus);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			stepNumber = "s1";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: Update wifiClient mac address using webpa");
			LOGGER.info(
					"STEP 1: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress data type: 0 value: <5GHz WiFi client mac>");
			LOGGER.info("STEP 1: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.MacAddress";
			macAddress = ((Device) clientDevice).getConnectedDeviceInfo().getWifiMacAddress();

			String formattedMacAddress = macAddress;
			if (CommonMethods.isNotNull(macAddress)) {
				macAddress = macAddress.replace(BroadBandTestConstants.DELIMITER_COLON,
						BroadBandTestConstants.EMPTY_STRING);
				status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_MAC_ADDRESS, BroadBandTestConstants.CONSTANT_0,
						macAddress);
			}
			if (status) {
				LOGGER.info("STEP 1: ACTUAL: Successfully updated client wifi mac using webpa");
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s2";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION: Update reporting period value as 5 using webpa");
			LOGGER.info(
					"STEP 2: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ReportingPeriod data type: 2 value: 5");
			LOGGER.info("STEP 2: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the reporting period value using webpa";
			status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_REPORTING_PERIOD, BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_5);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL: Successfully updated wifi client reporting period as 5 using webpa");
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s3";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION: Update override TTL value as 900 using webpa");
			LOGGER.info(
					"STEP 3: ACTION: Execute webpa set command: parameter: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL data type: 2 value: 900");
			LOGGER.info("STEP 3: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Default.OverrideTTL";
			status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_CLIENT_DEFAULT_OVERRIDE_TTL,
					BroadBandTestConstants.CONSTANT_2, Integer.toString(BroadBandTestConstants.CONSTANT_900));
			if (status) {
				LOGGER.info("STEP 3: ACTUAL: Successfully verified wifi client mac address length using webpa");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s4";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION: Create WiFi debug monitor log in nvram to capture the client report");
			LOGGER.info("STEP 4: ACTION: Execute command: touch /nvram/wifiMonDbg");
			LOGGER.info("STEP 4: EXPECTED: Should create a debug file under /nvram directory");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to create the file under /nvram directory";
			BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_TOUCH,
							BroadBandCommandConstants.FILE_WIFI_MON_DBG));
			status = (CommonMethods.isAtomSyncAvailable(device, tapEnv)) ? BroadBandCommonUtils
					.doesFileExistInAtomConsole(device, tapEnv, BroadBandCommandConstants.FILE_WIFI_MON_DBG).isStatus()
					: CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_WIFI_MON_DBG);
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL: Successfully created wifi monitoring debug log file under nvram directory");
			} else {
				LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s5";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION: Update wifiClient enable status as true using webpa");
			LOGGER.info(
					"STEP 5: ACTION: Execute webpa set command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled  datatype: boolean value: true");
			LOGGER.info("STEP 5: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled as true";
			status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 5: ACTUAL: Successfully enabled wifi client using webpa");
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s6";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION: Get client report from wifiMon file");
			LOGGER.info(
					"STEP 6: ACTION: Execute command: 1. grep -i \"wifi Destination\" /tmp/wifiMon 2. grep -i \"Polled station info\" /tmp/wifiMon");
			LOGGER.info("STEP 6: EXPECTED: Should get the report for configured wifi client");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the log message wifi destination in wifiMon file for configured client";
			response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_DESTINATION, BroadBandCommandConstants.FILE_WIFI_MON,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			// verify single client server url value in wifi destination output
			if (CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTestConstants.SINGLE_CLIENT_REPORT_SERVER_URL)) {
				errorMessage = "Failed to get the log message polled station info for configured client";
				response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
						BroadBandTraceConstants.LOG_MESSAGE_POLLED_STATION_INFO,
						BroadBandCommandConstants.FILE_WIFI_MON, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				// verify client wifi mac & vap value in polled station info output
				status = CommonMethods.isNotNull(response)
						&& CommonUtils.isGivenStringAvailableInCommandOutput(response,
								formattedMacAddress.toLowerCase())
						&& CommonUtils.isGivenStringAvailableInCommandOutput(response,
								BroadBandCommonUtils.concatStringUsingStringBuffer(
										BroadBandTestConstants.PRIVATE_VAP_COLON,
										BroadBandTestConstants.STRING_CONSTANT_2));
			}
			if (status) {
				LOGGER.info("STEP 6: ACTUAL: Successfully received the harvester logs for wifi client");
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s7";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION: Verify single client report log message in wifilog");
			LOGGER.info("STEP 7: ACTION: Execute command: grep -i single client report /rdklogs/logs/WiFilog.txt.0");
			LOGGER.info("STEP 7: EXPECTED: Response should contain the log message with transaction id");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the log message in WiFilog file";
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
					tapEnv, BroadBandTraceConstants.LOG_MESSAGE_SINGLE_CLIENT_REPORT,
					BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS));
			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL: Successfully verified the single client report log message in wifi log file");
			} else {
				LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s8";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION: Verify cloud url to upload the client report in parodus log file");
			LOGGER.info(
					"STEP 8: ACTION: Execute command: grep -i raw.kestrel.reports.WifiSingleClient /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 8: EXPECTED: Response should contain the cloud url in the PARODUSlog.txt.0 file");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the log message in PARODUSlog.txt.0 file";
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTestConstants.SINGLE_CLIENT_REPORT_SERVER_URL, BroadBandCommandConstants.LOG_FILE_PARODUS,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS));
			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL: Successfully verified single wifi client report cloud url in parodus log file");
			} else {
				LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s9";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION: Update wifiClient enable status as false using webpa");
			LOGGER.info(
					"STEP 9: ACTION: Execute webpa set command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled  datatype: boolean value: false");
			LOGGER.info("STEP 9: EXPECTED: Webpa set operation should be success");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the webpa parameter Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.Enabled as false";
			status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFICLIENT_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("STEP 9: ACTUAL: Successfully updated wifi client enabled status as false using webpa");
			} else {
				LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying single client harvester report" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {

			if (preStatus) {
				LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
				LOGGER.info("POST-CONDITION STEPS");
				/**
				 * POST CONDITION 1 : Remove wifi monitor debug file from /nvram and /tmp
				 * directory
				 */
				LOGGER.info("#######################################################################################");
				LOGGER.info(
						"POST-CONDITION 1: DESCRIPTION: Remove wifi monitor debug file from /nvram directory and reboot the device");
				LOGGER.info(
						"POST-CONDITION 1: ACTION: Execute command: 1. rm /nvram/wifiMonDbg 2. rm /tmp/wifiMon 3. reboot");
				LOGGER.info(
						"POST-CONDITION 1: EXPECTED: Should should remove wifi debug and monitor log file successfully");
				LOGGER.info("#######################################################################################");
				// remove wifiMonDbg file from /nvram directory
				BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_TO_REMOVE,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandCommandConstants.FILE_WIFI_MON_DBG));
				// remove wifiMon file from /tmp directory
				BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_TO_REMOVE,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandCommandConstants.FILE_WIFI_MON));
				// verify wifiMonDbg & wifiMon files are removed
				status = !((CommonMethods.isAtomSyncAvailable(device, tapEnv))
						? BroadBandCommonUtils
								.doesFileExistInAtomConsole(device, tapEnv, BroadBandCommandConstants.FILE_WIFI_MON_DBG)
								.isStatus()
						: CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_WIFI_MON_DBG)
								&& (CommonMethods.isAtomSyncAvailable(device, tapEnv))
										? BroadBandCommonUtils.doesFileExistInAtomConsole(device, tapEnv,
												BroadBandCommandConstants.FILE_WIFI_MON).isStatus()
										: CommonUtils.isFileExists(device, tapEnv,
												BroadBandCommandConstants.FILE_WIFI_MON));

				LOGGER.info("Status is:" + status);

				CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
				if (status) {
					LOGGER.info("POST-CONDITION 1: ACTUAL: Successfully removed wifi monitor and wifi debug files");
				} else {
					LOGGER.info("POST-CONDITION 1: ACTUAL: Failed to remove wifi monitor and wifi debug files");
				}

				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			}
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-HARVESTER-REPORT-1003");
		// ###############################################################//
	}

	/**
	 * Test to verify Removal of IDS
	 * <ol>
	 * <li>1. Check whether the IDS syscfg parameter is removed</li>
	 * <li>2. Check if IDS enable parameter available</li>
	 * <li>3. Check if IDS scan task parameter available</li>
	 * <li>4. Check if samhain log file available</li>
	 * <li>5.Check whether the file upload2splunk.sh is removed</li>
	 * <li>6. Check for any entry of IDS in dcmscript</li>
	 * </ol>
	 * 
	 * @author Deepa Bada
	 * @refactor Athira
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-IDS-1006")
	public void testToVerifyRemovalOfIDS(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-IDS-1006");
		LOGGER.info("TEST DESCRIPTION: Test to verify Removal of IDS");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Check whether the IDS syscfg parameter is removed");
		LOGGER.info("2. Check if IDS enable parameter available");
		LOGGER.info("3. Check if IDS scan task parameter available");
		LOGGER.info("4. Check if samhain log file available");
		LOGGER.info("5.Check whether the file upload2splunk.sh is removed");
		LOGGER.info("6. Check for any entry of IDS in dcmscript ");
		LOGGER.info("#######################################################################################");

		// Variable to store testcaseID
		String testCaseId = "TC-RDKB-IDS-006";
		// Variable to store step number
		String stepNumber = "s1";
		// variable to store status
		boolean status = false;
		// Variable to store errorMessage
		String errorMessage = null;
		// Variable to store response
		String response = null;

		try {

			// STEP 1: Check whether the IDS syscfg parameter is removed
			stepNumber = "s1";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 1: DESCRIPTION :  Check whether the IDS syscfg parameter is removed");
			LOGGER.info("STEP 1: ACTION : Execute Command:Execute command :cat /opt/secure/data/syscfg.db | grep IDS");
			LOGGER.info("STEP 1: EXPECTED : Should return empty");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to verify IDS syscfg parameter is removed";
			response = tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_TO_GREP_IDS_IN_SYSCFG_DB);
			LOGGER.info("response :" + response);
			if (response.contains(BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY)) {
				response = tapEnv.executeCommandInSettopBox(device,
						BroadbandPropertyFileHandler.getCommandForIDSinSyscfg());
			}
			LOGGER.info("response is " + response);
			status = CommonMethods.isNull(response);

			// Factory reset is required if the test case is running for the first time in
			// the device to upate with
			// latest reset changes
			if (!status) {
				if (BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device)) {
					response = tapEnv.executeCommandInSettopBox(device,
							BroadBandCommandConstants.CMD_TO_GREP_IDS_IN_SYSCFG_DB);
					if (response.contains(BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY)) {
						response = tapEnv.executeCommandInSettopBox(device,
								BroadbandPropertyFileHandler.getCommandForIDSinSyscfg());
					}
					LOGGER.info("response is " + response);
					status = CommonMethods.isNull(response);

				} else
					LOGGER.error("failed to perform factory reset");
			}

			if (status) {
				LOGGER.info("STEP 1: ACTUAL: Successfully verified IDS syscfg parameter is removed");
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 2: Check if IDS enable parameter available
			stepNumber = "s2";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 2: DESCRIPTION : Check if IDS enable parameter available ");
			LOGGER.info(
					"STEP 2: ACTION : Execute command :dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.IDS.Enable");
			LOGGER.info("STEP 2: EXPECTED : IDS enable parameter is not available");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to check abscence of IDS enable parameter";
			response = tapEnv.executeCommandInSettopBox(device,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_DMCLI_GET_VALUE,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
							BroadBandWebPaConstants.DEVICE_DEVICEINFO_RFC_FEATURE_IDS_ENABLE));
			LOGGER.info("response is " + response);

			status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTestConstants.PATTERN_DMCLIPARAMETER_NOT_FOUND);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL: Successfully verified absence of IDS enable parameter");
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 3: Check if IDS scan task parameter available
			stepNumber = "s3";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 3: DESCRIPTION : Check if IDS scan task  parameter available ");
			LOGGER.info(
					"STEP 3: ACTION : Execute command :dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.IDS.ScanTask");
			LOGGER.info("STEP 3: EXPECTED : IDS scan task parameter is  not available");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to check abscence of IDS scan task parameter";
			response = tapEnv.executeCommandInSettopBox(device,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_DMCLI_GET_VALUE,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
							BroadBandWebPaConstants.WEBPA_PARAM_IDS_SCAN_TASK));
			LOGGER.info("response is " + response);

			status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTestConstants.PATTERN_DMCLIPARAMETER_NOT_FOUND);

			if (status) {
				LOGGER.info("STEP 3: ACTUAL: Successfully verified absence of IDS scn task parameter");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 4: Check if samhain log file available
			stepNumber = "s4";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 4: DESCRIPTION : Check if samhain log file available");
			LOGGER.info("STEP 4: ACTION : Execute command: ls -l /rdklogs/logs/samhain*");
			LOGGER.info("STEP 4: EXPECTED : samhain log file is not available");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to check if samhain log file available";
			response = tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_TO_FIND_SAMHAIN);
			LOGGER.info("response is " + response);

			status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
					AutomaticsConstants.NO_SUCH_FILE_OR_DIRECTORY);

			if (status) {
				LOGGER.info("STEP 4: ACTUAL: Successfully verified absence of samhain log file ");
			} else {
				LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 5: Check whether the file upload2splunk.sh is removed
			stepNumber = "s5";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 5: DESCRIPTION : Check whether the file upload2splunk.sh is removed ");
			LOGGER.info("STEP 5: ACTION : Execute command: find / -name *upload2splunk* 2>/dev/null");
			LOGGER.info("STEP 5: EXPECTED : upload2splunk.sh file is not present ");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to check if upload2splunk.sh is removed";
			response = tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_TO_FIND_UPLOAD2SPLUNK);
			LOGGER.info("response is " + response);

			status = CommonMethods.isNull(response);

			if (status) {
				LOGGER.info("STEP 5: ACTUAL: Successfully verified absence ofupload2splunk.sh file ");
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			// STEP 6: Check for any entry of IDS in dcmscript
			stepNumber = "s6";
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 6: DESCRIPTION : Check for any entry of IDS in dcmscript ");
			LOGGER.info("STEP 6: ACTION : Execute command: cat /rdklogs/logs/dcmscript.log | grep IDS");
			LOGGER.info("STEP 6: EXPECTED : Successfully verified IDS entry in dcmscript");
			LOGGER.info("#######################################################################################");
			errorMessage = "Failed to check for any entry of IDS in dcmsript";
			response = tapEnv.executeCommandInSettopBox(device, BroadBandCommandConstants.CMD_TO_GREP_IDS_DCMSCRIPT);

			status = CommonMethods.isNull(response);

			if (status) {
				LOGGER.info("STEP 6: ACTUAL: Successfully verified entry of IDS in dcmscript ");
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while verifying IDS removal:" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-IDS-1006");

	}

	/**
	 * This test case is to Verify the DHCP Lan client
	 * 
	 * 
	 * <ol>
	 * <li>STEP 1: Verify the client connected to ethernet has IP Address assigned
	 * from DHCP.</li>
	 * 
	 * <li>STEP 2: Verify whether interface got the correct IPv6 address.</li>
	 * 
	 * <li>STEP 3 :Verify whether you have connectivity using that particular
	 * interface using IPV4</li>
	 * 
	 * <li>STEP 4 :Verify whether you have connectivity using that particular
	 * interface using IPV6</li>
	 * 
	 * </ol>
	 * 
	 * @param device Dut to be used
	 * @author Joseph_Maduram
	 * @refactor Said Hisham
	 */

	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-DHCP-LANCLIENT-1001")
	public void testToVerifyDhcpLanClient(Dut device) {
		// String to store the test case status
		boolean status = false;
		// Test case id
		String testId = "TC-RDKB-DHCP-LANCLIENT-101";
		// Test step number
		String testStepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		Dut connectedClientSettop = null;
		BroadBandResultObject result = null; // stores test result and error
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-DHCP-LANCLIENT-1001");
			LOGGER.info("TEST DESCRIPTION:Verify whether the connected client to the ethernet got the IP address");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1: Verify the client connected to ethernet has IP Address assigned from
			 * DHCP
			 *
			 */
			testStepNumber = "s1";
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Verify the client connected to ethernet has IP Address assigned from DHCP");
			LOGGER.info(
					"STEP 1: ACTION : Client connected to ethernet from gateway should receive valid IP Address in DHCP range");
			LOGGER.info("STEP 1: EXPECTED: Client connected to LAN should be assigned with IP address from gateway");
			LOGGER.info("#####################################################################################");
			connectedClientSettop = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			errorMessage = "Unable to connect to Ethernet client";
			if (null != connectedClientSettop) {
				status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv,
						device, connectedClientSettop);
				errorMessage = "Client connected to ethernet haven't receieve valid IP Address from Gateway";
			}
			if (status) {
				LOGGER.info(
						"S1 ACTUAL: Client connected to ethernet from gateway has received valid IP Address in DHCP range");
			} else {
				LOGGER.error("S1 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2:Verify whether interface got the correct IPv6 address.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 2: DESCRIPTION : Verify whether interface  got the correct IPv6  address.");
			LOGGER.info("STEP 2: ACTION : Connected client should get the IPV6 Interface");
			LOGGER.info("STEP 2: EXPECTED:Interface IPv6 address should  be shown");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s2";
			status = false;
			errorMessage = "interface  didnt got the correct IPV6 address";
			result = BroadBandConnectedClientUtils
					.verifyIpv6AddressOfEthInterfaceConnectedWithBroadbandDevice(connectedClientSettop, tapEnv);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("S2 ACTUAL :Interface  got the correct IPv6 address");
			} else {
				LOGGER.error("S2 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 3:Verify whether you have connectivity using that particular interface
			 * using IPV4.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 3: Verify whether there is  connectivity using that particular interface using IPV4 ");
			LOGGER.info("STEP 3: ACTION : connectivity for Ipv4 interface should be successful");
			LOGGER.info("STEP 3: EXPECTED: Connectivity check should return status as 200");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s3";
			status = false;
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					connectedClientSettop, BroadBandTestConstants.URL_HTTPS_FACEBOOK,
					BroadBandTestConstants.IP_VERSION4);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("S3 ACTUAL: connectivity successful using ipv4 interface");
			} else {
				LOGGER.error("S3 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 4:Verify whether there is connectivity using that particular interface
			 * using IPV6.
			 * 
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP 4: Verify whether you have connectivity using that particular interface using IPV6");
			LOGGER.info("STEP 4: ACTION : connectivity for Ipv6 interface should be successful");
			LOGGER.info("STEP 4:EXPECTED: Connectivity check should return status as 200");
			LOGGER.info("#####################################################################################");
			status = false;
			testStepNumber = "s4";
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					connectedClientSettop, BroadBandTestConstants.URL_HTTPS_FACEBOOK,
					BroadBandTestConstants.IP_VERSION6);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("S4 ACTUAL: connectivity successful using ipv6 interface");
			} else {
				LOGGER.error("S4 ACTUAL: " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} catch (Exception exception) {
			LOGGER.error("Exception occured during execution !!!!" + exception.getMessage());
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, false, errorMessage, true);
		}
		LOGGER.info("ENDING TESTCASE :TC-RDKB-DHCP-LANCLIENT-1001");
	}

	/**
	 *
	 * Test Case : Verify Private WiFi channel selection (ACS) should remain same
	 * post MESH enabled or disabled
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * 
	 * <li>PRE CONDITION 1 : Perform Factory reset and reactivate device if any of
	 * the ACS initial status is disabled</li>
	 * <li>PRE CONDITION 2 : Verify 2.4 GHz SSID is enabled.</li>
	 * <li>PRE CONDITION 3 : Verify 5 GHz SSID is enabled.</li>
	 * <li>PRE CONDITION 4 : Verify the private wifi 2.4 ghz and 5 ghz ssid's are
	 * broadcasting in connected client.</li>
	 * <li>Step 1 : Verify mesh is enabled using TR181 parameter,if not enable wifi
	 * mesh service using tr181 parameter</li>
	 * <li>Step 2 : Verify the Automatic channel selection mode for wifi 2.5 ghz as
	 * true.</li>
	 * <li>Step 3 : Verify the Automatic channel selection mode for wifi 5 ghz as
	 * true.</li>
	 * <li>Step 4 : Verify ACS status using Wi-Fi driver level commands</li>
	 * <li>Step 5 : Disable Mesh using TR181 parameter</li>
	 * <li>Step 6 : Verify the Automatic channel selection mode for wifi 2.5 ghz as
	 * true even when mesh is disabled</li>
	 * <li>Step 7 : Verify the Automatic channel selection mode for wifi 5 ghz as
	 * true even when mesh is disabled</li>
	 * <li>Step 8 : Verify ACS status using Wi-Fi driver level commands</li>
	 * <li>Step 9 : Verify mesh disable persist</li>
	 * <li>Step 10 : Verify mesh is enabled using TR181 parameter,if not enable wifi
	 * mesh service using tr181 parameter</li>
	 * <li>Step 11 :Verify the Automatic channel selection mode for wifi 2.5 ghz as
	 * true</li>
	 * <li>Step 12 : Verify the Automatic channel selection mode for wifi 5 ghz as
	 * true</li>
	 * <li>Step 13 : Verify ACS status using Wi-Fi driver level commands</li>
	 * <li>Step 14 : Connect the client to 2.4 GHz Private Wi-Fi Network and verify
	 * connection status</li> LOGGER.info(
	 * <li>Step 15 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 16 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 17 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 18 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 Ghz</li>
	 * <li>Step 19 : Connect the connected client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 20 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 21 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 22 :Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 5 Ghz.</li>
	 * <li>Step 23 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 Ghz.</li>
	 * <li>POST-CONDITION 1 : Verify disconnecting the client connected with in the
	 * setup.</li>
	 * <li>POST-CONDITION 2 : Revert the default wifi mesh status and automatic
	 * channel mode.</li>
	 * 
	 * 
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Deepika Sekar
	 * @refactor Said Hisham
	 *
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI, BroadBandTestGroup.WEBPA })
	@TestDetails(testUID = "TC-RDKB-ACS-PERSIST-MESH-1001")
	public void testToVerifyAcsPersistPostMeshChange(Dut device) {
		String testCaseId = "TC-RDKB-ACS-PERSIST-MESH-101";
		String errorMessage = null;
		boolean status = false;
		int stepNumber = 1;
		int postConStepNumber = 1;
		int preConStepNumber = 0;
		String acsStep = "S" + stepNumber;
		List<Dut> ssidVisibleDevices = new ArrayList<Dut>();
		Dut ssidVisibleDeviceOne = null;
		String meshInitialStatus = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
		boolean acs24InitialStatus = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ, BroadBandTestConstants.TRUE,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
		boolean acs5InitialStatus = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ, BroadBandTestConstants.TRUE,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
		boolean acs24InitialStatusChanged = false;
		boolean acs5InitialStatusChanged = false;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-ACS-PERSIST-MESH-1001");
			LOGGER.info(
					"TEST DESCRIPTION: Verify Private WiFi channel selection (ACS) should remain same post MESH enabled or disabled");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					" PRE CONDITION 1 : Perform Factory reset and reactivate device if any of the ACS initial status is disabled");
			LOGGER.info(" PRE CONDITION 2 : Verify 2.4 GHz SSID is enabled.");
			LOGGER.info(" PRE CONDITION 3 : Verify 5 GHz SSID is enabled.");
			LOGGER.info(
					" PRE CONDITION 4 : Verify the private wifi 2.4 ghz and 5 ghz ssid's are broadcasting in connected client.");
			LOGGER.info(
					"Step 1 : Verify mesh is enabled using TR181 parameter,if not enable wifi mesh service via XPC");
			LOGGER.info("Step 2 : Verify the Automatic channel selection mode for wifi 2.5 ghz as true.");
			LOGGER.info("Step 3 : Verify the Automatic channel selection mode for wifi 5 ghz as true.");
			LOGGER.info("Step 4 : Verify ACS status using Wi-Fi driver level commands ");
			LOGGER.info("Step 5 : Disable Mesh using XPC");
			LOGGER.info(
					"Step 6 : Verify the Automatic channel selection mode for wifi 2.5 ghz as true even when mesh is disabled");
			LOGGER.info(
					"Step 7 : Verify the Automatic channel selection mode for wifi 5 ghz as true even when mesh is disabled");
			LOGGER.info("Step 8 : Verify ACS status using Wi-Fi driver level commands ");
			LOGGER.info("Step 9 : Verify mesh disable persist");
			LOGGER.info(
					"Step 10 : Verify mesh is enabled using TR181 parameter,if not enable wifi mesh service via XPC");
			LOGGER.info("Step 11 : Verify the Automatic channel selection mode for wifi 2.5 ghz as true");

			LOGGER.info("Step 12 : Verify the Automatic channel selection mode for wifi 5 ghz as true");
			LOGGER.info("Step 13 : Verify ACS status using Wi-Fi driver level commands ");
			LOGGER.info("Step 14 : Connect the client to 2.4 GHz Private Wi-Fi Network and verify connection status ");
			LOGGER.info("Step 15 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID");
			LOGGER.info("Step 16 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID");
			LOGGER.info(
					"Step 17 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					"Step 18 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(
					"Step 19 : Connect  the connected client  in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 20 : Verify  the correct IPv4  address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 21 : Verify  the correct IPv6  address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 22 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 Ghz.");
			LOGGER.info(
					"Step 23 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 Ghz.");
			LOGGER.info(" POST-CONDITION 1 : Verify disconnecting the client connected with in the setup.");
			LOGGER.info(" POST-CONDITION 2 : Revert the default wifi mesh status and automatic channel mode.");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			if (!acs24InitialStatus || !acs5InitialStatus) {

				preConStepNumber++;
				BroadBandPreConditionUtils.executePreConditionToFactoryResetAndReacitivateDevice(device, tapEnv,
						preConStepNumber, true);
				if (!acs24InitialStatus) {
					acs24InitialStatusChanged = true;
				} else {
					acs5InitialStatusChanged = true;
				}
			}
			/**
			 * PRE-CONDITION 2-3 : VERIFY THE PRIVATE WIFI 2.4 GHZ AND 5 GHZ SSID'S ARE
			 * ENABLED
			 */
			preConStepNumber++;
			BroadBandPreConditionUtils.executePreConditionToVerifyRadioStatus(device, tapEnv, preConStepNumber);
			/**
			 * PRE-CONDITION 4 : VERIFY THE PRIVATE WIFI 2.4 GHZ AND 5 GHZ SSID'S ARE
			 * BROADCASTING IN CONNECTED CLIENT
			 */
			preConStepNumber++;
			ssidVisibleDevices = BroadBandPreConditionUtils.executePreConditionToGetBothPrivateWiFiSsidsVisibleDevices(
					device, tapEnv, preConStepNumber, BroadBandTestConstants.CONSTANT_1);
			ssidVisibleDeviceOne = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_0);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1-4 : Check ACS status using webpa and wifi driver commands after
			 * enabling mesh.
			 */
			checkACSBasedOnMeshStatus(device, tapEnv, BroadBandTestConstants.TRUE, testCaseId,
					BroadBandTestConstants.CONSTANT_0);
			/**
			 * Step 5-8 : Check ACS status using webpa and wifi driver commands after
			 * disabling mesh.
			 */
			checkACSBasedOnMeshStatus(device, tapEnv, BroadBandTestConstants.FALSE, testCaseId,
					BroadBandTestConstants.CONSTANT_4);
			/**
			 * Step 9 : VERIFY MESH DISABLE PERSIST
			 */
			stepNumber = BroadBandTestConstants.CONSTANT_9;
			acsStep = "S" + stepNumber;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY MESH DISABLE PERSIST");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND : Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.Mesh.Enable");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE WIFI MESH SERVICE SHOULD BE STILL IN DISABLED STATE");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO ENABLE WIFI MESH SERVICE USING WEBPA";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL: THE WIFI MESH SERVICE IS STILL IN DISABLED STATE");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, acsStep, status, errorMessage, true);
			/**
			 * Step 10-13 : Check ACS status using webpa and wifi driver commands after
			 * enabling mesh.
			 */
			checkACSBasedOnMeshStatus(device, tapEnv, BroadBandTestConstants.TRUE, testCaseId,
					BroadBandTestConstants.CONSTANT_9);

			/**
			 * Step 14 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHZ SSID AND
			 * VERIFY CONNECTION STATUS AFTER FIRMWARE UPGRADE
			 */
			stepNumber = BroadBandTestConstants.CONSTANT_14;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * SETP 15- 18 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * Step 19 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHZ SSID AND VERIFY
			 * CONNECTION STATUS AFTER FIRMWARE UPGRADE
			 */
			stepNumber = 19;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 20- 23 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING WIFI CLIENT CONNECTIVITY AFTER FIRMWARE UPGRADE : "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, acsStep, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			BroadBandPostConditionUtils.executePostConditionToDisconnectConnectedClient(device, tapEnv,
					ssidVisibleDeviceOne, postConStepNumber);
			/**
			 * POST CONDITION : REVERT THE DEFAULT WIFI MESH STATUS AND AUTOMATIC CHANNEL
			 * MODE.
			 */
			postConStepNumber++;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : DESCRIPTION : REVERT THE DEFAULT WIFI MESH STATUS AND AUTOMATIC CHANNEL MODE.");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : SET THE DEFAULT WIFI MESH STATUS AND AUTOMATIC CHANNEL MODE USING WEBPA");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : EXPECTED : MUST SET THE DEFAULT WIFI MESH STATUS AND AUTOMATIC CHANNEL MODE ");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO SET DEFAULT WIFI MESH STATUS AND AUTOMATIC CHANNEL MODE";
			List<WebPaParameter> webPaParameters = new ArrayList<WebPaParameter>();
			if (acs24InitialStatusChanged) {
				WebPaParameter defaultAutoChannelMode2ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ,
						BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
				webPaParameters.add(defaultAutoChannelMode2ghz);
			}
			if (acs5InitialStatusChanged) {
				WebPaParameter defaultAutoChannelMode5ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ,
						BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
				webPaParameters.add(defaultAutoChannelMode5ghz);
			}
			if (!meshInitialStatus.equals(BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE))) {
				WebPaParameter meshStatusDisabled = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE, meshInitialStatus,
						WebPaDataTypes.BOOLEAN.getValue());
				webPaParameters.add(meshStatusDisabled);
			}
			status = BroadBandWebPaUtils.setVerifyMultipleWebPAInPolledDuration(device, tapEnv, webPaParameters,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTUAL : SUCCESSFULLY SET DEFAULT WIFI MESH STATUS AND AUTOMATIC CHANNEL MODE");
			} else {
				LOGGER.info("POST-CONDITION " + postConStepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-ACS-PERSIST-MESH-1001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Common steps for checking ACS status by enabling/disabling mesh
	 * 
	 * @param device
	 * @param meshStatus
	 * @param testCaseId
	 * @param stepNumbers
	 * @refactor Said Hisham
	 */
	public static void checkACSBasedOnMeshStatus(Dut device, AutomaticsTapApi tapEnv, String meshStatus,
			String testCaseId, int stepNumber) {
		// String to store the test case status
		boolean status = false;
		// String to store the error message
		String errorMessage = null;
		String acsStep = "S" + stepNumber;
		boolean wifiDriverCommands = BroadbandPropertyFileHandler.isWifiDriverCommandsApplicableForDevices(device);

		/**
		 * STEP : ENABLE/DISABLE THE WIFI MESH SERVICE USING TR181 PARAMETER
		 */

		stepNumber++;
		acsStep = "S" + stepNumber;
		status = false;

		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"STEP " + stepNumber + ": DESCRIPTION : ENABLE/DISABLE THE WIFI MESH SERVICE USING TR181 PARAMETER");
		LOGGER.info("STEP " + stepNumber + ": ACTION : EXECUTE COMMAND : Enable/Disable mesh");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE WIFI MESH SERVICE SHOULD BE SET AS " + meshStatus
				+ "  SUCCESSFULLY");
		LOGGER.info("#######################################################################################");
		errorMessage = "FAILED TO ENABLE WIFI MESH SERVICE USING WEBPA";
		status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE, meshStatus,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (!status) {
			status = BroadBandMeshUtils.enableOrDisableMesh(device, tapEnv, Boolean.parseBoolean(meshStatus));

		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL: THE WIFI MESH SERVICE IS SHOULD BE SET AS " + meshStatus
					+ "  SUCCESSFULLY");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testCaseId, acsStep, status, errorMessage, true);

		/**
		 * STEP : VERIFY THE AUTOMATIC CHANNEL SELECTION MODE FOR WIFI 2.5 GHZ AS TRUE.
		 */
		stepNumber++;
		acsStep = "S" + stepNumber;
		status = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : VERIFY THE AUTOMATIC CHANNEL SELECTION MODE FOR WIFI 2.5 GHZ AS TRUE.");
		LOGGER.info(
				"STEP " + stepNumber + ": ACTION : EXECUTE WEBPA COMMAND : Device.WiFi.Radio.10000.AutoChannelEnable");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : AUTOMATIC CHANNEL SELECTION MODE FOR 2GHZ SHOULD BE ENABLED");
		LOGGER.info("#######################################################################################");
		errorMessage = "FAILED TO ENABLE THE AUTOMATIC CHANNEL SELECTION MODE FOR 2GHZ";
		status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ, BroadBandTestConstants.TRUE,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ ": ACTUAL: AUTOMATIC CHANNEL SELECTION MODE FOR 2GHZ IS ENABLED SUCCESSFULLY.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testCaseId, acsStep, status, errorMessage, true);

		/**
		 * STEP : VERIFY THE AUTOMATIC CHANNEL SELECTION MODE FOR WIFI 5 GHZ AS TRUE.
		 */
		stepNumber++;
		acsStep = "S" + stepNumber;
		status = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : SET AND VERIFY THE AUTOMATIC CHANNEL SELECTION MODE FOR WIFI 5 GHZ AS TRUE.");
		LOGGER.info(
				"STEP " + stepNumber + ": ACTION : EXECUTE WEBPA COMMAND : Device.WiFi.Radio.10100.AutoChannelEnable");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : AUTOMATIC CHANNEL SELECTION MODE FOR 5 GHZ SHOULD BE ENABLED");
		LOGGER.info("#######################################################################################");
		errorMessage = "FAILED TO ENABLE THE AUTOMATIC CHANNEL SELECTION MODE FOR 5GHZ";
		status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ, BroadBandTestConstants.TRUE,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ ": ACTUAL: AUTOMATIC CHANNEL SELECTION MODE FOR 5 GHZ IS ENABLED SUCCESSFULLY.");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testCaseId, acsStep, status, errorMessage, true);

		/**
		 * STEP : SET AND VERIFY THE AUTOMATIC CHANNEL SELECTION MODE FOR WIFI 5 GHZ AS
		 * TRUE.
		 */
		stepNumber++;
		acsStep = "S" + stepNumber;
		status = false;
		if (wifiDriverCommands) {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE AUTOMATIC CHANNEL SELECTION MODE USING WIFI DRIVER LEVEL COMMANDS FOR 2.4 and 5 GHz");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE For Specific device trigger commands for 2.4 and 5 GHz\r\n"
					+ "qcsapi_sockraw host0 00:11:22:AB:CD:EE get_auto_channel wifi0_0 \r\n"
					+ "qcsapi_sockraw host0 00:11:22:AB:CD:EE get_auto_channel wifi2_0 \r\n"
					+ "For specific device trigger commands for 2.4 and 5 GHz\r\n"
					+ "qcsapi_pcie get_auto_channel wifi0_0 \r\n" + "qcsapi_pcie get_auto_channel wifi2_0 \r\n"
					+ "For Atom devices trigger commands for 2.4 and 5 GHz in atom console\r\n"
					+ "cfg -s | grep AP_PRIMARY_CH");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : AUTOMATIC CHANNEL SELECTION MODE FOR 2.4 and 5 GHZ SHOULD BE ENABLED");
			LOGGER.info("#######################################################################################");
			errorMessage = "ACS status is not enabled for 2.4Ghz";

			status = BroadBandCommonUtils.retrieveACSStatusUsingSystemCommands(device, tapEnv,
					BroadBandTestConstants.BAND_2_4GHZ);

			if (status && !CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				errorMessage = "ACS status is not enabled for 5Ghz";
				status = false;
				status = BroadBandCommonUtils.retrieveACSStatusUsingSystemCommands(device, tapEnv,
						BroadBandTestConstants.BAND_5GHZ);
			}
			if (status)

			{
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: AUTOMATIC CHANNEL SELECTION MODE FOR 2.4 and 5 GHZ IS VERIFIED IN SYSTEM COMMANDS.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, acsStep, status, errorMessage, true);
		} else {
			LOGGER.info("STEP " + stepNumber + " IS NOT APPLICABLE FOR some specific models");
			errorMessage = "Wifi driver commands is NA for some specific models";
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, acsStep, ExecutionStatus.NOT_APPLICABLE,
					errorMessage, false);
		}
	}

	/**
	 *
	 * Test Case : Verify the RDKB Telemetry logs in Gateway connected with WiFi
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <li>PRE-CONDITION 1 : Perform factory reset on the device</li>
	 * <li>PRE-CONDITION 2 : Reactivate the router device</li>
	 * <li>PRE-CONDITION 3 : Verify the private wifi 2.4 GHz and 5 GHz SSID's are
	 * broadcasting in connected client</li>
	 * <li>PRE-CONDITION 4 : Set and verify the wifi telemetry log interval as 5
	 * minutes</li>
	 * <li>PRE-CONDITION 5 : Verify the wifi helath log file</li>
	 * <li>Step 1 : Connect the client into 2.4 GHz private ssid and verify
	 * connection status</li>
	 * <li>Step 2 : Connect another client into 5 GHz private ssid and verify
	 * connection status</li>
	 * <li>Step 3 : Verify the correct IPv4 address for client connected with
	 * private wifi 2.4 GHz ssid</li>
	 * <li>Step 4 : Verify the correct IPv6 address for client connected with
	 * private wifi 2.4 GHz ssid</li>
	 * <li>Step 5 : Verify the internet connectivity in the client connected with
	 * private wifi 2.4 GHz ssid using ipv4 interface</li>
	 * <li>Step 6 : Verify the internet connectivity in the client connected with
	 * private wifi 2.4 GHz ssid using ipv6 interface</li>
	 * <li>Step 7 : Verify the correct IPv4 address for client connected with
	 * private wifi 5 GHz ssid</li>
	 * <li>Step 8 : Verify the correct IPv6 address for client connected with
	 * private wifi 5 GHz ssid</li>
	 * <li>Step 9 : Verify the internet connectivity in the client connected with
	 * private wifi 5 GHz ssid using ipv4 interface</li>
	 * <li>Step 10 : Verify the internet connectivity in the client connected with
	 * private wifi 5 GHz ssid using ipv6 interface</li>
	 * <li>Step 11 : Verify log information \"WIFI_BANDUTILIZATION_1\" is available
	 * in wifihealth.txt log</li>
	 * <li>Step 12 : Verify log information \"WIFI_BANDUTILIZATION_2\" is available
	 * in wifihealth.txt log</li>
	 * <li>Step 13 : Verify log information \"WIFI_MAC_1\" is available and
	 * validating consumable format for Client Mac in wifihealth.txt log</li>
	 * <li>Step 14 : Verify log information \"WIFI_MAC_2\" is available and
	 * validating consumable format for Client Mac in wifihealth.txt wifihealth.txt
	 * log</li>
	 * <li>Step 15 : Verify log information \"WIFI_MAC_1_TOTAL_COUNT\" is available
	 * in wifihealth.txt log</li>
	 * <li>Step 16 : Verify log information \"WIFI_MAC_2_TOTAL_COUNT\" is available
	 * in wifihealth.txt log</li>
	 * <li>Step 17 : Verify log information \"WIFI_RXCLIENTS_1\" is available in
	 * wifihealth.txt log</li>
	 * <li>Step 18 : Verify log information \"WIFI_RXCLIENTS_2\" is available in
	 * wifihealth.txt log</li>
	 * <li>Step 19 : Verify log information \"WIFI_TXCLIENTS_1\" is available in
	 * wifihealth.txt log</li>
	 * <li>Step 20 : Verify log information \"WIFI_TXCLIENTS_2\" is available in
	 * wifihealth.txt log</li>
	 * <li>Step 21 : Verify log information \"Wifi_Broadcast_Complete\" is available
	 * in wifihealth.txt log</li>
	 * <li>Step 22 : Verify log information \"Wifi_name_broadcasted\" is available
	 * in wifihealth.txt log</li>
	 * <li>Step 23 : Verify log information \"Rdkb_connected_clients\" is available
	 * in wifihealth.txt log</li>
	 * <li>Step 24 : Verify disconnecting the client from 2.4 GHz private wifi
	 * SSID</li>
	 * <li>Step 25 : Verify disconnecting the client from 5 GHz private wifi
	 * SSID</li>
	 * <li>Step 26 : Verify log information \"RDKB_SYS_MEM_INFO_SYS\" is available
	 * in SelfHeal.txt.0</li>
	 * <li>Step 27 : Verify log information \"USED_MEM\" is available in
	 * SelfHeal.txt.0</li>
	 * <li>Step 28 : Verify log information \"FREE_MEM\" is available in
	 * SelfHeal.txt.0</li>
	 * <li>POST-CONDITION 1 : Reactivate the Device</li>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar,Rajapandian
	 *
	 * @Refactor Sruthi Santhosh
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-WIFI-WIFI-RDKB-TELE-5001")
	public void testToverifyWiFiTelemetryLogsOnGateway(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WIFI-WIFI-RDKB-TELE-501";
		String errorMessage = null;
		boolean status = false;
		boolean isFactoryResetDone = false;
		boolean isReactivated = false;
		int stepNumber = 1;
		int preConStepNumber = 1;
		int postConStepNumber = 1;
		String stepNum = "S" + stepNumber;
		List<Dut> ssidVisibleDevices = new ArrayList<Dut>();
		Dut ssidVisibleDeviceOne = null;
		Dut ssidVisibleDeviceTwo = null;
		String pattenMatcher = null;
		String searchLogMessage = null;
		String logMessage = null;
		String getMacAddress = null;

		// Variable Declaration Ends
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-WIFI-RDKB-TELE-5001");
			LOGGER.info("TEST DESCRIPTION: Verify the RDKB telemetry logs in Gateway connected with WiFi");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION 1 : Perform factory reset on the device");
			LOGGER.info("PRE-CONDITION 2 : Reactivate the router device");
			LOGGER.info(
					"PRE-CONDITION 3 : Verify the private wifi 2.4 GHz and 5 GHz SSID's are broadcasting in connected client");
			LOGGER.info("PRE-CONDITION 4 : Set and verify the wifi telemetry log interval as 5 minutes");
			LOGGER.info("PRE-CONDITION 5 : Verify the wifi helath log file");
			LOGGER.info("Step 1 : Connect the client into 2.4 GHz private ssid and verify connection status");
			LOGGER.info("Step 2 : Connect another client into 5 GHz private ssid and verify connection status");
			LOGGER.info("Step 3 : Verify the correct IPv4 address for client connected with private wifi 2.4 GHz ssid");
			LOGGER.info("Step 4 : Verify the correct IPv6 address for client connected with private wifi 2.4 GHz ssid");
			LOGGER.info(
					"Step 5 : Verify the internet connectivity in the client connected with private wifi 2.4 GHz ssid using ipv4 interface");
			LOGGER.info(
					"Step 6 : Verify the internet connectivity in the client connected with private wifi 2.4 GHz ssid using ipv6 interface");
			LOGGER.info("Step 7 : Verify the correct IPv4 address for client connected with private wifi 5 GHz ssid");
			LOGGER.info("Step 8 : Verify the correct IPv6 address for client connected with private wifi 5 GHz ssid");
			LOGGER.info(
					"Step 9 : Verify the internet connectivity in the client connected with private wifi 5 GHz ssid using ipv4 interface ");
			LOGGER.info(
					"Step 10 : Verify the internet connectivity in the client connected with private wifi 5 GHz ssid using ipv6 interface ");
			LOGGER.info(
					"Step 11 : Verify log information \"WIFI_BANDUTILIZATION_1\" is available in wifihealth.txt log");
			LOGGER.info(
					"Step 12 : Verify log information \"WIFI_BANDUTILIZATION_2\" is available in wifihealth.txt log");
			LOGGER.info(
					"Step 13 : Verify log information \"WIFI_MAC_1\" is available and validating consumable format for Client Mac in wifihealth.txt log file");
			LOGGER.info(
					"Step 14 : Verify log information \"WIFI_MAC_2\" is available and validating consumable format for Client Mac in wifihealth.txt log file");
			LOGGER.info(
					"Step 15 : Verify log information \"WIFI_MAC_1_TOTAL_COUNT\" is available in wifihealth.txt log");
			LOGGER.info(
					"Step 16 : Verify log information \"WIFI_MAC_2_TOTAL_COUNT\" is available in wifihealth.txt log");
			LOGGER.info("Step 17 : Verify log information \"WIFI_RXCLIENTS_1\" is available in wifihealth.txt log");
			LOGGER.info("Step 18 : Verify log information \"WIFI_RXCLIENTS_2\" is available in wifihealth.txt log");
			LOGGER.info("Step 19 : Verify log information \"WIFI_TXCLIENTS_1\" is available in wifihealth.txt log");
			LOGGER.info("Step 20 : Verify log information \"WIFI_TXCLIENTS_2\" is available in wifihealth.txt log");
			LOGGER.info(
					"Step 21 : Verify log information \"Wifi_Broadcast_Complete\" is available in wifihealth.txt log");
			LOGGER.info(
					"Step 22 : Verify log information \"Wifi_name_broadcasted\" is available in wifihealth.txt log");
			LOGGER.info(
					"Step 23 : Verify log information \"Rdkb_connected_clients\" is available in wifihealth.txt log");
			LOGGER.info("Step 24 : Verify disconnecting the client from 2.4 GHz private wifi SSID");
			LOGGER.info("Step 25 : Verify disconnecting the client from 5 GHz private wifi SSID");
			LOGGER.info("Step 26 : Verify log information \"RDKB_SYS_MEM_INFO_SYS\" is available in SelfHeal.txt.0");
			LOGGER.info("Step 27 : Verify log information \"USED_MEM\" is available in SelfHeal.txt.0");
			LOGGER.info("Step 28 : Verify log information \"FREE_MEM\" is available in SelfHeal.txt.0");
			LOGGER.info("POST-CONDITION 1 : Reactivate the Device");
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : PERFORM FACTORY RESET ON THE DEVICE
			 */

			isFactoryResetDone = BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv,
					preConStepNumber);
			/**
			 * PRE-CONDITION 2 : REACTIVATE THE ROUTER DEVICE
			 */
			preConStepNumber++;
			isReactivated = BroadBandPreConditionUtils.executePreConditionToReacitivateDevice(device, tapEnv,
					preConStepNumber);
			/**
			 * PRE-CONDITION 3 : VERIFY THE PRIVATE WIFI 2.4 GHZ AND 5 GHZ SSID'S ARE
			 * BROADCASTING IN CONNECTED CLIENT
			 */
			preConStepNumber++;
			ssidVisibleDevices = BroadBandPreConditionUtils.executePreConditionToGetBothPrivateWiFiSsidsVisibleDevices(
					device, tapEnv, preConStepNumber, BroadBandTestConstants.CONSTANT_2);
			ssidVisibleDeviceOne = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_0);
			ssidVisibleDeviceTwo = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_1);
			/**
			 * PRE-CONDITION 4 : SET AND LOG INTERVAL FOR TELEMETRY
			 */
			preConStepNumber++;
			BroadBandPreConditionUtils.executePreConditionToSetTelemetryLogInterval(tapEnv, device, preConStepNumber);

			/**
			 * PRE-CONDITION 5 : VERIFY THE WIFI HEALTH LOG FILE AVAILABILITY
			 */
			preConStepNumber++;
			BroadBandPreConditionUtils.executePreConditionToVerifyIsLogFileExist(tapEnv, device, preConStepNumber,
					BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG,
					BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1 : CONNECT THE CLIENT INTO 2.4 GHZ PRIVATE SSID AND VERIFY CONNECTION
			 * STATUS
			 */
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);
			/**
			 * STEP 2 : CONNECT THE CLIENT INTO 5 GHZ PRIVATE SSID AND VERIFY CONNECTION
			 * STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 3-6 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED WITH
			 * 2.4GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 7-10 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 7;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * SETP 11 : VERIFY LOG INFORMATION "WIFI_BANDUTILIZATION_1" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber = 11;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_BANDUTILIZATION, BroadBandTestConstants.STRING_CONSTANT_1,
					BroadBandTestConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateTelemetryInWiFiLogFile(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG);

			/**
			 * SETP 12 : VERIFY LOG INFORMATION "WIFI_BANDUTILIZATION_2" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_BANDUTILIZATION, BroadBandTestConstants.STRING_CONSTANT_2,
					BroadBandTestConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateTelemetryInWiFiLogFile(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG);

			/**
			 * SETP 13 : VERIFY LOG INFORMATION \"WIFI_MAC_1\" IS AVAILABLE AND VALIDATING
			 * CONSUMABLE FORMAT FOR CLIENT MAC IN WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			status = false;
			stepNum = "S" + stepNumber;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_MAC, BroadBandTestConstants.STRING_CONSTANT_1,
					BroadBandTestConstants.COLON);
			String response = null;
			String command = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY LOG INFORMATION " + searchLogMessage
					+ " IS AVAILABLE IN WIFIHEALTH.TXT LOG");
			LOGGER.info(
					"STEP " + stepNumber + " : ACTION : GREP -I " + searchLogMessage + " /RDKLOGS/LOGS/WIFIHEALTH.TXT");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : EXPECTED TELEMETRY " + searchLogMessage
					+ " SHOULD BE AVAILABLE IN WIFIHEALTH.TXT LOG");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO VERIFY THE TELEMETRY " + searchLogMessage + " IN WIFIHEALTH.TXT LOG";
			long startTime = System.currentTimeMillis();
			try {
				command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
						BroadBandTestConstants.TEXT_DOUBLE_QUOTE, searchLogMessage,
						BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
						BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG, BroadBandTestConstants.SYMBOL_PIPE,
						BroadBandTestConstants.CMD_TAIL_1);
				do {
					response = tapEnv.executeCommandUsingSsh(device, command);
					status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(
							response.toLowerCase(),
							((Device) ssidVisibleDeviceOne).getConnectedDeviceInfo().getWifiMacAddress().toLowerCase());
				} while (!status
						&& ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS)
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			} catch (Exception e) {
				LOGGER.error(
						"EXCEPTION OCCURRED WHILE VALIDATING " + searchLogMessage + " TELEMETRY :" + e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY VERIFIED THE TELEMETRY " + searchLogMessage
						+ " IN WIFIHEALTH.TXT LOG");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * SETP 14: VERIFY LOG INFORMATION \"WIFI_MAC_2\" IS AVAILABLE AND VALIDATING
			 * CONSUMABLE FORMAT FOR CLIENT MAC IN WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			status = false;
			stepNum = "S" + stepNumber;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_MAC, BroadBandTestConstants.STRING_CONSTANT_2,
					BroadBandTestConstants.COLON);
			response = null;
			command = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY LOG INFORMATION " + searchLogMessage
					+ " IS AVAILABLE IN WIFIHEALTH.TXT LOG");
			LOGGER.info(
					"STEP " + stepNumber + " : ACTION : GREP -I " + searchLogMessage + " /RDKLOGS/LOGS/WIFIHEALTH.TXT");
			LOGGER.info("STEP " + stepNumber + " : EXPECTED : EXPECTED TELEMETRY " + searchLogMessage
					+ " SHOULD BE AVAILABLE IN WIFIHEALTH.TXT LOG");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO VERIFY THE TELEMETRY " + searchLogMessage + " IN WIFIHEALTH.TXT LOG";
			startTime = System.currentTimeMillis();
			try {
				command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
						BroadBandTestConstants.TEXT_DOUBLE_QUOTE, searchLogMessage,
						BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
						BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG, BroadBandTestConstants.SYMBOL_PIPE,
						BroadBandTestConstants.CMD_TAIL_1);
				do {
					response = tapEnv.executeCommandUsingSsh(device, command);
					getMacAddress = ((Device) ssidVisibleDeviceTwo).getConnectedDeviceInfo().getWifiMacAddress()
							.toLowerCase();
					status = CommonMethods.isNotNull(response)
							&& CommonUtils.patternSearchFromTargetString(response.toLowerCase(), getMacAddress);
				} while (!status
						&& ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS)
						&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			} catch (Exception e) {
				LOGGER.error(
						"EXCEPTION OCCURRED WHILE VALIDATING " + searchLogMessage + " TELEMETRY :" + e.getMessage());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY VERIFIED THE TELEMETRY " + searchLogMessage
						+ " IN WIFIHEALTH.TXT LOG");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * SETP 15 : VERIFY LOG INFORMATION \"WIFI_MAC_1_TOTAL_COUNT\" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_MAC_TOTAL_COUNT.replaceAll(
							BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_CONSTANT_1),
					BroadBandTestConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateTelemetryInWiFiLogFile(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG);

			/**
			 * SETP 16 : VERIFY LOG INFORMATION \"WIFI_MAC_2_TOTAL_COUNT\" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_MAC_TOTAL_COUNT.replaceAll(
							BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_CONSTANT_2),
					BroadBandTestConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateTelemetryInWiFiLogFile(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG);

			/**
			 * SETP 17 : VERIFY LOG INFORMATION \"WIFI_RXCLIENTS_1\" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_RXCLIENTS, BroadBandTestConstants.STRING_CONSTANT_1,
					BroadBandTestConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateTelemetryInWiFiLogFile(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG);

			/**
			 * SETP 18 : VERIFY LOG INFORMATION \"WIFI_RXCLIENTS_2\" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_RXCLIENTS, BroadBandTestConstants.STRING_CONSTANT_2,
					BroadBandTestConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateTelemetryInWiFiLogFile(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG);

			/**
			 * SETP 19 : VERIFY LOG INFORMATION \"WIFI_TXCLIENTS_1\" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_TXCLIENTS, BroadBandTestConstants.STRING_CONSTANT_1,
					BroadBandTestConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateTelemetryInWiFiLogFile(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG);

			/**
			 * SETP 20 : VERIFY LOG INFORMATION \"WIFI_TXCLIENTS_2\" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.LOG_MESSAGE_WIFI_TXCLIENTS, BroadBandTestConstants.STRING_CONSTANT_2,
					BroadBandTestConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateTelemetryInWiFiLogFile(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG);

			/**
			 * SETP 21 : VERIFY LOG INFORMATION \"WIFI_BROADCAST_COMPLETE\" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.ACTIVATION_WIFI_BROADCAST_COMPLETE, BroadBandTestConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			executeTestStepToValidateTelemetryInWiFiLogFile(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0);

			/**
			 * SETP 22 : VERIFY LOG INFORMATION \"WIFI_NAME_BROADCASTED\" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTraceConstants.ACTIVATION_WIFI_NAME_BROADCAST, BroadBandTestConstants.COLON);
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.PATTERN_FOR_STRING);
			executeTestStepToValidateTelemetryInWiFiLogFile(device, testCaseId, stepNumber, pattenMatcher,
					searchLogMessage, BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0);
			/**
			 * SETP 23 : VERIFY LOG INFORMATION \"RDKB_CONNECTED_CLIENTS\" IS AVAILABLE IN
			 * WIFIHEALTH.TXT LOG
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			getMacAddress = ((Device) ssidVisibleDeviceOne).getConnectedDeviceInfo().getWifiMacAddress();
			logMessage = BroadBandTraceConstants.RDKB_CONNECTED_CLIENTS + BroadBandTestConstants.COLON
					+ BroadBandTraceConstants.STRING_LOG_MESSAGE + getMacAddress;
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, logMessage,
					BroadBandCommandConstants.LOCATION_LM_LOG_TXT_0, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = CommonMethods.isNotNull(response) && response.contains(BroadBandTraceConstants.STRING_CONNECTED)
					|| response.contains(BroadBandTestConstants.STRING_ONLINE);
			errorMessage = "UNABLE TO VERIFIED THE TELEMETRY \"RDKB_CONNECTED_CLIENTS\" IN LM.TXT.0 LOG";
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE TELEMETRY \"RDKB_CONNECTED_CLIENTS\" IN LM.TXT.0 LOG");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 24 : VERIFY DISCONNECTING THE CLIENT FROM 2.4 GHZ PRIVATE WIFI SSID
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * STEP 25 : VERIFY DISCONNECTING THE CLIENT FROM 5 GHZ PRIVATE WIFI SSID
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 26 : VERIFY LOG INFORMATION \"RDKB_SYS_MEM_INFO_SYS\" IS AVAILABLE IN
			 * SELFHEAL.TXT.0 LOG
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : VERIFY LOG INFORMATION \"RDKB_SYS_MEM_INFO_SYS\" IS AVAILABLE IN SELFHEAL.TXT.0 LOG");
			LOGGER.info("STEP " + stepNumber
					+ " : ACTION : GREP -I \"RDKB_SYS_MEM_INFO_SYS\" /RDKLOGS/LOGS/WIFIHEALTH.TXT");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : EXPECTED TELEMETRY \"RDKB_SYS_MEM_INFO_SYS\" SHOULD BE AVAILABLE IN SELFHEAL.TXT.0 LOG");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO VERIFY THE TELEMETRY \"RDKB_SYS_MEM_INFO_SYS\" IN SELFHEAL.TXT.0 LOG";
			searchLogMessage = BroadBandTraceConstants.LOG_MESSAGE_RDKB_SYS_MEM_INFO_SYS;
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, searchLogMessage,
					BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.FORTYFIVE_MINUTES_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = CommonMethods.isNotNull(response)
					&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_FOR_SYS_TOTAL_MEMEORY)
					&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_FOR_SYS_USED_MEMEORY)
					&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_FOR_SYS_FREE_MEMEORY);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE TELEMETRY \"RDKB_SYS_MEM_INFO_SYS\" IN SELFHEAL.TXT.0 LOG");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * SETP 27 : VERIFY LOG INFORMATION \"USED_MEM\" IS AVAILABLE IN SELFHEAL.TXT.0
			 * LOG
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : VERIFY LOG INFORMATION \"USED_MEM\" IS AVAILABLE IN SELFHEAL.TXT.0 LOG");
			LOGGER.info("STEP " + stepNumber + " : ACTION : GREP -I \"USED_MEM\" /RDKLOGS/LOGS/SELFHEAL.TXT.0");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : EXPECTED TELEMETRY \"USED_MEM\" SHOULD BE AVAILABLE IN SELFHEAL.TXT.0 LOG");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO VERIFY THE TELEMETRY \"USED_MEM\" IN SELFHEAL.TXT.0 LOG";
			searchLogMessage = BroadBandTraceConstants.LOG_MESSAGE_WIFI_USED_MEM;
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.COLON, BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, searchLogMessage,
					BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response, pattenMatcher);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE TELEMETRY \"USED_MEM\" IN SELFHEAL.TXT.0 LOG");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * SETP 28 : VERIFY LOG INFORMATION \"FREE_MEM\" IS AVAILABLE IN SELFHEAL.TXT.0
			 * LOG
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : VERIFY LOG INFORMATION \"FREE_MEM\" IS AVAILABLE IN SELFHEAL.TXT.0 LOG");
			LOGGER.info("STEP " + stepNumber + " : ACTION : GREP -I \"FREE_MEM\" /RDKLOGS/LOGS/SELFHEAL.TXT.0");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : EXPECTED TELEMETRY \"FREE_MEM\" SHOULD BE AVAILABLE IN SELFHEAL.TXT.0 LOG");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO VERIFY THE TELEMETRY \"FREE_MEM\" IN SELFHEAL.TXT.0 LOG";
			searchLogMessage = BroadBandTraceConstants.LOG_MESSAGE_WIFI_FREE_MEM;
			pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
					BroadBandTestConstants.COLON, BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, searchLogMessage,
					BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response, pattenMatcher);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE TELEMETRY \"FREE_MEM\" IN SELFHEAL.TXT.0 LOG");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = "EXCEPTION OCCURRED WHILE VALIDATING TEST METHOD testToverifyWiFiTelemetryLogsOnGateway() "
					+ errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * POST-CONDITION 1 : DEVICE REACTIVATION
			 */
			if (isFactoryResetDone && !isReactivated) {
				BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, isReactivated,
						postConStepNumber);
				postConStepNumber++;
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-WIFI-RDKB-TELE-5001");
	}

	/**
	 * Test step method used to validate the given wifi telemetry in wifi helath log
	 * file
	 * 
	 * @param device           instance of{@link Dut}
	 * @param testCaseId       Test case ID
	 * @param stepNumber       Step Number
	 * @param indexValue       Index Value for wifi band
	 * @param pattenMatcher    Pattern Matcher for Log
	 * @param searchLogMessage Search Message from Log file
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	public static void executeTestStepToValidateTelemetryInWiFiLogFile(Dut device, String testCaseId, int stepNumber,
			String pattenMatcher, String searchLogMessage, String logFileName) {
		String errorMessage = null;
		boolean status = false;
		String stepNum = "S" + stepNumber;
		String response = null;
		String command = null;
		long startTime = System.currentTimeMillis();
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : VERIFY LOG INFORMATION " + searchLogMessage
				+ " IS AVAILABLE IN WIFIHEALTH.TXT LOG");
		LOGGER.info("STEP " + stepNumber + " : ACTION : GREP -I " + searchLogMessage + " " + logFileName);
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : EXPECTED TELEMETRY " + searchLogMessage
				+ " SHOULD BE AVAILABLE IN " + logFileName);
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO VERIFY THE TELEMETRY " + searchLogMessage + " IN " + logFileName;
		try {
			command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
					BroadBandTestConstants.TEXT_DOUBLE_QUOTE, searchLogMessage,
					BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
					logFileName, BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.CMD_TAIL_1);
			do {
				response = tapEnv.executeCommandUsingSsh(device, command);
				status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response, pattenMatcher);
			} while (!status
					&& ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS)
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
		} catch (Exception e) {
			LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING " + searchLogMessage + " TELEMETRY :" + e.getMessage());
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY VERIFIED THE TELEMETRY " + searchLogMessage
					+ " IN " + logFileName);
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	}

}