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
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils.WifiOperatingStandard;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaServerResponse;

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
			// verify 5ghz radio status
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
				int counter = 0;
				for (counter = 0; counter < responseInJson.length(); counter++) {
					JSONObject json = responseInJson.getJSONObject(counter);
					String name = json.getString(BroadBandTestConstants.STRING_NAME);
					if (CommonMethods.isNotNull(name)) {
						passCount++;
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
			status = false;
			status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
					connectedDeviceActivated, tapEnv);

			if (status) {
				LOGGER.info("STEP " + stepNumberss[1] + ":  ACTUAL RESULT : Interface got the correct IPv6 address.");
			} else {
				LOGGER.info("STEP " + stepNumberss[1] + ":  ACTUAL RESULT :" + errorMessage);
			}

			LOGGER.info("******************************************************");
			tapEnv.updateExecutionStatus(device, testId, radioStepNumber, status, errorMessage, false);
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
			LOGGER.info("***************************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : Verify whether interface  get the correct IPv6  address.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Get the device IPv6 address using below command Linux : ifconfig wlan0 |grep -i \"inet6 addr:\" Windows:ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\" ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Interface IPv6 address should  be shown");
			LOGGER.info("***************************************************************************************");
			errorMessage = "interface  didnt got the correct IPV6 address";
			String osType = ((Device) connectedDeviceActivated).getOsType();
			status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
					connectedDeviceActivated, tapEnv);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : Interface  got the correct IPv6 address");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

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
			LOGGER.info("***************************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : Verify whether interface  get the correct IPv6  address.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Get the device IPv6 address using below command Linux : ifconfig wlan0 |grep -i \"inet6 addr:\" Windows:ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\" ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Interface IPv6 address should  be shown");
			LOGGER.info("***************************************************************************************");
			errorMessage = "interface  didnt got the correct IPV6 address";
			String osType = ((Device) connectedDeviceActivated).getOsType();
			status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
					connectedDeviceActivated, tapEnv);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : Interface  got the correct IPv6 address");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("***************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

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
			errorMessage = "MAC Filter mode is not configured as Allow";
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_MAC_FILTER_MODE);
			status = response.equalsIgnoreCase(BroadBandTestConstants.MAC_FILTER_ALLOW);
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
			LOGGER.info("STEP 14: Verify whether there is no connectivity using that particular interface using IPV4 ");
			LOGGER.info("EXPECTED: Connectivity check should'nt  return the status as 200");
			LOGGER.info("#####################################################################################");
			testStepNumber = "s14";
			status = false;
			errorMessage = "Connectivty check using IPV4 address success";
			command = ((Device) connectedDeviceActivated).getOsType()
					.equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
							? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
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
	 * @param settop                     {@link Settop}
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
	 * Verify the IP range in 5GHZ WiFi connected client when DHCP Server
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
		// Variable to store command response
		String response = null;
		// Variable to store error Message
		String errorMessage = null;
		List<Dut> connectedClientSettops = null;
		// Dut instance to store 2 ghz client device
		Dut clientConnectedWith2Ghz = null;
		// Dut instance to store 5 ghz client device
		Dut clientConnectedWith5Ghz = null;

		String ssidName2Ghz = null;

		String ssidName5Ghz = null;

		try {

			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-WEBPA-4005");

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
     * Test case is created to check the ability to change mode from g-n to n-only mode on the 2.4 GHz radio
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1: Check the operating mode for 2.4GHz Wi-Fi from RDKB device</li>
     * <li>Step 2: Check the mode of operation in 2.4 Ghz client</li>
     * <li>Step 3: Change the mode of operation in 2.4GHz client from 802.11 g/n to 802.11 n and verify the status</li>
     * <li>Step 4: Check 2.4 Ghz client is connected and IPV4 address obtained</li>
     * <li>Step 5: Check the mode of operation in 2.4 Ghz client</li>
     * </ol>
     * 
     * @author anandam.s
     * @refactor Alan_Bivera
     * 
     * @param device
     *            {@link Dut}
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
		     * Step 5 : Connect to 2.4 GHz client and verify whether device is connected and IPV4 address
		     * obtained
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
    }

    /**
     * Method to check the connectivity of device over wlan
     * 
     * @param Dut
     *            device
     * 
     * @return boolean connection status
     * @refactor Alan_Bivera
     */

    private boolean checkConnectivityOfDevice(Dut device) {

	LOGGER.debug("Entering checkConnectivityOfDevice");
	String commandResponse = null;
	String command = ((Device) device).getOsType().equalsIgnoreCase(BroadBandConnectedClientTestConstants.OS_LINUX)
		? BroadBandConnectedClientTestConstants.COMMAND_CURL_LINUX_IPV4_ADDRESS
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

}