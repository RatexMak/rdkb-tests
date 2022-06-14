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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.test.AutomaticsTestBase;

public class BroadBandPublicWifiTests extends AutomaticsTestBase {

	/** Constant holds the test step number **/
	private static int stepNumber = 0;

	/** Constant holds the test pre condition step number **/
	private static int preConStepNumber = 0;

	/** Constant holds the test post condition step number **/
	private static int postConStepNumber = 0;

	/** Constant holds the test step number with S **/
	private static String stepNum = "";

	/** Constant holds the Error Message **/
	private static String errorMessage = null;

	/** Constant holds the test step status **/
	private static boolean status = false;

	/** Variable holds the device factory status **/
	private static boolean isFactoryReset = false;

	/** Variable holds the device reactivated status **/
	private static boolean isReactivated = false;

	/** Constant holds the Current Firmware version **/
	private static String initialFirmwareVersion = null;

	/**
	 *
	 * Test Case : Private WiFi channel selection should remain same post Public
	 * WiFi enabled
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Pre-Condition 1:Verify the private wifi 2.4 GHz and 5 GHz SSID's are
	 * broadcasting in connected client</li>
	 * <li>Pre-Condition 2:Disable the prefer private wifi</li>
	 * <li>Step 1: Verify by default, Channel Selection mode should be set to
	 * true.</li>
	 * <li>Step 2: Enable the public on the Gateway device</li>
	 * <li>Step 3: Get and verify the channel selection value true for 2.4 and 5
	 * GHz.</li>
	 * <li>Step 4: Connect a client to the public wifi 5 GHz SSID.</li>
	 * <li>Step 5: Verify the correct IPv4 address for client connected with 5 GHz
	 * public wifi SSID in client</li>
	 * <li>Step 6: Verify the internet connectivity in the client connected with 5
	 * GHz public wifi ssid using ipv4 interface</li>
	 * <li>Step 7: Verify disconnecting the 5 GHz public wifi SSID</li>
	 * <li>Step 8: Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 9: Verify the correct IPv4 address for client connected with 2.4 GHz
	 * SSID</li>
	 * <li>Step 10: Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 11: Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 12:Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 13: Connect the connected client in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 14: Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 15: Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 16: Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 17: Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 18: Verify disconnecting the 2.4 GHz private wifi SSID</li>
	 * <li>Step 19: Verify disconnecting the 5 GHz private wifi SSID</li>
	 * <li>Post-Condition 1 : Enable the Public Wifi</li>
	 * <li>Post-Condition 2 : Enable the Prefer Private Wifi</li>
	 * <li>Post-Condition 3 : Disable the Auto Selection Mode</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @Refactor Sruthi Santhosh
	 * 
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
	@TestDetails(testUID = "TC-RDKB-PUBLIC-WIFI-AUTO-CH-3001")
	public void testToVerifyAotoChnlStsPublicWiFiEnbldOrDsbld(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-PUBLIC-WIFI-AUTO-CH-301";
		stepNumber = 1;
		postConStepNumber = 1;
		stepNum = "S" + stepNumber;
		String errorMessage = "";
		boolean status = false;
		BroadBandResultObject resultObject = null;
		Dut ssidVisibleDeviceOne = null;
		Dut ssidVisibleDeviceTwo = null;
		List<Dut> ssidVisibleDevices = null;
		boolean isDefaultAutoChnlChanged = false;
		List<WebPaParameter> webPaParameters = new ArrayList<WebPaParameter>();

		boolean isFibreDevice = DeviceModeHandler.isFibreDevice(device);
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PUBLIC-WIFI-AUTO-CH-3001");
		LOGGER.info("TEST DESCRIPTION: Private WiFi channel selection should remain same post PublicWiFi enabled");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"Pre-Condition 1:Verify the private wifi 2.4 GHz and 5 GHz SSID's are broadcasting in connected client");
		LOGGER.info("Pre-Condition 2:Disable the prefer private wifi");
		LOGGER.info("Step 1: Verify by default, Channel Selection mode should be set to true.");
		LOGGER.info("Step 2: Enable the public on the Gateway device");
		LOGGER.info("Step 3: Get and verify the channel selection value true for 2.4 and 5 GHz.");
		LOGGER.info("Step 4: Connect a client to the public wifi 5 GHz SSID.");
		LOGGER.info(
				"Step 5: Verify the correct IPv4 address for client connected with 5 GHz public wifi SSID in client");
		LOGGER.info(
				"Step 6: Verify the internet connectivity in the client connected with 5 GHz public wifi ssid using ipv4 interface");
		LOGGER.info("Step 7: Verify disconnecting the 5 GHz public wifi SSID");
		LOGGER.info("Step 8: Connect the connected client in the setup to 2.4 GHz SSID and verify connection status");
		LOGGER.info("Step 9: Verify the correct IPv4 address for client connected with 2.4 GHz SSID");
		LOGGER.info("Step 10: Verify the correct IPv6 address for client connected with 2.4 GHz SSID");
		LOGGER.info(
				"Step 11: Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz");
		LOGGER.info(
				"Step 12:Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz");
		LOGGER.info("Step 13: Connect the connected client in the setup to 5 GHz SSID and verify connection status");
		LOGGER.info("Step 14: Verify the correct IPv4 address for client connected with 5 GHz SSID");
		LOGGER.info("Step 15: Verify the correct IPv6 address for client connected with 5 GHz SSID");
		LOGGER.info(
				"Step 16: Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz");
		LOGGER.info(
				"Step 17: Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz");
		LOGGER.info("Step 18: Verify disconnecting the 2.4 GHz private wifi SSID");
		LOGGER.info("Step 19: Verify disconnecting the 5 GHz private wifi SSID");

		LOGGER.info("Post-Condition 1 : Enable the Prefer Private Wifi");
		LOGGER.info("Post-Condition 2 : Disable the Auto Selection Mode");
		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : VERIFY THE PRIVATE WIFI 2.4 GHZ AND 5 GHZ SSID'S ARE
			 * BROADCASTING IN CONNECTED CLIENT
			 */
			ssidVisibleDevices = BroadBandPreConditionUtils.executePreConditionToGetBothPrivateWiFiSsidsVisibleDevices(
					device, tapEnv, preConStepNumber, BroadBandTestConstants.CONSTANT_2);
			ssidVisibleDeviceOne = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_0);
			ssidVisibleDeviceTwo = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_1);

			/**
			 * PRE-CONDITION 2 : DISABLE THE PREFER PRIVATE WIFI
			 */
			preConStepNumber++;
			BroadBandPreConditionUtils.executePreCondToTogglePreferPrivateWiFi(device, tapEnv, false, preConStepNumber);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : VERIFY BY DEFAULT, CHANNEL SELECTION MODE SHOULD BE SET TO TRUE
			 */
			status = false;
			errorMessage = "Unable to Change AutoChannelEnable to TRUE. ";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify by default, Channel Selection mode should be set to true. ");
			LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa command:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ + " And "
					+ BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Channel Selection mode should be set to Automatic.");
			LOGGER.info("******************************************************************************");
			WebPaParameter autoChannel2Ghz = BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3);
			webPaParameters.add(autoChannel2Ghz);
			WebPaParameter autoChannel5Ghz = BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3);
			webPaParameters.add(autoChannel5Ghz);
			resultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
					webPaParameters);
			status = resultObject.isStatus();
			errorMessage = resultObject.getErrorMessage();
			if (status) {
				isDefaultAutoChnlChanged = status;
				LOGGER.info(
						"STEP " + stepNumber + ": ACTUAL : Channel Selection mode has been successfully set to true");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 2 : ENABLE THE PUBLIC ON THE GATEWAY DEVICE
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			if (!isFibreDevice) {
				errorMessage = "Unable to enable the public wifi on gateway device";
				LOGGER.info("******************************************************************************");
				LOGGER.info(
						"STEP " + stepNumber + " : DESCRIPTION : Set  and verify the publicwifi status is enabled.");
				LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa command:"
						+ BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI);
				LOGGER.info("STEP " + stepNumber
						+ " : EXPECTED : Public wifi must be enabled and it must return value as true");
				LOGGER.info("******************************************************************************");
				List<WebPaParameter> webPaParametersPublic = BroadBandWebPaUtils
						.getListOfWebpaParametersForBothPublicWifis();
				resultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
						webPaParametersPublic);
				status = resultObject.isStatus();
				errorMessage = resultObject.getErrorMessage();
				if (status) {
					LOGGER.info(
							"STEP " + stepNumber + ": ACTUAL : Successfully enabled the public wifi on gateway device");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			} else {
				errorMessage = "Public related step is not applicable for fibre models";
				LOGGER.info(errorMessage);
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			/**
			 * Step 3 : GET AND VERIFY THE CHANNEL SELECTION VALUE TRUE FOR 2.4 AND 5 GHZ
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Unable to get AutoChannelEnable to TRUE. ";
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Get and verify the channel selection value true for 2.4 and 5 GHz.");
			LOGGER.info(
					"STEP 3: ACTION : Check the mode using the below webpa commands for both 2.4GHz and 5GHz Wireless radios. curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.WiFi.Radio.10000.AutoChannelEnable curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.WiFi.Radio.10100.AutoChannelEnable  ");
			LOGGER.info(
					"STEP 3: EXPECTED : Channel Selection mode should be set to Automatic.The output of the command should be as below,Expected Output: {\"parameters\":[{\"name\":\"Device.WiFi.Radio.10000.AutoChannelEnable\",\"value\":\"true\",\"dataType\":3,\"parameterCount\":1,\"message\":\"Success\"}],\"statusCode\":200}{\"parameters\":[{\"name\":\"Device.WiFi.Radio.10100.AutoChannelEnable\",\"value\":\"true\",\"dataType\":3,\"parameterCount\":1,\"message\":\"Success\"}],\"statusCode\":200}");
			LOGGER.info("******************************************************************************");
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS)
					&& BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ,
							BroadBandTestConstants.TRUE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Channel Selection mode Successfully has been set to true");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 4 : CONNECT A CLIENT TO THE PUBLIC WIFI 5 GHZ SSID
			 */
			if (!isFibreDevice) {
				stepNumber++;
				executeTestStepToConnectPublicWiFi(device, testCaseId, ssidVisibleDeviceTwo,
						BroadBandTestConstants.BAND_5GHZ);

				/**
				 * Step 5-6 : VALITATE 5 GHZ PUBLIC WIFI INTERFACE & INTERNET CONNECTION ON
				 * CLIENT
				 */
				BroadBandWiFiUtils.validatePublicWifiConnectionStatus(device, testCaseId, ssidVisibleDeviceTwo,
						stepNumber);

				/**
				 * Step 7 : VERIFY DISCONNECTING THE 5 GHZ PUBLIC WIFI SSID
				 */
				stepNumber = 7;
				executeTestStepToConnectPublicWiFi(device, testCaseId, ssidVisibleDeviceTwo,
						BroadBandTestConstants.BAND_5GHZ);
			} else {
				stepNumber = BroadBandTestConstants.CONSTANT_4;
				while (stepNumber <= BroadBandTestConstants.CONSTANT_9) {
					stepNum = "s" + stepNumber;
					errorMessage = "STEP " + stepNumber + ": ACTUAL : PUBLIC WIFI NOT APPLICABLE  FOR PACE DEVICES";
					LOGGER.info("******************************************************************************");
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
					stepNumber++;
				}
			}
			/**
			 * STEP 8 : CONNECT THE CLIENT INTO 2.4 GHZ PRIVATE SSID AND VERIFY CONNECTION
			 * STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * STEP 9-12 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * STEP 13 : CONNECT THE CLIENT INTO 5 GHZ PRIVATE SSID AND VERIFY CONNECTION
			 * STATUS
			 */
			stepNumber = 13;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * STEP 14-17 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHZ AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 18 : VERIFY DISCONNECTING THE CLIENT FROM 2.4 GHZ PRIVATE WIFI SSID
			 */
			stepNumber = 18;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * STEP 19 : VERIFY DISCONNECTING THE CLIENT FROM 5 GHZ PRIVATE WIFI SSID
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");

			/**
			 * POST-CONDITION 1 : ENABLE/DISABLE THE PUBLIC WIFI based on value set in
			 * rdkb.whitelist.publicwifivalue property
			 */
			BroadBandPostConditionUtils.executePostConditionToEnableOrDisablePublicWifiBasedOnStbProperty(device,
					tapEnv, postConStepNumber);

			/**
			 * POST-CONDITION 2 : ENABLE THE PREFER PRIVATE WIFI
			 */
			postConStepNumber++;
			BroadBandPostConditionUtils.executePostConditionToTogglePreferPrivateWiFi(device, tapEnv, true,
					postConStepNumber);

			/**
			 * POST-CONDITION 3 : DISABLE THE AUTO CHANNEL SELECTION MODE
			 */
			if (isDefaultAutoChnlChanged) {
				postConStepNumber++;
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info(
						"POST-CONDITION : DESCRIPTION : 1.Verify by default, Channel Selection mode should be set to false.2.Disable public wifi using Webpa.");
				LOGGER.info(
						"POST-CONDITION : ACTION : Check the mode using the below dmcli commands for both 2.4GHz and 5GHz Wireless radios. curl -4 -k -H \"Authorization: Bearer <Token>\" -X PATCH <WEBPA URL>/device/mac:<ECM_MAC>/config -d \"{\"parameters\":[{\"dataType\":3, \"name\":\"Device.WiFi.Radio.10000.AutoChannelEnabl\",\"value\":\"false\"}]}\"curl -4 -k -H \"Authorization: Bearer <Token>\" -X PATCH <WEBPA URL>/device/mac:<ECM_MAC>/config -d \"{\"parameters\":[{\"dataType\":3, \"name\":\"Device.WiFi.Radio.10100.AutoChannelEnable\",\"value\":\"false\"}]}\"Execute webpa :curl -4 -k -H \"Authorization: Bearer <Token>\" -X PATCH <WEBPA URL>/device/mac:<ECM_MAC>/config -d \"{\"parameters\":[{\"dataType\":3, \"name\":\"Device.DeviceInfo.X_COMCAST_COM_xfinitywifiEnable\",\"value\":\"false\"}]}\" ");
				LOGGER.info(
						"POST-CONDITION : EXPECTED : Channel Selection mode should be set to Automatic. The output of the command should be as below, Expected Output:{\"parameters\":[{\"name\":\"Device.WiFi.Radio.10000.AutoChannelEnable\",\"value\":\"false\",\"dataType\":3,\"parameterCount\":1,\"message\":\"Success\"}],\"statusCode\":200}{\"parameters\":[{\"name\":\"Device.WiFi.Radio.10100.AutoChannelEnable\",\"value\":\"false\",\"dataType\":3,\"parameterCount\":1,\"message\":\"Success\"}],\"statusCode\":200}It must return value as FALSEExpected output:{\"parameters\":[{\"name\":\"Device.DeviceInfo.X_COMCAST_COM_xfinitywifiEnable\",\"value\":\"false\",\"dataType\":3,\"parameterCount\":1,\"message\":\"Success\"}],\"statusCode\":200}");
				LOGGER.info("#######################################################################################");
				webPaParameters.clear();
				WebPaParameter autoChannel2Ghz = BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_2GHZ,
						BroadBandTestConstants.FALSE, BroadBandTestConstants.CONSTANT_3);
				webPaParameters.add(autoChannel2Ghz);
				WebPaParameter autoChannel5Ghz = BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_AUTOCHANNELENABLE_STATUS_5GHZ,
						BroadBandTestConstants.FALSE, BroadBandTestConstants.CONSTANT_3);
				webPaParameters.add(autoChannel5Ghz);
				resultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
						webPaParameters);
				status = resultObject.isStatus();
				errorMessage = resultObject.getErrorMessage();
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : SUCCESSFULLY  CHANNEL SELECTION MODE  SET TO 'FALSE'.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-PUBLIC-WIFI-AUTO-CH-3001");
	}

	/**
	 * Test step method used to connect the public wifi for given GHz frequency band
	 * 
	 * @param device          instance of{@link Dut}
	 * @param testCaseId      Test case ID
	 * @param deviceToConnect Device to Connect
	 * @param wifiBand        Frequency band to connect the wifi
	 * @refactor Said Hisham
	 */
	public static void executeTestStepToConnectPublicWiFi(Dut device, String testCaseId, Dut deviceToConnect,
			String wifiBand) {
		/**
		 * STEP : CONNECT A CLIENT TO THE PUBLIC WIFI GIVEN GHZ SSID.
		 */
		stepNum = "S" + stepNumber;
		status = false;
		errorMessage = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"STEP " + stepNumber + " : DESCRIPTION : CONNECT A CLIENT TO THE PUBLIC WIFI " + wifiBand + " SSID.");
		LOGGER.info("STEP " + stepNumber + " : ACTION : CONNECT TO " + wifiBand
				+ " PUBLIC WIFI USING BELOW COMMANDS : FOR LINUX :nmcli dev wifi connect <ssid> FOR WINDOWS: netsh wlan connect ssid=<ssid>");
		LOGGER.info("STEP " + stepNumber + " : EXPECTED : CONNECTION SHOULD BE SUCCESSFUL FOR CLIENT WITH " + wifiBand
				+ " PUBLIC WIFI SSID ");
		LOGGER.info("#######################################################################################");
		errorMessage = "PUBLIC WIFI CONNECTION FAILED FOR " + wifiBand + " SSID IN CLIENT";
		status = ConnectedNattedClientsUtils.connectToSSID(deviceToConnect, tapEnv,
				BroadBandConnectedClientUtils.getPublicSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
						wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
								? WiFiFrequencyBand.WIFI_BAND_2_GHZ
								: WiFiFrequencyBand.WIFI_BAND_5_GHZ),
				BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PUBLIC_SSID_PASSPHRASE.replace(
								BroadBandTestConstants.STRING_REPLACE,
								wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
										? BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PUBLIC_WIFI
										: BroadBandWebPaConstants.WEBPA_INDEX_5_GHZ_PUBLIC_WIFI)),
				BroadBandConnectedClientTestConstants.SECURITY_MODE_OPEN.toLowerCase(), false);
		if (status) {
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : PUBLIC WIFI " + wifiBand
					+ " SSID CONNECTION SUCCESSFUL FOR CLIENT");
		} else {
			LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	}
}