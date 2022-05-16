/**
* Copyright 2021 Comcast Cable Communications Management, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
/* You may obtain a copy of the License at
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
import java.util.Map;

import org.apache.http.HttpStatus;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.enums.BroadBandWhixEnumConstants.WEBPA_AP_INDEXES;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsUtils;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.device.Device;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.parentalcontrol.BroadBandParentalControlUtils;
import com.automatics.rdkb.utils.LoggerUtils;

public class BroadBandSSIDTest extends AutomaticsTestBase {

	/**
	 * Verify network connectivity in clients when lnf enabled
	 * <ol>
	 * <li>PRE-CONDITION 1 : OBTAIN A WIFI CLIENT ASSOSIATED WITH THE GATEWAY</li>
	 * <li>PRE-CONDITION 2 :VERIFY THE CORRECT IPV4 ADDRESS FOR WIFI CLIENT</li>
	 * <li>PRE-CONDITION 3 :VERIFY THE CORRECT IPV4 ADDRESS FOR WIFI CLIENT</li>
	 * <li>PRE-CONDITION 4 : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI
	 * CLIENT USING IPV4 INTERFACE</li>
	 * <li>PRE-CONDITION 5 : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI
	 * CLIENT USING IPV6 INTERFACE</li>
	 * <li>PRE-CONDITION 6 : CONFIGURE RFC PAYLOAD TO ENABLE BLOCK AND LOST LNF
	 * FEATURE</li>
	 * <li>Enable lnf ssid and broadcast using webpa/dmcli</li>
	 * <li>Connect wifi connected client to lnf 2.4Ghz ssid</li>
	 * <li>VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4
	 * INTERFACE</li>
	 * <li>Connect wifi connected client to lnf 5Ghz ssid</li>
	 * <li>VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4
	 * INTERFACE</li>
	 * <li>Configure RFC payload to enable block and lost lnf feature</li>
	 * <li>Verify
	 * DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BlockLostandFoundInternet.Enable
	 * parameter is updated by RFC</li>
	 * <li>Connect wifi connected client to lnf 2.4Ghz ssid</li>
	 * <li>VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4
	 * INTERFACE</li> *
	 * <li>VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6
	 * INTERFACE</li>
	 * <li>Connect wifi connected client to lnf 5Ghz ssid</li>
	 * <li>VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4
	 * INTERFACE</li> *
	 * <li>VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6
	 * INTERFACE</li>
	 * <li>Connect wifi connected client to Private 2.4Ghz ssid</li>
	 * <li>VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4
	 * INTERFACE</li>
	 * <li>VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6
	 * INTERFACE</li>
	 * <li>Connect wifi connected client to Private 5Ghz ssid</li>
	 * <li>VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4
	 * INTERFACE</li>
	 * <li>VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6
	 * INTERFACE</li>
	 * <li>Reboot device and validate online status</li>
	 * <li>Validate Block and lost lnf status to be same using webpa</li>
	 * <li>Factory reset device using webpa</li>
	 * <li>Validate Block and lost lnf status to be default using webpa</li>
	 * <li>POST-CONDITION 1 : BROAD BAND DEVICE REACTIVATION USING WEBPA</li>
	 * <li>POST-CONDITION 2 : DELETE RFC PROPERTIES FILE FROM nvram</li>
	 * </ol>
	 * 
	 * @param device Dut instance
	 * @author Prasanthreddy A
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
	@TestDetails(testUID = "TC-RDKB-LNF-NTWK-1001")
	public void testToVerifyLnfSsid(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-LNF-NTWK-101";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		Dut wifiConnectedClient = null;
		boolean factoryReset = false;
		int counter = 0;
		boolean disabledByRfc = false;
		boolean enabledByRfc = false;
		boolean revertedRfc = false;
		BroadBandResultObject bandResultObject;
		JSONObject jsonObject = new JSONObject();
		String payload = BroadBandTestConstants.STRING_RFC_DATA_PAYLOAD
				.replace(BroadBandTestConstants.CONSTANT_REPLACE_STBMAC_LSAPAYLOADDATA, device.getHostMacAddress());
		Map<String, String> defaultParamValues = new HashMap<String, String>();
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-LNF-NTWK-1001");
		LOGGER.info("TEST DESCRIPTION: Verify network connectivity in clients when lnf enabled");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1  : OBTAIN A WIFI CLIENT ASSOSIATED WITH THE GATEWAY");
		LOGGER.info("PRE-CONDITION 2  :VERIFY THE CORRECT IPV4 ADDRESS FOR WIFI CLIENT");
		LOGGER.info("PRE-CONDITION 3  :VERIFY THE CORRECT IPV4 ADDRESS FOR WIFI CLIENT");
		LOGGER.info(
				"PRE-CONDITION 4  : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
		LOGGER.info(
				"PRE-CONDITION 5  : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6 INTERFACE");
		LOGGER.info("PRE-CONDITION 6 : CONFIGURE RFC PAYLOAD TO ENABLE BLOCK AND LOST LNF FEATURE");
		LOGGER.info("1. Enable lnf ssid and broadcast using webpa/dmcli");
		LOGGER.info("2. Connect wifi connected client to lnf 2.4Ghz ssid");
		LOGGER.info("3. VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
		LOGGER.info("4. Connect wifi connected client to lnf 5Ghz ssid");
		LOGGER.info("5. VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
		LOGGER.info("6.  Configure RFC payload to enable block and lost lnf feature");
		LOGGER.info(
				"7. Verify DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BlockLostandFoundInternet.Enable parameter is updated by RFC");
		LOGGER.info("8. Connect wifi connected client to lnf 2.4Ghz ssid");
		LOGGER.info("9. VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
		LOGGER.info("10. VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6 INTERFACE");
		LOGGER.info("11. Connect wifi connected client to lnf 5Ghz ssid");
		LOGGER.info("12. VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
		LOGGER.info("13. VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6 INTERFACE");
		LOGGER.info("14. Connect wifi connected client to Private 2.4Ghz ssid");
		LOGGER.info("15. VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
		LOGGER.info("16. VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6 INTERFACE");
		LOGGER.info("17. Connect wifi connected client to Private 5Ghz ssid");
		LOGGER.info("18. VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
		LOGGER.info("19. VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6 INTERFACE");
		LOGGER.info("20. Reboot device and validate online status");
		LOGGER.info("21. Validate Block and lost lnf status to be same using webpa");
		LOGGER.info("22. Factory reset device using webpa");
		LOGGER.info("23. Validate Block and lost lnf status to be default using webpa");
		LOGGER.info("POST-CONDITION 1 :  BROAD BAND DEVICE REACTIVATION USING WEBPA");
		LOGGER.info("POST-CONDITION 2 : DELETE RFC PROPERTIES FILE FROM nvram");
		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			wifiConnectedClient = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device, tapEnv,
					BroadBandTestConstants.EMPTY_STRING);
			/**
			 * PRECONDITION 6 : OBTAIN A WIFI CLIENT ASSOSIATED WITH THE GATEWAY
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 6 : DESCRIPTION : CONFIGURE RFC PAYLOAD TO ENABLE BLOCK AND LOST LNF FEATURE");
			LOGGER.info(
					"PRE-CONDITION 6 : ACTION :  COPY AND UPDATE /nvram/rfc.properties WITH MOCK RFC CONFIG SERVER URL 2. Post payload after replacing ESTB mac and enable/disable 3. REBOOT DEVICE OR TRIGGER CONFIGURABLE RFC CHECK-IN");
			LOGGER.info(
					"PRE-CONDITION 6 : EXPECTED : SUCCESSFULLY REBOOTED OR TRIGGERED CHECK-IN AFTER CONFIGURING RFC PAYLOAD");
			LOGGER.info("#######################################################################################");
			boolean disabledBlocklnf = false;
			boolean disabledBlocklnfPrefix = false;
			boolean disabledBlocklnfIpv6 = false;
			try {
				String[] params = new String[] { BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_ENABLE,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_PREFIX };
				defaultParamValues = tapEnv.executeMultipleWebPaGetCommands(device, params);

			} catch (Exception e) {
				LOGGER.info("Failed to get webpa response " + e.getMessage());
			}
			if (!defaultParamValues.isEmpty()) {

				try {
					if (defaultParamValues.get(BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE)
							.equals(BroadBandTestConstants.TRUE)) {
						jsonObject.put(
								CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.TR181_DOT,
										BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE),
								BroadBandTestConstants.FALSE);
						disabledBlocklnf = true;
					}
					if (defaultParamValues
							.get(BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_ENABLE)
							.equals(BroadBandTestConstants.TRUE)) {
						jsonObject.put(CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.TR181_DOT,
								BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_ENABLE),
								BroadBandTestConstants.FALSE);
						disabledBlocklnfPrefix = true;
					}
					if (defaultParamValues
							.get(BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_PREFIX)
							.equals(BroadBandTestConstants.TRUE)) {
						jsonObject.put(CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.TR181_DOT,
								BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_PREFIX),
								BroadBandTestConstants.FALSE);
						disabledBlocklnfIpv6 = true;
					}
				} catch (JSONException e) {
					LOGGER.error("Unable to parse given Json input array");
				}
				if (jsonObject.length() != BroadBandTestConstants.CONSTANT_0) {

					if (BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
							payload.replace(BroadBandTestConstants.STRING_REPLACE, jsonObject.toString()))) {
						errorMessage = "Unable to reboot device successfully";
						status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv) && disabledBlocklnf
								? BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
										BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE,
										BroadBandTestConstants.FALSE).isStatus()
								: true && disabledBlocklnfPrefix
										? BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
												BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_ENABLE,
												BroadBandTestConstants.FALSE).isStatus()
										: true && disabledBlocklnfIpv6 ? BroadBandRfcFeatureControlUtils
												.verifyParameterUpdatedByRfc(device, tapEnv,
														BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_PREFIX,
														BroadBandTestConstants.FALSE)
												.isStatus() : true;
						disabledByRfc = status;
					}
				} else {
					status = true;
				}

			}
			if (status) {
				LOGGER.info("PRE-CONDITION 6 : ACTUAL : SUCESSFULLY DISABLED BLOCK LNF INTERNET FEATURE");
			} else {
				LOGGER.error("PRE-CONDITION 6 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			stepNum = "S1";
			errorMessage = "Unable to enable lnf using webpa";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Enable lnf ssid and broadcast using webpa/dmcli");
			LOGGER.info(
					"STEP 1: ACTION : Execute webpa set command:Device.WiFi.SSID.{i}.Enable bool trueDevice.WiFi.AccessPoint.{i}.SSIDAdvertisementEnabled bool true");
			LOGGER.info("STEP 1: EXPECTED : Successfully enabled lnf using webpa ");
			LOGGER.info("**********************************************************************************");

			List<WebPaParameter> listOfSetParameter = new ArrayList<WebPaParameter>();
			List<WebPaParameter> listOfSetParameterToApply = new ArrayList<WebPaParameter>();

			listOfSetParameter.add(BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
					BroadBandWebPaConstants.WEBPA_WAREHOUSE_WIRELESS_SSID1_ENABLE_LNF_2_4, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.CONSTANT_3));
			listOfSetParameter.add(BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
					BroadBandWebPaConstants.WEBPA_WAREHOUSE_WIRELESS_SSID_ENABLE_LNF_5G, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.CONSTANT_3));
			listOfSetParameter.add(BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
					BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_AccessPoint_10004_SSIDAdvertisementEnabled,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3));
			listOfSetParameter.add(BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
					BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_AccessPoint_10104_SSIDAdvertisementEnabled,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.CONSTANT_3));
			listOfSetParameter.add(BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10004_SSID,
					BroadBandTestConstants.STRING_LNF_SSID_24GHZ, BroadBandTestConstants.CONSTANT_0));
			listOfSetParameter.add(BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10104_SSID,
					BroadBandTestConstants.STRING_LNF_SSID_5GHZ, BroadBandTestConstants.CONSTANT_0));
			bandResultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
					listOfSetParameter);
			status = bandResultObject.isStatus();
			if (status) {

				listOfSetParameterToApply.add(BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_APPLY_SETTING, BroadBandTestConstants.TRUE,
						BroadBandTestConstants.CONSTANT_3));
				listOfSetParameterToApply.add(BroadBandWebPaUtils.setAndReturnWebPaParameterObject(
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_APPLY_SETTING, BroadBandTestConstants.TRUE,
						BroadBandTestConstants.CONSTANT_3));
				Map<String, String> responseMap = tapEnv.executeMultipleWebPaSetCommands(device,
						listOfSetParameterToApply);
				status = responseMap.containsValue(RDKBTestConstants.SNMP_RESPONSE_SUCCESS);
			}

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully enabled lnf ssid");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S2";
			errorMessage = "Unable to connect to wifi 2.4Ghz";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Connect wifi connected client to lnf 2.4Ghz ssid");
			LOGGER.info(
					"STEP 2: ACTION : Execute : Linux :nmcli d wifi connect <ssid> password <password> iface wlan0Windows:netsh wlan connect name=\"<ssid>\"");
			LOGGER.info("STEP 2: EXPECTED : Successfully connected to wifi  2.4Ghz ssid");
			LOGGER.info("**********************************************************************************");

			bandResultObject = BroadBandConnectedClientUtils.connectClientsToGivenTypeOfWifi(device, tapEnv,
					wifiConnectedClient, WEBPA_AP_INDEXES.PHASE1_LNF, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			status = bandResultObject.isStatus();
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully connected to lnf 2.4Ghz SSID");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			if (status) {
				stepNum = "S3";
				errorMessage = "Unable to validate internet connectivity";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 3: DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
				LOGGER.info(
						"STEP 3: ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v \"www.google.com\"  | grep \"200 OK\" OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep \"200 OK\" OR ping -4 -n 5 google.com ON THE CONNECTED LAN CLIENT");
				LOGGER.info("STEP 3: EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV4");
				LOGGER.info("**********************************************************************************");

				do {
					bandResultObject = BroadBandConnectedClientUtils
							.checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(tapEnv, wifiConnectedClient,
									BroadBandTestConstants.IP_VERSION4);
					status = bandResultObject.isStatus();
					counter++;
				} while (!status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
						BroadBandTestConstants.NINETY_SECOND_IN_MILLIS) && counter < 3);
				if (status) {
					LOGGER.info("STEP 3: ACTUAL : Successfully verified internet connectivity fot IPV4 Interface");
				} else {
					LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			} else {
				stepNum = "S3";
				errorMessage = "Unable to validate internet connectivity";
				status = false;
				LOGGER.info("STEP 3: DESCRIPTION : NOT TESTED BECAUSE FAILED TO CONNECT TO WIFI");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED,
						errorMessage, false);
			}
			stepNum = "S4";
			errorMessage = "Unable to connect to wifi 5Ghz";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Connect wifi connected client to lnf 5Ghz ssid");
			LOGGER.info(
					"STEP 4: ACTION : Execute : Linux :nmcli d wifi connect <ssid> password <password> iface wlan0Windows:netsh wlan connect name=\"<ssid>\"");
			LOGGER.info("STEP 4: EXPECTED : Successfully connected to wifi  5Ghz ssid");
			LOGGER.info("**********************************************************************************");

			bandResultObject = BroadBandConnectedClientUtils.connectClientsToGivenTypeOfWifi(device, tapEnv,
					wifiConnectedClient, WEBPA_AP_INDEXES.PHASE1_LNF, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			status = bandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Successfully connected to lnf 5Ghz SSID");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			if (status) {
				stepNum = "S5";
				errorMessage = "Unable to validate internet connectivity";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 5: DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
				LOGGER.info(
						"STEP 5: ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v \"www.google.com\"  | grep \"200 OK\" OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep \"200 OK\" OR ping -4 -n 5 google.com ON THE CONNECTED LAN CLIENT");
				LOGGER.info("STEP 5: EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV4");
				LOGGER.info("**********************************************************************************");
				counter = 0;
				do {
					bandResultObject = BroadBandConnectedClientUtils
							.checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(tapEnv, wifiConnectedClient,
									BroadBandTestConstants.IP_VERSION4);
					status = bandResultObject.isStatus();
					counter++;
				} while (!status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
						BroadBandTestConstants.NINETY_SECOND_IN_MILLIS) && counter < 3);

				if (status) {
					LOGGER.info("STEP 5: ACTUAL :  Successfully verified internet connectivity fot IPV4 Interface");
				} else {
					LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				stepNum = "S5";
				errorMessage = "Unable to validate internet connectivity";
				status = false;
				LOGGER.info("STEP 5 : NOT TESTED BECAUSE FAILED TO CONNECT TO WIFI");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED,
						errorMessage, false);
			}
			stepNum = "S6";
			errorMessage = "Failed to configure RFC payload for DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BlockLostandFoundInternet.Enable enable/disable";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION :  Configure RFC payload to enable block and lost lnf feature");
			LOGGER.info(
					"STEP 6: ACTION : Copy and update /nvram/rfc.properties with mock RFC config server URL2. Post payload after replacing ESTB mac and enable/disable 3. Reboot device or trigger Configurable RFC check-in");
			LOGGER.info(
					"STEP 6: EXPECTED :  Successfully rebooted or triggered check-in after configuring RFC payload");
			LOGGER.info("**********************************************************************************");

			try {
				jsonObject.put(
						CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.TR181_DOT,
								BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE),
						BroadBandTestConstants.TRUE);
				jsonObject.put(
						CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.TR181_DOT,
								BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_ENABLE),
						BroadBandTestConstants.TRUE);
				jsonObject.put(
						CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.TR181_DOT,
								BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_PREFIX),
						BroadBandTestConstants.TRUE);

			} catch (JSONException e) {
				LOGGER.error("Unable to parse given Json input array");
			}

			if (BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
					payload.replace(BroadBandTestConstants.STRING_REPLACE, jsonObject.toString()))) {
				errorMessage = "Unable to reboot device successfully";
				status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
				enabledByRfc = status;
				if (status) {
					bandResultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
							listOfSetParameter);
					status = bandResultObject.isStatus();
					if (status) {
						Map<String, String> responseMap = tapEnv.executeMultipleWebPaSetCommands(device,
								listOfSetParameterToApply);
						status = responseMap.containsValue(RDKBTestConstants.SNMP_RESPONSE_SUCCESS);
					}
				}
			}

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Rfc settings post and rebooted device successfully");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S7";
			errorMessage = "Unable to validate values in log files";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 7: DESCRIPTION : Verify DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BlockLostandFoundInternet.Enable parameter is updated by RFC");
			LOGGER.info(
					"STEP 7: ACTION : Verify /tmp/rfc_configdata.txt contains posted parameter value2. Verify log message for updation in /rdklogs/logs/dcmrfc.log3. Verify parameter value is changed");
			LOGGER.info("STEP 7: EXPECTED : Successfully validated values in rfc");
			LOGGER.info("**********************************************************************************");

			BroadBandResultObject result = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE, BroadBandTestConstants.TRUE);
			BroadBandResultObject resultObject1 = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device,
					tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_ENABLE,
					BroadBandTestConstants.TRUE);
			BroadBandResultObject resultObject2 = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device,
					tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_PREFIX,
					BroadBandTestConstants.TRUE);
			status = result.isStatus() && resultObject1.isStatus() && resultObject2.isStatus();
			if (!status) {
				status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE,
						BroadBandTestConstants.TRUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
						&& BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_ENABLE,
								BroadBandTestConstants.TRUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
						&& BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE_IPV6_PREFIX,
								BroadBandTestConstants.TRUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
								BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			}
			if (status) {
				LOGGER.info("STEP 7: ACTUAL :Successfully verified block lost and found using webpa");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S8";
			errorMessage = "Unable to connect to wifi 2.4Ghz";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Connect wifi connected client to lnf 2.4Ghz ssid");
			LOGGER.info(
					"STEP 8: ACTION : Execute : Linux :nmcli d wifi connect <ssid> password <password> iface wlan0Windows:netsh wlan connect name=\"<ssid>\"");
			LOGGER.info("STEP 8: EXPECTED : Successfully connected to wifi  2.4Ghz ssid");
			LOGGER.info("**********************************************************************************");

			bandResultObject = BroadBandConnectedClientUtils.connectClientsToGivenTypeOfWifi(device, tapEnv,
					wifiConnectedClient, WEBPA_AP_INDEXES.PHASE1_LNF, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			status = bandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 8: ACTUAL :Successfully connected to lnf 2.4Ghz SSID");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			if (status) {
				stepNum = "S9";
				errorMessage = "Unable to validate internet connectivity";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 9: DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
				LOGGER.info(
						"STEP 9: ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v \"www.google.com\"  | grep \"200 OK\" OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep \"200 OK\" OR ping -4 -n 5 google.com ON THE CONNECTED LAN CLIENT");
				LOGGER.info("STEP 9: EXPECTED : THE INTERNET CONNECTIVITY MUST BE NOT AVAILABLE INTERFACE USING IPV4");
				LOGGER.info("**********************************************************************************");
				counter = 0;
				do {
					bandResultObject = BroadBandConnectedClientUtils
							.checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(tapEnv, wifiConnectedClient,
									BroadBandTestConstants.IP_VERSION4);
					status = !bandResultObject.isStatus();
					counter++;
				} while (!status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
						BroadBandTestConstants.NINETY_SECOND_IN_MILLIS) && counter < 3);
				if (status) {
					LOGGER.info(
							"STEP 9: ACTUAL :  Successfully verified internet connectivity fot IPV4 Interface is not available");
				} else {
					LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				stepNum = "S10";
				errorMessage = "Unable to validate internet connectivity";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 10: DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6 INTERFACE");
				LOGGER.info(
						"STEP 10: ACTION : EXECUTE COMMAND, WINDOWS : curl -6 -v \"www.google.com\"  | grep \"200 OK\" OR ping -6 -n 5 google.com, LINUX : curl -6 -f --interface <interfaceName> www.google.com | grep \"200 OK\" OR ping -6 -n 5 google.com ON THE CONNECTED LAN CLIENT");
				LOGGER.info("STEP 10: EXPECTED : THE INTERNET CONNECTIVITY MUST NOT BE AVAILABLE INTERFACE USING IPV6");
				LOGGER.info("**********************************************************************************");
				counter = 0;
				do {
					bandResultObject = BroadBandConnectedClientUtils
							.checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(tapEnv, wifiConnectedClient,
									BroadBandTestConstants.IP_VERSION6);
					status = !bandResultObject.isStatus();
					counter++;
				} while (!status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
						BroadBandTestConstants.NINETY_SECOND_IN_MILLIS) && counter < 3);
				if (status) {
					LOGGER.info(
							"STEP 10: ACTUAL : Successfully verified internet connectivity fot IPV6 Interface is not available");
				} else {
					LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				stepNum = "S9";
				errorMessage = "Unable to validate internet connectivity";
				status = false;
				LOGGER.info("STEP 9: DESCRIPTION : NOT TESTED BECAUSE FAILED TO CONNECT TO WIFI");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED,
						errorMessage, false);
				stepNum = "S10";
				errorMessage = "Unable to validate internet connectivity";
				status = false;
				LOGGER.info("STEP 10: DESCRIPTION : NOT TESTED BECAUSE FAILED TO CONNECT TO WIFI");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED,
						errorMessage, false);
			}
			stepNum = "S11";
			errorMessage = "Unable to connect to wifi 5Ghz";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Connect wifi connected client to lnf 5Ghz ssid");
			LOGGER.info(
					"STEP 11: ACTION : Execute : Linux :nmcli d wifi connect <ssid> password <password> iface wlan0Windows:netsh wlan connect name=\"<ssid>\"");
			LOGGER.info("STEP 11: EXPECTED : Successfully connected to wifi  5Ghz ssid");
			LOGGER.info("**********************************************************************************");

			bandResultObject = BroadBandConnectedClientUtils.connectClientsToGivenTypeOfWifi(device, tapEnv,
					wifiConnectedClient, WEBPA_AP_INDEXES.PHASE1_LNF, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			status = bandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 11: ACTUAL : Successfully connected to lnf 5Ghz SSID ");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			if (status) {
				stepNum = "S12";
				errorMessage = "Unable to validate internet connectivity";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 12: DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
				LOGGER.info(
						"STEP 12: ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v \"www.google.com\"  | grep \"200 OK\" OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep \"200 OK\" OR ping -4 -n 5 google.com ON THE CONNECTED LAN CLIENT");
				LOGGER.info("STEP 12: EXPECTED : THE INTERNET CONNECTIVITY MUST BE NOT AVAILABLE INTERFACE USING IPV4");
				LOGGER.info("**********************************************************************************");
				counter = 0;
				do {
					bandResultObject = BroadBandConnectedClientUtils
							.checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(tapEnv, wifiConnectedClient,
									BroadBandTestConstants.IP_VERSION4);
					status = !bandResultObject.isStatus();
					counter++;
				} while (!status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
						BroadBandTestConstants.NINETY_SECOND_IN_MILLIS) && counter < 3);
				if (status) {
					LOGGER.info(
							"STEP 12: ACTUAL : Successfully verified internet connectivity for IPV4 Interface is not available");
				} else {
					LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				stepNum = "S13";
				errorMessage = "Unable to validate internet connectivity";
				status = false;

				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 13: DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6 INTERFACE");
				LOGGER.info(
						"STEP 13: ACTION : EXECUTE COMMAND, WINDOWS : curl -6 -v \"www.google.com\"  | grep \"200 OK\" OR ping -6 -n 5 google.com, LINUX : curl -6 -f --interface <interfaceName> www.google.com | grep \"200 OK\" OR ping -6 -n 5 google.com ON THE CONNECTED LAN CLIENT");
				LOGGER.info("STEP 13: EXPECTED : THE INTERNET CONNECTIVITY MUST NOT BE AVAILABLE INTERFACE USING IPV6");
				LOGGER.info("**********************************************************************************");
				counter = 0;
				do {
					bandResultObject = BroadBandConnectedClientUtils
							.checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(tapEnv, wifiConnectedClient,
									BroadBandTestConstants.IP_VERSION6);
					status = !bandResultObject.isStatus();
					counter++;
				} while (!status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
						BroadBandTestConstants.NINETY_SECOND_IN_MILLIS) && counter < 3);
				if (status) {
					LOGGER.info(
							"STEP 13: ACTUAL : Successfully verified internet connectivity fot IPV6 Interface is not available");
				} else {
					LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				stepNum = "S12";
				errorMessage = "Unable to validate internet connectivity";
				status = false;
				LOGGER.info("STEP 12: DESCRIPTION : NOT TESTED BECAUSE FAILED TO CONNECT TO WIFI");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED,
						errorMessage, false);
				stepNum = "S13";
				errorMessage = "Unable to validate internet connectivity";
				status = false;
				LOGGER.info("STEP 13: DESCRIPTION : NOT TESTED BECAUSE FAILED TO CONNECT TO WIFI");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED,
						errorMessage, false);
			}
			/**
			 * Step 14- Step 16 Helper method to check wifi 2.4Ghz and internet connectivity
			 */
			validateGivenWifiWithInternetConnectivity(device, tapEnv, testCaseId, wifiConnectedClient,
					WEBPA_AP_INDEXES.PRIVATE_WIFI, WiFiFrequencyBand.WIFI_BAND_2_GHZ,
					BroadBandTestConstants.PRIVATE_WIFI_TYPE, BroadBandTestConstants.CONSTANT_14);
			/**
			 * Step 17- Step 19 Helper method to check wifi 2.4Ghz and internet connectivity
			 */
			validateGivenWifiWithInternetConnectivity(device, tapEnv, testCaseId, wifiConnectedClient,
					WEBPA_AP_INDEXES.PRIVATE_WIFI, WiFiFrequencyBand.WIFI_BAND_5_GHZ,
					BroadBandTestConstants.PRIVATE_WIFI_TYPE, BroadBandTestConstants.CONSTANT_17);

			stepNum = "S20";
			errorMessage = "Unable to reboot device";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 20: DESCRIPTION : Reboot device and validate online status");
			LOGGER.info("STEP 20: ACTION : Execute : reboot");
			LOGGER.info("STEP 20: EXPECTED : Successfully rebooted device and online status");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				revertedRfc = status;
				LOGGER.info("STEP 20: ACTUAL : Successfully rebooted device and validated device status after reboot");
			} else {
				LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S21";
			errorMessage = "Unable to validate status using webpa";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 21: DESCRIPTION : Validate Block and lost lnf status to be same using webpa");
			LOGGER.info(
					"STEP 21: ACTION : Execute webpa :Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BlockLostandFoundInternet.Enable Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.IPv6subPrefix.EnableDevice.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.IPv6onLnF.Enable ");
			LOGGER.info("STEP 21: EXPECTED : Successfully validated status using webpa");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 21: ACTUAL :Successfully verified block lnf after reboot is still enabled");
			} else {
				LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			stepNum = "S22";
			errorMessage = "Unable to factory reset device using webpa";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 22: DESCRIPTION : Factory reset device using webpa");
			LOGGER.info(
					"STEP 22: ACTION : Execute webpa set :Device.X_CISCO_COM_DeviceControl.FactoryReset string \"Router,Wifi,VoIP,Dect,MoCA\"");
			LOGGER.info("STEP 22: EXPECTED : Successfully factory reset device using webpa");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);

			if (status) {
				factoryReset = status;
				LOGGER.info("STEP 22: ACTUAL : Successfully completed factory reset and validate device online status");
			} else {
				LOGGER.error("STEP 22: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S23";
			errorMessage = "Unable to validate status using webpa";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 23: DESCRIPTION : Validate Block and lost lnf status to be default using webpa");
			LOGGER.info(
					"STEP 23: ACTION : Execute webpa :Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.BlockLostandFoundInternet.Enable Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.IPv6subPrefix.EnableDevice.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.IPv6onLnF.Enable ");
			LOGGER.info("STEP 23: EXPECTED : Successfully validated status using webpa");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_LOST_N_FOUND_INTERNET_ENABLE, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 23: ACTUAL : Successfully verified block lnf after factory reset is disabled");
			} else {
				LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * POST-CONDITION : BEGIN BROAD BAND DEVICE REACTIVATION
			 */
			BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, !factoryReset, 1);

			if (!revertedRfc && (enabledByRfc || disabledByRfc)) {
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION 2 : DESCRIPTION : DELETE RFC PROPERTIES FILE FROM nvram");
				LOGGER.info("POST-CONDITION 2 : ACTION : EXECUTE rm rfc.properties FROM NVRAM FOLDER ");
				LOGGER.info("POST-CONDITION 2 : EXPECTED : SUCCESSFULLY DETELED FILE FROM nvram");
				LOGGER.info("#######################################################################################");
				status = BroadBandRfcFeatureControlUtils.removeNvramOverrideForRfc(device, tapEnv)
						&& (HttpStatus.SC_OK == BroadBandRfcFeatureControlUtils
								.clearSettingsInProxyXconfDcmServerForRDKB(device, tapEnv, false, "EncryptCloudData"));

				// (HttpStatus.SC_OK == CommonMethods.clearSettingsInProxyXconfDcmServer(device,
				// tapEnv, false,
				// "EncryptCloudData"));

				if (status) {
					LOGGER.info("POST-CONDITION 2: ACTUAL : Successfully deleted file from nvran");
				} else {
					LOGGER.error("POST-CONDITION 2: ACTUAL : Unable to delete file from nvram");
				}
			}

			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-LNF-NTWK-1001");
	}
	
    /**
     * Helper method to validate given wifi status
     * 
     * @param device
     *            Dut instance
     * @param tapEnv
     *            AutomaticsTapApi instance
     * @param testCaseId
     *            String testcaseid
     * @param wifiConnectedClient
     *            connected client
     * @param webpaIndex
     *            String WEBPA_AP_INDEXES
     * @param frequencyBand
     *            String WiFiFrequencyBand
     * @param wifiType
     *            String wifitype
     * @param stepNumber
     *            Integer step num
     */
    public void validateGivenWifiWithInternetConnectivity(Dut device, AutomaticsTapApi tapEnv, String testCaseId,
	    Dut wifiConnectedClient, WEBPA_AP_INDEXES webpaIndex, WiFiFrequencyBand frequencyBand, String wifiType,
	    Integer stepNumber) {
	int counter = 0;
	String stepNum = "S" + stepNumber;
	String errorMessage = "Unable to connect to wifi " + frequencyBand;
	boolean status = false;

	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + " DESCRIPTION : Connect wifi connected client to " + wifiType + " "
		+ frequencyBand + " ssid");
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION : Execute : Linux :nmcli d wifi connect <ssid> password <password> iface wlan0Windows:netsh wlan connect name=\"<ssid>\"");
	LOGGER.info("STEP " + stepNumber + ": EXPECTED : Successfully connected to wifi  " + wifiType + " "
		+ frequencyBand + " ssid");
	LOGGER.info("**********************************************************************************");

	BroadBandResultObject bandResultObject = BroadBandConnectedClientUtils.connectClientsToGivenTypeOfWifi(device,
		tapEnv, wifiConnectedClient, webpaIndex, frequencyBand);
	status = bandResultObject.isStatus();

	if (status) {
	    LOGGER.info(
		    "STEP " + stepNumber + ": ACTUAL : Successfully connected to " + wifiType + " " + frequencyBand);
	} else {
	    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	if (status) {
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    errorMessage = "Unable to validate internet connectivity";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV4 INTERFACE");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v \"www.google.com\"  | grep \"200 OK\" OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep \"200 OK\" OR ping -4 -n 5 google.com ON THE CONNECTED LAN CLIENT");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV4");
	    LOGGER.info("**********************************************************************************");
	    do {
		bandResultObject = BroadBandConnectedClientUtils.checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(
			tapEnv, wifiConnectedClient, BroadBandTestConstants.IP_VERSION4);
		status = bandResultObject.isStatus();
		counter++;
	    } while (!status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.NINETY_SECOND_IN_MILLIS)
		    && counter < 3);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully verified ipv4 connectivity");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    if (DeviceModeHandler.isFibreDevice(device)) {
		stepNumber++;
		stepNum = "S" + stepNumber;
		errorMessage = "Unable to validate internet connectivity";
		status = false;
		LOGGER.info(
			"STEP " + stepNumber + ": DESCRIPTION : NOT TESTED BECAUSE PROD FIBRE IPV6 NOT SUPPORTED");
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED,
			errorMessage, false);
	    } else {
		stepNumber++;
		stepNum = "S" + stepNumber;
		errorMessage = "Unable to validate internet connectivity";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
			+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CONNECTED WIFI CLIENT USING IPV6 INTERFACE");
		LOGGER.info("STEP " + stepNumber
			+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -6 -v \"www.google.com\"  | grep \"200 OK\" OR ping -6 -n 5 google.com, LINUX : curl -6 -f --interface <interfaceName> www.google.com | grep \"200 OK\" OR ping -6 -n 5 google.com ON THE CONNECTED LAN CLIENT");
		LOGGER.info("STEP " + stepNumber
			+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV6");
		LOGGER.info("**********************************************************************************");
		counter = 0;
		do {
		    bandResultObject = BroadBandConnectedClientUtils
			    .checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(tapEnv, wifiConnectedClient,
				    BroadBandTestConstants.IP_VERSION6);
		    status = bandResultObject.isStatus();
		    counter++;
		} while (!status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
			BroadBandTestConstants.NINETY_SECOND_IN_MILLIS) && counter < 3);
		if (status) {
		    LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully verified ipv6 internet connectivity");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }
	} else {
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    errorMessage = "Unable to validate internet connectivity";
	    status = false;
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : NOT TESTED BECAUSE FAILED TO CONNECT TO WIFI");
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED, errorMessage,
		    false);
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    errorMessage = "Unable to validate internet connectivity";
	    status = false;
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : NOT TESTED BECAUSE FAILED TO CONNECT TO WIFI");
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_TESTED, errorMessage,
		    false);
	}
    }
    
    /**
     * Method to verify Logging all occurrences of connections to LNF PSK SSIDs
     * 
     * <ol>
     * <li>Verify connecting any client to 2.4GHz or 5GHz WiFi.</li>
     * <li>Verify Ipv4 Address of the connected client.</li>
     * <li>Verify Ipv6 Address of the connected client.</li>
     * <li>Verify the Internet connectivity in the connected wifi client.</li>
     * <li>Verify Enabling lnf ssid and broadcast using Webpa/dmcli.</li>
     * <li>Verify connecting 2.4GHz WiFi connected client to lnf 2.4GHz ssid.</li>
     * <li>Verify the Internet connectivity in the connected wifi client.</li>
     * <li>Verify expected log message in /rdklogs/logs/WiFilog.txt.0 after connecting to lnf 2.4GHz ssid.</li>
     * <li>Verify connecting 5GHz WiFi connected client to lnf 5GHz ssid.</li>
     * <li>Verify the Internet connectivity in the connected wifi client.</li>
     * <li>Verify expected log message in /rdklogs/logs/WiFilog.txt.0 after connecting to lnf 5GHz ssid.</li>
     * <li>Verify video client connectivity with Lnf SSIDs.</li>
     * <li>POST-CONDITION 1: Verify disabling Lnf SSID.</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * 
     * @author prashant.mishra12
     * @refactor Athira
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-WIFI-CC-WIN-5004")
    public void testToVerifyLoggingOccurrencesOfConnectionsToLNfSsids(Dut device) {
	// Variable declaration starts
	String testCaseId = "TC-RDKB-WIFI-CC-WIN-004";
	String stepNum = "s1";
	String errorMessage = "";
	String response = "";
	String response1 = "";
	String response2 = "";
	boolean status = false;
	boolean isLnfSsidEnabled = false;
	int counter = 0;
	Dut deviceConnected = null;
	Dut videoDeviceForTest = null;
	BroadBandResultObject result = null;
	BroadBandResultObject bandResultObject;
	// Variable declaration ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CC-WIN-5004");
	LOGGER.info("TEST DESCRIPTION: Test to Verify Logging all occurrences of connections to LNF PSK SSIDs");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify connecting any client to 2.4GHz or 5GHz WiFi.");
	LOGGER.info("2. Verify Ipv4 Address of the connected client.");
	LOGGER.info("3. Verify Ipv6 Address of the connected client.");
	LOGGER.info("4. Verify the internet connectivity in the connected wifi client.");
	LOGGER.info("5. Verify Enabling lnf ssid and broadcast using Webpa/dmcli.");
	LOGGER.info("6. Verify connecting 2.4GHz WiFi connected client to lnf 2.4Ghz ssid.");
	LOGGER.info("7. Verify the internet connectivity in the connected wifi client.");
	LOGGER.info(
		"8. Verify expected log message in /rdklogs/logs/WiFilog.txt.0 after connecting to lnf 2.4Ghz ssid.");
	LOGGER.info("9. Verify connecting 5GHz WiFi connected client to lnf 5Ghz ssid.");
	LOGGER.info("10. Verify the internet connectivity in the connected wifi client.");
	LOGGER.info(
		"11. Verify expected log message in /rdklogs/logs/WiFilog.txt.0 after connecting to lnf 5Ghz ssid.");
	LOGGER.info("POST-CONDITION 1: Verify disabling Lnf SSID.");
	LOGGER.info("#######################################################################################");

	try {
	    errorMessage = "Unable to connect any of the associated client to WiFi.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify connecting any client to 2.4GHz or 5GHz WiFi.");
	    LOGGER.info("STEP 1: ACTION : Connect any associated client to 2.4GHz or 5Ghz WiFi.");
	    LOGGER.info("STEP 1: EXPECTED : One of the associated client must be connected to WiFi successfully.");
	    LOGGER.info("**********************************************************************************");
	    deviceConnected = BroadBandConnectedClientUtils
		    .get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
	    status = (null != deviceConnected);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : One of the associated client is connected to WiFi successfully.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s2";
	    errorMessage = "Unable to get correct Ipv4 Address for connected client.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify Ipv4 Address of the connected client.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute following command in connected client : ipconfig |grep -A 10 'Wireless adapter Wi-Fi' |grep -i 'IPv4 Address'");
	    LOGGER.info("STEP 2: EXPECTED : It should return the correct Ipv4 Address for connected client.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
		    ((Device) deviceConnected).getOsType(), deviceConnected, tapEnv);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Ipv4 Address of the connected client is verified successfully.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s3";
	    errorMessage = "Unable to get correct Ipv6 Address for connected client.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify Ipv6 Address of the connected client.");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute following command in connected client : ipconfig |grep -A 10 'Wireless adapter Wi-Fi' |grep -i 'IPv6 Address'");
	    LOGGER.info("STEP 3: EXPECTED : It should return the correct Ipv6 Address for connected client.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
		    ((Device) deviceConnected).getOsType(), deviceConnected, tapEnv);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Ipv6 Address of the connected client is verified successfully.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s4";
	    errorMessage = "Unable to verify internet connectivity in the connected wifi client.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify the internet connectivity in the connected wifi client.");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute following command in connected client : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com");
	    LOGGER.info("STEP 4: EXPECTED : Internet connectivity must be available in connected client.");
	    LOGGER.info("**********************************************************************************");
	    long startTime = System.currentTimeMillis();
	    do {
		result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
			deviceConnected,
			BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
			BroadBandTestConstants.IP_VERSION4);
		status = result.isStatus();
		errorMessage = result.getErrorMessage();
	    } while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Internet connectivity is verified successfully in connected client.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s5";
	    errorMessage = "Failed to enable lnf SSIDs using Webpa.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify Enabling lnf ssid and broadcast using Webpa/dmcli.");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute Webpa Set Command for following params: A)Device.WiFi.SSID.{i}.Enable B)Device.WiFi.AccessPoint.{i}.SSIDAdvertisementEnabled and set value as true.");
	    LOGGER.info("STEP 5: EXPECTED : Lnf should be enabled successfully using Webpa/dmcli.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandConnectedClientUtils.enableDisableAndBroadcastLnfSsid(device, tapEnv,
		    BroadBandTestConstants.TRUE);
	    if (status) {
		isLnfSsidEnabled = true;
		LOGGER.info("STEP 5: ACTUAL : Lnf SSID enabled and broadcasted successfully using Webpa/dmcli");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s6";
	    errorMessage = "Unable to connect to lnf 2.4Ghz ssid.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify connecting 2.4GHz WiFi connected client to lnf 2.4Ghz ssid.");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the following command in connected client: nmcli d wifi connect <ssid> password <password> iface wlan0Windows:netsh wlan connect name=\"<ssid>\"");
	    LOGGER.info("STEP 6: EXPECTED : Successfully connected to lnf 2.4Ghz ssid.");
	    LOGGER.info("**********************************************************************************");
	    String currentTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    // Clearing WiFilog.txt.0 file
	    CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0);
	    bandResultObject = BroadBandConnectedClientUtils.connectClientsToGivenTypeOfWifi(device, tapEnv,
		    deviceConnected, WEBPA_AP_INDEXES.PHASE1_LNF, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    status = bandResultObject.isStatus();
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Client connected successfully to lnf 2.4Ghz ssid.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    if (status) {
		stepNum = "s7";
		errorMessage = "Unable to verify internet connectivity in the connected wifi client.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 7: DESCRIPTION : Verify the internet connectivity in the connected wifi client.");
		LOGGER.info(
			"STEP 7: ACTION : Execute following command in connected client : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com");
		LOGGER.info("STEP 7: EXPECTED : Internet connectivity must be available in connected client.");
		LOGGER.info("**********************************************************************************");
		do {
		    bandResultObject = BroadBandConnectedClientUtils
			    .checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(tapEnv, deviceConnected,
				    BroadBandTestConstants.IP_VERSION4);
		    status = bandResultObject.isStatus();
		    counter++;
		} while (!status
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.NINETY_SECOND_IN_MILLIS)
			&& counter < BroadBandTestConstants.CONSTANT_3);
		if (status) {
		    LOGGER.info(
			    "STEP 7: ACTUAL : Internet connectivity is verified successfully in connected client after connecting to lnf 2.4Ghz ssid.");
		} else {
		    LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		stepNum = "s8";
		errorMessage = "Expected log message is not present in /rdklogs/logs/WiFilog.txt.0.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 8: DESCRIPTION : Verify expected log message in /rdklogs/logs/WiFilog.txt.0 after connecting to lnf 2.4Ghz ssid.");
		LOGGER.info(
			"STEP 8: ACTION : SSH the device and execute the following command: cat /rdklogs/logs/WiFilog.txt.0 | grep RDKB_WIFI & verify expected log message.");
		LOGGER.info(
			"STEP 8: EXPECTED : Expected log message 'RDKB_WIFI_NOTIFY: connectedTo:2.4_LNF_PSK_SSID clientMac:<Connected Client MAC address>' should present in /rdklogs/logs/WiFilog.txt.0.");
		LOGGER.info("**********************************************************************************");
		boolean isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
		String connectedDeviceMacAddress = BroadBandParentalControlUtils.getConnectedClientMACAddress(tapEnv,
			deviceConnected, BroadBandTestConstants.CLIENT_TYPE_WIFI);
		String expectedLogMessage1 = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandTraceConstants.RDKB_WIFI_NOTIFICATION_MESSAGE_1.replace(
				BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_WIFI_BAND_2_4),
			connectedDeviceMacAddress);
		String expectedLogMessage2 = BroadBandCommonUtils.concatStringUsingStringBuffer(
				BroadBandTraceConstants.RDKB_WIFI_NOTIFICATION_MESSAGE_2.replace(
					BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_WIFI_BAND_2_4),
				connectedDeviceMacAddress);
		AutomaticsUtils.sleep(AutomaticsConstants.FIVE_MINUTES);
		startTime = System.currentTimeMillis();
		do {
		    response1 = LoggerUtils.getLatestLogMessageBasedOnTimeStamp(tapEnv, device, expectedLogMessage1,
			    BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0, currentTimeStamp,
			    isAtomSyncAvailable);
		    response2 = LoggerUtils.getLatestLogMessageBasedOnTimeStamp(tapEnv, device, expectedLogMessage2,
				    BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0, currentTimeStamp,
				    isAtomSyncAvailable);
		    status = (CommonMethods.isNotNull(response1) || CommonMethods.isNotNull(response2));
		} while (!status
			&& ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS)
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.NINETY_SECOND_IN_MILLIS));
		AutomaticsUtils.sleep(AutomaticsConstants.FIVE_MINUTES);
		if (!status)
			startTime = System.currentTimeMillis();
		do {
		    response1 = LoggerUtils.getLatestLogMessageBasedOnTimeStamp(tapEnv, device, expectedLogMessage1,
			    BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0, currentTimeStamp,
			    isAtomSyncAvailable);
		    
		    response2 = LoggerUtils.getLatestLogMessageBasedOnTimeStamp(tapEnv, device, expectedLogMessage2,
				    BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0, currentTimeStamp,
				    isAtomSyncAvailable);
		    status = (CommonMethods.isNotNull(response1) || CommonMethods.isNotNull(response2));
		} while (!status
			&& ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS)
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.NINETY_SECOND_IN_MILLIS));	
			
			
		if (status) {
		    LOGGER.info(
			    "STEP 8: ACTUAL : Expected log message verified successfully in /rdklogs/logs/WiFilog.txt.0 after connecting to lnf 2.4Ghz ssid.");
		} else {
		    LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    else {
		LOGGER.info("Unable to connect 2.4GHz Lnf SSID so skipping Step S7 & S8.");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s7", ExecutionStatus.NOT_TESTED, errorMessage,
			false);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s8", ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	    stepNum = "s9";
	    errorMessage = "Unable to connect to lnf 5Ghz ssid.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify connecting 5GHz WiFi connected client to lnf 5Ghz ssid.");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the following command in connected client: nmcli d wifi connect <ssid> password <password> iface wlan0Windows:netsh wlan connect name=\"<ssid>\"");
	    LOGGER.info("STEP 9: EXPECTED : Successfully connected to lnf 5Ghz ssid.");
	    LOGGER.info("**********************************************************************************");
	    bandResultObject = BroadBandConnectedClientUtils.connectClientsToGivenTypeOfWifi(device, tapEnv,
		    deviceConnected, WEBPA_AP_INDEXES.PHASE1_LNF, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
	    status = bandResultObject.isStatus();
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Client connected successfully to lnf 5Ghz ssid.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    if (status) {
		stepNum = "s10";
		errorMessage = "Unable to verify internet connectivity in the connected wifi client.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 10: DESCRIPTION : Verify the internet connectivity in the connected wifi client.");
		LOGGER.info(
			"STEP 10: ACTION : Execute following command in connected client : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com");
		LOGGER.info("STEP 10: EXPECTED : Internet connectivity must be available in connected client.");
		LOGGER.info("**********************************************************************************");
		counter = 0;
		do {
		    bandResultObject = BroadBandConnectedClientUtils
			    .checkInternetConnectivityUsingCurlOrPingforIpv4OrIpv6(tapEnv, deviceConnected,
				    BroadBandTestConstants.IP_VERSION4);
		    status = bandResultObject.isStatus();
		    counter++;
		} while (!status
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.NINETY_SECOND_IN_MILLIS)
			&& counter < BroadBandTestConstants.CONSTANT_3);
		if (status) {
		    LOGGER.info(
			    "STEP 10: ACTUAL : Internet connectivity is verified successfully in connected client after connecting to lnf 2.4Ghz ssid.");
		} else {
		    LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		
		stepNum = "s11";
		errorMessage = "Expected log message is not present in /rdklogs/logs/WiFilog.txt.0.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 11: DESCRIPTION : Verify expected log message in /rdklogs/logs/WiFilog.txt.0 after connecting to lnf 5Ghz ssid.");
		LOGGER.info(
			"STEP 11: ACTION : SSH the device and execute the following command: cat /rdklogs/logs/WiFilog.txt.0 | grep RDKB_WIFI & verify expected log message.");
		LOGGER.info(
			"STEP 11: EXPECTED : Expected log message 'RDKB_WIFI_NOTIFY: connectedTo:5_LNF_PSK_SSID clientMac:<Connected Client MAC address>' should present in /rdklogs/logs/WiFilog.txt.0.");
		LOGGER.info("**********************************************************************************");
		String connectedDeviceMacAddress = BroadBandParentalControlUtils.getConnectedClientMACAddress(tapEnv,
			deviceConnected, BroadBandTestConstants.CLIENT_TYPE_WIFI);
		String expectedLogMessage1 = BroadBandTraceConstants.RDKB_WIFI_NOTIFICATION_MESSAGE_1
			.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.WIFI_BAND_5_0)
			+ connectedDeviceMacAddress;
		String expectedLogMessage2 = BroadBandTraceConstants.RDKB_WIFI_NOTIFICATION_MESSAGE_2
				.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.WIFI_BAND_5_0)
				+ connectedDeviceMacAddress;

		AutomaticsUtils.sleep(AutomaticsConstants.FIVE_MINUTES);
		response1 = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, expectedLogMessage1,
			BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0,
			BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		
		response2 = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, expectedLogMessage2,
				BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0,
				BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		status = (CommonMethods.isNotNull(response1) || CommonMethods.isNotNull(response2));
		if(!status)
		{
			response1 = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, expectedLogMessage1,
					BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0,
					BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			response2 = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, expectedLogMessage2,
					BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0,
					BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = (CommonMethods.isNotNull(response1) || CommonMethods.isNotNull(response2));
		}
		
		if(!status)
		{
		boolean isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
		AutomaticsUtils.sleep(AutomaticsConstants.FIVE_MINUTES);
		startTime = System.currentTimeMillis();
		do {
			response1 = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, expectedLogMessage1,
					BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0,
					BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			response2 = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, expectedLogMessage2,
					BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0,
					BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = (CommonMethods.isNotNull(response1) || CommonMethods.isNotNull(response2));
		} while (!status
			&& ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS)
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.NINETY_SECOND_IN_MILLIS));
		AutomaticsUtils.sleep(AutomaticsConstants.FIVE_MINUTES);
		if (!status)
			startTime = System.currentTimeMillis();
		do {
			response1 = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, expectedLogMessage1,
					BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0,
					BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			response2 = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, expectedLogMessage2,
					BroadBandCommandConstants.LOCATION_FILE_WIFI_LOG_TXT_0,
					BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = (CommonMethods.isNotNull(response1) || CommonMethods.isNotNull(response2));
		} while (!status
			&& ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS)
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.NINETY_SECOND_IN_MILLIS));	
		}				

		if (status) {
		    LOGGER.info(
			    "STEP 11: ACTUAL : Expected log message verified successfully in /rdklogs/logs/WiFilog.txt.0 after connecting to lnf 5Ghz ssid.");
		} else {
		    LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    } else {
		LOGGER.error("Unable to connect to 5GHz Lnf SSID so skipping S10 & S11.");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "S10", ExecutionStatus.NOT_TESTED, errorMessage,
			false);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "S11", ExecutionStatus.NOT_TESTED, errorMessage,
			false);
	    }

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    if (isLnfSsidEnabled) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");

		status = false;
		errorMessage = "Failed to disable Lnf SSIDs.";
		LOGGER.info("#######################################################################################");
		LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Verify disabling Lnf SSID.");
		LOGGER.info(
			"POST-CONDITION 1 : ACTION : Execute Webpa Set Command for following params: A)Device.WiFi.SSID.{i}.Enable B)Device.WiFi.AccessPoint.{i}.SSIDAdvertisementEnabled and set value as false.");
		LOGGER.info("POST-CONDITION 1 : EXPECTED : Lnf should be disabled successfully using Webpa/dmcli.");
		LOGGER.info("#######################################################################################");
		try {
		    status = BroadBandConnectedClientUtils.enableDisableAndBroadcastLnfSsid(device, tapEnv,
			    BroadBandTestConstants.FALSE);
		} catch (Exception exception) {
		    LOGGER.error(exception.getMessage() + errorMessage);
		}
		if (status) {
		    LOGGER.info("POST-CONDITION 1 : ACTUAL : Lnf SSIDs disabled successfully.");
		} else {
		    LOGGER.error("POST-CONDITION 1 : ACTUAL : " + errorMessage);
		}
		LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CC-WIN-5004");
    }

}