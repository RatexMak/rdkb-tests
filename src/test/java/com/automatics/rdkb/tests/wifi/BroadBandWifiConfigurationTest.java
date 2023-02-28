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
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.constants.LinuxCommandConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils.WifiOperatingStandard;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;

public class BroadBandWifiConfigurationTest extends AutomaticsTestBase {

	/** Constant holds the test step number **/
	private static int stepNumber = 0;
	/** Constant holds the test pre condition step number **/
	private static int preConStepNumber = 0;
	/** Constant holds the test post condition step number **/
	private static int postConStepNumber = 0;
	/** Constant holds the default operating standard bandWidth for 2.4 Ghz **/
	private static String defaultOperStandardBandWidth = null;
	/** Constant holds the default operating standard for 2.4 Ghz **/
	private static String defaultOperStandard_2_4Ghz = null;
	/** Constant holds the default operating standard for 5 Ghz **/
	private static String defaultOperStandard_5Ghz = null;
	/** Constant holds the Error Message **/
	private static String errorMessage = null;
	/** Constant holds the test step status **/
	private static boolean status = false;
	/** Constant holds the bandwidth set status 2.4 Ghz **/
	private static boolean bandwidthSetStatus = false;
	/** Constant holds the default channel value for 2.4 Ghz **/
	private static int defaultChannel_2_4Ghz = 0;
	/** Constant holds the default channel value for 5 Ghz **/
	private static int defaultChannel_5Ghz = 0;
	/** Variable holds the device reactivated status **/
	private static boolean isReactivated = false;
	/** Constant holds the test step number with S **/
	private static String stepNum = "";
	/** Constant holds the auto channles staus for 2.4 Ghz **/
	private static boolean isAutoChnl2Ghz = false;

	/** Constant holds the auto channles staus for 5 Ghz **/
	private static boolean isAutoChnl5Ghz = false;

	/**
	 * Test Case : Verify that the Preshared key of WIFI access point can be updated
	 * & read by admin for 2.4 Ghz
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Verify 2.4 GHz SSID is enabled</li>
	 * <li>Step 1 : Get and verify the default passphrase for 2.4 GHz SSID using
	 * webpa</li>
	 * <li>Step 2 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 3 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 4 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 5 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 6 : Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 7 : Change the passphrase for 2.4GHz ssid using webpa param</li>
	 * <li>Step 8 : Verify the state of the 2.4 GHz ssid in connected client</li>
	 * <li>Step 9 : Connect the client to 2.4 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>Step 10 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 11 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 12 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 13 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 14 : Verify disconnecting the 2.4 GHz private wifi SSID</li>
	 * </ol>
	 * 
	 * @author Muthukumar
	 * @refactor Athira
	 * 
	 * @param device instance of {@link Dut}
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-WIFI-KEY-PHARSE-5001")
	public void testToVerifyClientConnectivity2_4GhzWifiPassphraseChange(Dut device) {
		String testId = "TC-RDKB-WIFI-KEY-PHARSE-501";
		stepNumber = 1;
		preConStepNumber = 1;
		postConStepNumber = 1;
		String testStepNumber = "S" + stepNumber;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWith2Ghz = null;
		String defaultPasswordFromWebPa = null;
		String defaultPasswordFromSnmp = null;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-KEY-PHARSE-5001");
			LOGGER.info(
					"TEST DESCRIPTION: Verify that the Preshared key of WIFI access point can be updated & read by admin for 2.4 Ghz.");

			LOGGER.info("TEST STEPS : ");
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: ");
			LOGGER.info(" 1 : Get and verify the default passphrase for 2.4 GHz SSID using webpa");
			LOGGER.info(
					" 2 : Connect  the connected client  in the setup to 2.4 GHz SSID and verify connection status");
			LOGGER.info(" 3 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID");
			LOGGER.info(" 4 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID");
			LOGGER.info(
					" 5 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 6 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 7 : Change the passphrase for 2.4GHz ssid using webpa param ");
			LOGGER.info(" 8 : Verify the state of the 2.4 GHz ssid in connected client");
			LOGGER.info(" 9 : Connect the client to 2.4 GHz Private Wi-Fi Network and verify connection status ");
			LOGGER.info(" 10 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID");
			LOGGER.info(" 11 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID");
			LOGGER.info(
					" 12 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 13 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 14 : Verify disconnecting the 2.4 GHz private wifi SSID");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : DESCRIPTION : VERIFY AND ENABLE 2.4GHZ PRIVATE SSID USING WEBPA");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTION :  VERIFY AND ENABLE 2.4GHZ PRIVATE SSID USING WEBPA PARAM 'Device.WiFi.SSID.10001.Enable'");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : EXPECTED : 2.4GHZ PRIVATE SSID SHOULD BE ENABLED SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			try {
				errorMessage = "2.4GHZ SSID IS IN DISABLED STATE";
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						BroadBandTestConstants.TRUE);
			} catch (TestException exp) {
				errorMessage = "FAILED TO ENABLE 2.4GHZ SSID USING WEBPA PARAM 'Device.WiFi.SSID.10001.Enable'";
				// Enable 2.4GHz Radio
				status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
						WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, true);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION " + preConStepNumber
						+ " : ACTUAL : 2.4GHZ PRIVATE SSID ENABLED IS ENABLED SUCCESSFULLY");
			} else {
				LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION " + preConStepNumber
						+ " FAILED : " + errorMessage);
			}
			LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1 : GET AND VERIFY DEFAULT PASSPHRASE FOR 2.4Ghz SSID USING WEBPA PARAM
			 */
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION :  GET AND VERIFY DEFAULT PASSPHRASE FOR 2.4Ghz SSID USING WEBPA PARAM");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION :  GET DEFAULT PASSPHRASE FOR 2.4Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.Security.X_COMCAST-COM_KeyPassphrase'");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED : DEFAULT PASSPHRASE FOR 2.4Ghz SSID SHOULD BE OBTAINED SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO GET DEFAULT PASSPHRASE FOR 2.4Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.Security.X_COMCAST-COM_KeyPassphrase'";

			try {

				defaultPasswordFromWebPa = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(
						device, tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);

				if (!DeviceModeHandler.isDSLDevice(device)) {

					defaultPasswordFromSnmp = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv,
							device, BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_PASSPHRASE.getOid(),
							BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_PASSPHRASE.getTableIndex());
					status = defaultPasswordFromWebPa.equalsIgnoreCase(defaultPasswordFromSnmp);

				} else {
					status = CommonMethods.isNotNull(defaultPasswordFromWebPa);
				}

			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL : OBTAINED AND VERIFIED THE DEFAULT PASSPHRASE FOR 2.4Ghz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2 : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID
			 */

			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 2.4GHz SSID";
			try {
				deviceConnectedWith2Ghz = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWith2Ghz != null);
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 2.4GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * SETP 3 - 6
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					stepNumber);

			/**
			 * STEP 7 : CHANGE THE PASSPHRASE FOR 2.4GHZ SSID USING WEBPA PARAM
			 */
			stepNumber = 7;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION :  CHANGE THE PASSPHRASE FOR 2.4GHZ SSID USING WEBPA PARAM");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION :  SET AND VERIFY THE PASSPHRASE FOR 2.4Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.Security.X_COMCAST-COM_KeyPassphrase'");
			LOGGER.info(
					"STEP :  " + stepNumber + " : EXPECTED: PASSPHRASE FOR 2.4Ghz SSID SHOULD BE CHANGED SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO SET PASSPHRASE FOR 2.4Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.Security.X_COMCAST-COM_KeyPassphrase'";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_2GHZ_PASSPHRASE,
					WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.TEST_SSID_PASSWORD);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL : CHANGED THE PASSPHRASE FOR 2.4GHZ SSID USING WEBPA PARAM SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 8 : VERIFY THE WIFI CONNECTION STATE OF THE 2.4GHZ CONNECTED CLIENT
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY THE WIFI CONNECTION STATE OF THE 2.4GHZ CONNECTED CLIENT");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : LINUX - nmcli | grep connected |grep -i wlan0 AND WINDOWS - netsh wlan show interface | grep -i 'state'");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED : CLIENT WIFI CONNECTION STATE SHOULD BE DISCONNECTED");
			LOGGER.info("#######################################################################################");
			errorMessage = "CLIENT DIDN'T GET DISCONNECTED FROM 2.4GHZ SSID, EVEN AFTER CHANGING THE PASSPHRASE";
			tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			status = ConnectedNattedClientsUtils.verifyWifiConnectionStatus(deviceConnectedWith2Ghz, tapEnv, null,
					false);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL : CLIENT WIFI CONNECTION STATE IS DISCONNECTED AS EXPECTED");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 9 : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED : THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 2.4GHz SSID";
			try {
				deviceConnectedWith2Ghz = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWith2Ghz != null);
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 2.4GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			/**
			 * SETP 10 - 13
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					stepNumber);
			/**
			 * Step 14: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHZ
			 */
			stepNumber = 14;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 2.4GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 2.4GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith2Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ));
			if (status) {
				deviceConnectedWith2Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 2.4GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE EXECUTING TEST CASE 'TC-RDKB-WIFI-KEY-PHARSE-5001' : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
			LOGGER.info("POSTCONDITION :  " + postConStepNumber
					+ " : DESCRIPTION :  CHANGE THE DEFAULT PASSPHRASE FOR 2.4GHZ SSID USING WEBPA PARAM");
			LOGGER.info("POSTCONDITION :  " + postConStepNumber
					+ " : ACTION :  SET AND VERIFY THE DEFAULT PASSPHRASE FOR 2.4Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.Security.X_COMCAST-COM_KeyPassphrase'");
			LOGGER.info("POSTCONDITION :  " + postConStepNumber
					+ " : EXPECTED: DEFAULT PASSPHRASE FOR 2.4Ghz SSID SHOULD BE CHANGED SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO SET DEFAULT PASSPHRASE FOR 2.4Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10001.Security.X_COMCAST-COM_KeyPassphrase'";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_2GHZ_PASSPHRASE,
					WebPaDataTypes.STRING.getValue(), defaultPasswordFromWebPa);
			if (status) {
				LOGGER.info("POSTCONDITION :  " + postConStepNumber
						+ " : ACTUAL : CHANGED THE DEFAULT PASSPHRASE FOR 2.4GHZ SSID USING WEBPA PARAM SUCCESSFULLY");
			} else {
				LOGGER.error("POSTCONDITION :  " + postConStepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			postConStepNumber = 2;
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith2Ghz(device, tapEnv,
					deviceConnectedWith2Ghz, postConStepNumber);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-KEY-PHARSE-5001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 *
	 * Test Case : Verify that the Preshared key of WIFI access point can be updated
	 * & read by admin for 5 Ghz
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Verify 5 GHz SSID is enabled</li>
	 * <li>Step 1 : Get and verify the default passphrase for 5 GHz SSID using
	 * webpa</li>
	 * <li>Step 2 : Connect the connected client in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 3 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 4 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 5 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 6 : Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 7 : Change the passphrase for 5GHz ssid using webpa param</li>
	 * <li>Step 8 : Verify the state of the 5 GHz ssid in connected client</li>
	 * <li>Step 9 : Connect the client to 5 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>Step 10 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 11 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 12 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 13 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 14 : Verify disconnecting the 5 GHz private wifi SSID</li>
	 * </ol>
	 * 
	 * @author Muthukumar
	 * @refactor Athira
	 * 
	 * @param device instance of {@link Dut}
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-WIFI-KEY-PHARSE-5002")
	public void testToVerifyClientConnectivity5GhzWifiPassphraseChange(Dut device) {
		String testId = "TC-RDKB-WIFI-KEY-PHARSE-502";
		stepNumber = 1;
		preConStepNumber = 1;
		postConStepNumber = 1;
		String testStepNumber = "S" + stepNumber;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWith5Ghz = null;
		String defaultPasswordFromWebPa = null;
		String defaultPasswordFromSnmp = null;

		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-KEY-PHARSE-5002");
			LOGGER.info(
					"TEST DESCRIPTION: Verify that the Preshared key of WIFI access point can be updated & read by admin for 5 Ghz.");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: ");
			LOGGER.info(" 1 : Get and verify the default passphrase for 5 GHz SSID using webpa");
			LOGGER.info(" 2 : Connect  the connected client  in the setup to 5 GHz SSID and verify connection status");
			LOGGER.info(" 3 : Verify  the correct IPv4  address for client connected with 5 GHz SSID");
			LOGGER.info(" 4 : Verify  the correct IPv6  address for client connected with 5 GHz SSID");
			LOGGER.info(
					" 5 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
			LOGGER.info(
					" 6 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
			LOGGER.info(" 7 : Change the passphrase for 5GHz ssid using webpa param ");
			LOGGER.info(" 8 : Verify the state of the 5 GHz ssid in connected client");
			LOGGER.info(" 9 : Connect the client to 5 GHz Private Wi-Fi Network and verify connection status ");
			LOGGER.info(" 10 : Verify  the correct IPv4  address for client connected with 5 GHz SSID");
			LOGGER.info(" 11 : Verify  the correct IPv6  address for client connected with 5 GHz SSID");
			LOGGER.info(
					" 12 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
			LOGGER.info(
					" 13 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
			LOGGER.info(" 14 : Verify disconnecting the 5 GHz private wifi SSID");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : DESCRIPTION : VERIFY AND ENABLE 5GHZ PRIVATE SSID USING WEBPA");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTION :  VERIFY AND ENABLE 5GHZ PRIVATE SSID USING WEBPA PARAM 'Device.WiFi.SSID.10101.Enable'");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : EXPECTED : 5GHZ PRIVATE SSID SHOULD BE ENABLED SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			try {
				errorMessage = "5 GHZ SSID IS IN DISABLED STATE";
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
						BroadBandTestConstants.TRUE);
			} catch (TestException exp) {
				errorMessage = "FAILED TO ENABLE 5GHZ SSID USING WEBPA PARAM 'Device.WiFi.SSID.10101.Enable'";
				// Enable 2.4GHz Radio
				status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
						WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, true);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION " + preConStepNumber
						+ " : ACTUAL : 5GHZ PRIVATE SSID ENABLED IS ENABLED SUCCESSFULLY");
			} else {
				LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION " + preConStepNumber
						+ " FAILED : " + errorMessage);
			}
			LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");
			/**
			 * STEP 1 : GET AND VERIFY DEFAULT PASSPHRASE FOR 5 Ghz SSID USING WEBPA PARAM
			 */
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION :  GET AND VERIFY DEFAULT PASSPHRASE FOR 5Ghz SSID USING WEBPA PARAM");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION :  GET DEFAULT PASSPHRASE FOR 5Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10101.Security.X_COMCAST-COM_KeyPassphrase'");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED : DEFAULT PASSPHRASE FOR 5Ghz SSID SHOULD BE OBTAINED SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO GET DEFAULT PASSPHRASE FOR 5Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10101.Security.X_COMCAST-COM_KeyPassphrase'";

			try {

				defaultPasswordFromWebPa = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(
						device, tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				status = CommonMethods.isNotNull(defaultPasswordFromWebPa);

			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL : OBTAINED AND VERIFIED THE DEFAULT PASSPHRASE FOR 5Ghz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 2 : VERIFY CONNECTING THE WI-FI CLIENT IN THE DEVICE TO 5GHz SSID
			 */

			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE DEVICE TO 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 5GHz SSID";
			try {
				deviceConnectedWith5Ghz = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWith5Ghz != null);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 5GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * SETP 3 - 6
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					stepNumber);
			/**
			 * STEP 7 : CHANGE THE PASSPHRASE FOR 5GHZ SSID USING WEBPA PARAM
			 */
			stepNumber = 7;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION :  CHANGE THE PASSPHRASE FOR 5GHZ SSID USING WEBPA PARAM");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION :  SET AND VERIFY THE PASSPHRASE FOR 5Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10101.Security.X_COMCAST-COM_KeyPassphrase'");
			LOGGER.info(
					"STEP :  " + stepNumber + " : EXPECTED: PASSPHRASE FOR 5Ghz SSID SHOULD BE CHANGED SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO SET PASSPHRASE FOR 2.4Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10101.Security.X_COMCAST-COM_KeyPassphrase'";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_5GHZ_PASSPHRASE,
					WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.TEST_SSID_PASSWORD);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL : CHANGED THE PASSPHRASE FOR 5GHZ SSID USING WEBPA PARAM SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 8 : VERIFY THE WIFI CONNECTION STATE OF THE 5GHZ CONNECTED CLIENT
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY THE WIFI CONNECTION STATE OF THE 5GHZ CONNECTED CLIENT");
			LOGGER.info("STEP :  " + stepNumber
					+ " : ACTION : LINUX - nmcli | grep connected |grep -i wlan0 AND WINDOWS - netsh wlan show interface | grep -i 'state'");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED : CLIENT WIFI CONNECTION STATE SHOULD BE DISCONNECTED");
			LOGGER.info("#######################################################################################");
			errorMessage = "CLIENT DIDN'T GET DISCONNECTED FROM 5GHZ SSID, EVEN AFTER CHANGING THE PASSPHRASE";
			tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			status = ConnectedNattedClientsUtils.verifyWifiConnectionStatus(deviceConnectedWith5Ghz, tapEnv, null,
					false);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL : CLIENT WIFI CONNECTION STATE IS DISCONNECTED AS EXPECTED");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 9 : VERIFY CONNECTING THE WI-FI CLIENT IN THE DEVICE TO 5GHz SSID
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE DEVICE TO 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED : THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 5GHz SSID";
			try {
				deviceConnectedWith5Ghz = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWith5Ghz != null);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 5GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
			/**
			 * SETP 10 - 13
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					stepNumber);
			/**
			 * Step 14: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5
			 * GHZ
			 */
			stepNumber = 14;
			testStepNumber = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 5GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 5GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith5Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ));
			if (status) {
				deviceConnectedWith5Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 5GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE EXECUTING TEST CASE 'TC-RDKB-WIFI-KEY-PHARSE-5002' : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
					true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");
			LOGGER.info("POSTCONDITION :  " + postConStepNumber
					+ " : DESCRIPTION :  CHANGE THE DEFAULT PASSPHRASE FOR 5GHZ SSID USING WEBPA PARAM");
			LOGGER.info("POSTCONDITION :  " + postConStepNumber
					+ " : ACTION :  SET AND VERIFY THE DEFAULT PASSPHRASE FOR 5Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10101.Security.X_COMCAST-COM_KeyPassphrase'");
			LOGGER.info("POSTCONDITION :  " + postConStepNumber
					+ " : EXPECTED: DEFAULT PASSPHRASE FOR 5Ghz SSID SHOULD BE CHANGED SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO SET DEFAULT PASSPHRASE FOR 5Ghz SSID USING WEBPA PARAM 'Device.WiFi.AccessPoint.10101.Security.X_COMCAST-COM_KeyPassphrase'";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_5GHZ_PASSPHRASE,
					WebPaDataTypes.STRING.getValue(), defaultPasswordFromWebPa);
			if (status) {
				LOGGER.info("POSTCONDITION :  " + postConStepNumber
						+ " : ACTUAL : CHANGED THE DEFAULT PASSPHRASE FOR 5GHZ SSID USING WEBPA PARAM SUCCESSFULLY");
			} else {
				LOGGER.error("POSTCONDITION :  " + postConStepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			postConStepNumber = 2;
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith5Ghz(device, tapEnv,
					deviceConnectedWith5Ghz, postConStepNumber);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-KEY-PHARSE-5002");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Test Case : Verify the WIFI Connectivity on Dual Band Radio 802.11n client
	 * with security mode open.
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Step 1 :Set and verify the value of operational transmission rate of the
	 * 2.4 GHz with operating standard as g/n</li>
	 * <li>Step 2 :Set and verify the channel value to 11.</li>
	 * <li>Step 3 :Set and verify the security mode as "none" for 2.4 GHz SSID</li>
	 * <li>Step 4 :Set and verify the value of operational transmission rate of the
	 * 5GHz with operating standard as n</li>
	 * <li>Step 5 :Set and verify the channel value to 161.</li>
	 * <li>Step 6 :Set and verify the security mode as "none" for 5 GHz SSID</li>
	 * <li>Step 7 :Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 8 :Verify number of connected clients associated with 2.4 GHz</li>
	 * <li>Step 9 :Verify the client mac address and connection type connected with
	 * 2.4 GHz</li>
	 * <li>Step 10 :Connect the connected client in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 11 :Verify number of connected clients associated with 5 GHz</li>
	 * <li>Step 12 :Verify the client mac address and connection type connected with
	 * 5 GHz</li>
	 * <li>Step 13 :Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 14 :Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 15 :Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 16 :Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 17 :Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 18 :Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 19 :Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 20 :Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 21 :Verify disconnecting the 2.4 GHz SSID</li>
	 * <li>Step 22 :Verify disconnecting the 5 GHz SSID</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @refactor Athira
	 *
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-WIFI-CONFIG-CC-5001")
	public void testToVerifyWiFiConnDualBandRadioSecurityModeOpen(Dut device) {

		String testId = "TC-RDKB-WIFI-CONFIG-CC-501";
		String step = null;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWith2Ghz = null;
		Dut deviceConnectedWith5Ghz = null;
		boolean isSecModeChanged2Ghz = false;
		boolean isSecModeChanged5Ghz = false;
		String deviceDateTime = null;
		WifiOperatingStandard defaultOperatingStandard = null;
		stepNumber = 1;
		preConStepNumber = 0;
		postConStepNumber = 0;
		step = "S" + stepNumber;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CONFIG-CC-5001");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the WIFI Connectivity on Dual Band Radio 802.11n client with security mode open, DCS disabled, Operating standard, updated Channel number.");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"1 :Set and verify the value of operational transmission rate of the 2.4 GHz with operating standard as g/n");
			LOGGER.info("2 :Set and verify the channel value to 11.");
			LOGGER.info("3 :Set and verify the security mode as 'none' for 2.4 GHz SSID");
			LOGGER.info(
					"4 :Set and verify the value of operational transmission rate of the 5GHz with operating standard as n");
			LOGGER.info("5 :Set and verify the channel value to 161.");
			LOGGER.info("6 :Set and verify the security mode as 'none' for 5 GHz SSID");
			LOGGER.info("7 :Connect the connected client in the setup to 2.4 GHz SSID and verify connection status");
			LOGGER.info("8 :Verify number of connected clients associated with  2.4 GHz");
			LOGGER.info("9 :Verify the client mac address and connection type connected with 2.4 GHz");
			LOGGER.info("10 :Connect the connected client in the setup to 5 GHz SSID and verify connection status");
			LOGGER.info("11 :Verify the client mac address and connection type connected with 5 GHz");
			LOGGER.info("12 :Verify number of connected clients associated with 5 GHz");
			LOGGER.info("13 :Verify the correct IPv4 address for client connected with 2.4 GHz SSID");
			LOGGER.info("14 :Verify the correct IPv6 address for client connected with 2.4 GHz SSID");
			LOGGER.info(
					"15 :Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz");
			LOGGER.info(
					"16 :Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz");
			LOGGER.info("17 :Verify the correct IPv4 address for client connected with 5 GHz SSID");
			LOGGER.info("18 :Verify the correct IPv6 address for client connected with 5 GHz SSID");
			LOGGER.info(
					"19 :Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz");
			LOGGER.info(
					"20 :Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz");
			LOGGER.info("21 :Verify disconnecting the 2.4 GHz SSID");
			LOGGER.info("22 :Verify disconnecting the 5 GHz SSID");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			executePreConditionToSetChannelSelectionModeManual(device);
			executePreConditionToGetDefaultOperStandard(device);
			executePreConditionToGetDefaultChannel(device);
			executePreConditionToVerifyPrivateSsidIsEnabled(device);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4
			 * GHZ WITH OPERATING STANDARD AS g/n
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4 GHZ WITH OPERATING STANDARD AS g/n.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET TO g/n");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET OPERATING STANDARD AS g/n.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
					WebPaDataTypes.STRING.getValue(), WifiOperatingStandard.OPERATING_STANDARD_G_N.getOperatingmode());
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS g/n.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 2 : SET AND VERIFY THE CHANNEL VALUE TO 11 FOR 2.4 GHz.
			 */
			bandwidthSetStatus = verifyAndSetOperStandBandWidthAndChannelValue(device, testId);

			/**
			 * Step 3 : SET AND VERIFY THE SECURITY MODE "OPEN" FOR 2.4 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY  THE SECURITY MODE 'OPEN' FOR 2.4 GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE SECURITY MODE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED);

			LOGGER.info("STEP " + stepNumber + ": EXPECTED : SECURITY MODE MUST SET TO 'OPEN' FOR 2.4 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY MODE TO 'OPEN' FOR 2.4 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
					WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_NONE);
			if (status) {
				isSecModeChanged2Ghz = true;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY MODE TO 'OPEN' FOR 2.4 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 4 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 5
			 * GHZ WITH OPERATING STANDARD AS n
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 5 GHZ WITH OPERATING STANDARD AS n.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET TO n");
			LOGGER.info("#######################################################################################");
			if (!DeviceModeHandler.isRPIDevice(device)) {
				defaultOperatingStandard = CommonMethods.isAtomSyncAvailable(deviceConnectedWith5Ghz, tapEnv)
						|| DeviceModeHandler.isBusinessClassDevice(device)
								? WifiOperatingStandard.OPERATING_STANDARD_A_N
								: WifiOperatingStandard.OPERATING_STANDARD_A_N_AC;
				errorMessage = "UNABLE TO SET OPERATING STANDARD AS " + defaultOperatingStandard.getOperatingmode();
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(), defaultOperatingStandard.getOperatingmode());
			} else {
				defaultOperatingStandard = WifiOperatingStandard.OPERATING_STANDARD_AC;
				errorMessage = "UNABLE TO SET OPERATING STANDARD AS " + defaultOperatingStandard.getOperatingmode();
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(), defaultOperatingStandard.getOperatingmode());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS n.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 5 : SET AND VERIFY THE CHANNEL VALUE TO 161 FOR 5 GHZ.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY THE CHANNEL VALUE TO 161 FOR 5 GHZ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE CHANNEL VALUE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL MUST BE CHANGED TO 161 FOR 5 GHZ");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO CHANGE THE CHANNEL VALUE AS 161 FOR 5 GHZ";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
					WebPaDataTypes.INTEGER.getValue(),
					DeviceModeHandler.isRPIDevice(device) ? BroadBandTestConstants.CHANNEL_NO_44
							: BroadBandTestConstants.CHANNEL_NO_161);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE CHANNEL VALUE AS 161 FOR 5 GHZ.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 6 : SET AND VERIFY THE SECURITY MODE "NONE" FOR 5 GHZ SSID
			 * 
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY  THE SECURITY MODE 'OPEN' FOR 5 GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE SECURITY MODE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : SECURITY MODE MUST SET TO 'OPEN' FOR 5 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY MODE TO 'OPEN' FOR 5 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
					WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_NONE);
			if (status) {
				isSecModeChanged5Ghz = true;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY MODE TO 'OPEN' FOR 5 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 7 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT INTO 2.4GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 2.4 GHZ SSID");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT WITH 2.4 GHZ SSID";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			deviceConnectedWith2Ghz = BroadBandConnectedClientUtils
					.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			status = (null != deviceConnectedWith2Ghz);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY CONNECTED THE CLIENT WITH 2.4 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 8- 9
			 */
			verifyConnectedclientDetails(device, testId, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_2_4GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_ASSOCIATED_DEVICES, deviceDateTime);

			/**
			 * Step 10: VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 5 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT INTO 5 GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 5 GHz SSID AND PASSWORD");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 5 GHZ SSID");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT WITH 5 GHZ SSID";
			deviceConnectedWith5Ghz = BroadBandConnectedClientUtils.getOtherWiFiCapableClientDeviceAndConnect(device,
					tapEnv, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_5GHZ);
			status = (null != deviceConnectedWith5Ghz);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY CONNECTED THE CLIENT WITH 5 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 11- 12
			 */
			verifyConnectedclientDetails(device, testId, deviceConnectedWith5Ghz, BroadBandTestConstants.BAND_5GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_ASSOCIATED_DEVICES, deviceDateTime);

			/**
			 * SETP 13- 16
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					BroadBandTestConstants.BAND_2_4GHZ);
			/**
			 * SETP 17- 20
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					BroadBandTestConstants.BAND_5GHZ);
			/**
			 * Step 21: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHZ
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 2.4GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 2.4GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith2Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ));
			if (status) {
				deviceConnectedWith2Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 2.4GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 22: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5
			 * GHZ.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 5GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 5GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith5Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ));
			if (status) {
				deviceConnectedWith5Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 5GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VALIDATING WIFI CONNECTIVITY ON DUAL BAND RADIO 802.11n CLIENT WITH SECURITY MODE OPEN: "
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			executePostConditionToSetDefaultOperStandBandWidthFor2GHZ(device, defaultOperStandardBandWidth);
			executePostConditionToSetChannelSelectionModeAuto(device);
			executePostConditionToSetDefaultOperStandard(device);
			executePostConditionToSetDefaultChannel(device);
			executePostConditionToSetSecurityModeWPA2Personal(device, isSecModeChanged2Ghz, isSecModeChanged5Ghz);
			executePostConditionToDisconnectClients(device, deviceConnectedWith2Ghz, deviceConnectedWith5Ghz);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CONFIG-CC-5001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 *
	 * Test Case : Verify the WIFI Connectivity on Dual Band Radio 2.4 - 802.11n,
	 * 5-802.11ac with security mode WPA2-PSK ( AES )
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Step 1:Set and verify the value of operational transmission rate of the
	 * 2.4 GHz with operating standard as n</li>
	 * <li>Step 2:Set and verify the invalid operating standard and verify the
	 * process crash</li>
	 * <li>Step 3:Set and verify the channel value to 11.</li>
	 * <li>Step 4:Set and verify the security mode 'WPA2-Personal' for 2.4 GHz
	 * SSID</li>
	 * <li>Step 5:Set and verify the security encryption method as 'AES' for 2.4 GHz
	 * SSID</li>
	 * <li>Step 6:Set and verify the value of operational transmission rate of the 5
	 * GHz with operating standard as ac</li>
	 * <li>Step 7:Set and verify the invalid operating standard and verify the
	 * process crash</li>
	 * <li>Step 8:Set and verify the channel value to 161.</li>
	 * <li>Step 9:Set and verify the security mode 'WPA2-Personal' for 5 GHz
	 * SSID</li>
	 * <li>Step 10:Set and verify the security encryption method as 'AES' for 5 GHz
	 * SSID</li>
	 * <li>Step 11:Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 12 :Verify number of connected clients associated with 2.4 GHz</li>
	 * <li>Step 13 :Verify the client mac address and connection type connected with
	 * 2.4 GHz</li>
	 * <li>Step 14:Connect the connected client in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 15 :Verify number of connected clients associated with 5 GHz</li>
	 * <li>Step 16 :Verify the client mac address and connection type connected with
	 * 5 GHz</li>
	 * <li>Step 17 :Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 18 :Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 19 :Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 20 :Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 21 :Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 22 :Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 23 :Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 24 :Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 25 :Verify disconnecting the 2.4 GHz SSID</li>
	 * <li>Step 26 :Verify disconnecting the 5 GHz SSID</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Muthukumar
	 * @refactor
	 * 
	 **/
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-WIFI-CONFIG-CC-5002")
	public void testToVerifyWiFiConnDualBandRadioSecurityModeWpa2Aes(Dut device) {
		String testId = "TC-RDKB-WIFI-CONFIG-CC-502";
		String step = null;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWith2Ghz = null;
		Dut deviceConnectedWith5Ghz = null;
		stepNumber = 1;
		preConStepNumber = 0;
		postConStepNumber = 0;
		step = "S" + stepNumber;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CONFIG-CC-5002");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the WIFI Connectivity on Dual Band Radio 802.11n client with security mode WPA2-PSK, DCS disabled, Operating standard, updated Channel number.");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"1 :Set and verify the value of operational transmission rate of the 2.4 GHz with operating standard as n ");
			LOGGER.info("2 :Set and verify the invalid operating standard and verify the process crash");
			LOGGER.info("3 :Set and verify the channel value to 11.");
			LOGGER.info("4 :Set and verify the security mode 'WPA2-Personal' for 2.4 GHz SSID");
			LOGGER.info("5 :Set and verify the security encryption method as 'AES' for 2.4 GHz SSID");
			LOGGER.info(
					"6 :Set and verify the value of operational transmission rate of the 5 GHz with operating standard as ac");
			LOGGER.info("7 :Set and verify the invalid operating standard and verify the process crash");
			LOGGER.info("8 :Set and verify the channel value to 161.");
			LOGGER.info("9 :Set and verify the security mode 'WPA2-Personal' for 5 GHz SSID");
			LOGGER.info("10 :Set and verify the security encryption method as 'AES' for 5 GHz SSID");
			LOGGER.info("11 :Connect the connected client in the setup to 2.4 GHz SSID and verify connection status");
			LOGGER.info("12 :Verify number of connected clients associated with  2.4 GHz");
			LOGGER.info("13 :Verify the client mac address and connection type connected with 2.4 GHz");
			LOGGER.info("14 :Connect the connected client in the setup to 5 GHz SSID and verify connection status");
			LOGGER.info("15 :Verify number of connected clients associated with 5 GHz");
			LOGGER.info("16 :Verify the client mac address and connection type connected with 5 GHz");
			LOGGER.info("17 :Verify the correct IPv4 address for client connected with 2.4 GHz SSID");
			LOGGER.info("18 :Verify the correct IPv6 address for client connected with 2.4 GHz SSID");
			LOGGER.info(
					"19 :Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz");
			LOGGER.info(
					"20 :Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz");
			LOGGER.info("21 :Verify the correct IPv4 address for client connected with 5 GHz SSID");
			LOGGER.info("22 :Verify the correct IPv6 address for client connected with 5 GHz SSID");
			LOGGER.info(
					"23 :Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz");
			LOGGER.info(
					"24 :Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz");
			LOGGER.info("25 :Verify disconnecting the 2.4 GHz SSID");
			LOGGER.info("26 :Verify disconnecting the 5 GHz SSID");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			executePreConditionToSetChannelSelectionModeManual(device);
			executePreConditionToGetDefaultOperStandard(device);
			executePreConditionToGetDefaultChannel(device);
			executePreConditionToVerifyPrivateSsidIsEnabled(device);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4
			 * GHZ WITH OPERATING STANDARD AS n
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4 GHZ WITH OPERATING STANDARD AS n.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET TO n");
			errorMessage = "UNABLE TO SET OPERATING STANDARD AS n FOR OPERATIONAL TRANSMISSION RATE OF 2.4 GHZ FREQ BAND.";

			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
					WebPaDataTypes.STRING.getValue(),
					BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
							BroadBandPropertyKeyConstants.WIFI_OPERATING_STANDARDS_FOR_DEVICE_AND_WIFI_FREQUENCY_SPECIFIC_2GHZ));

			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS n FOR OPERATIONAL TRANSMISSION RATE OF 2.4 GHZ FREQ BAND.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 2 : validate the process crash when trying to set the invalid operating
			 * standard for DSL Device -2.4 GHz
			 */
			executeTestStepToValidateProcessCrash(device, testId, BroadBandTestConstants.BAND_2_4GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
					WifiOperatingStandard.OPERATING_STANDARD_N.getOperatingmode());

			/**
			 * Step 3 : SET AND VERIFY THE CHANNEL VALUE TO 11 FOR 2.4GHz.
			 */
			bandwidthSetStatus = verifyAndSetOperStandBandWidthAndChannelValue(device, testId);

			/**
			 * Step 4 : SET AND VERIFY THE SECURITY MODE WPA2-Personal" FOR 2.4 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ":DESCRIPTION : SET AND VERIFY  THE SECURITY MODE 'WPA3-Personal-Transition'for ADA and \"\r\n"
					+ "		    + \"'WPA2-Personal'for other devices FOR 2.4 GHZ SSID\"");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE SECURITY MODE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED);
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED : SECURITY MODE MUST SET TO 'WPA2-Personal' FOR 2.4 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY MODE TO 'WPA2-Personal' FOR 2.4 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
					WebPaDataTypes.STRING.getValue(), BroadBandCommonUtils.getSecurityModeForDeviceModels(device));
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY MODE TO 'WPA2-Personal' FOR 2.4 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 5 : SET AND VERIFY THE SECURITY ENCRYPTION METHOD AS "AES" FOR 2.4 GHZ
			 * SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE SECURITY ENCRYPTION METHOD AS 'AES' FOR 2.4 GHZ SSID");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY  THE SECURITY ENCRYPTION METHOD AS AES FOR 2.4GHz SSID USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_FOR_ENCRYPTIONMETHOD_IN_2GHZ_PRIVATE_WIFI);
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : SECURITY ENCRYPTION METHOD MUST SET TO 'AES' FOR 2.4 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY ENCRYPTION METHOD TO 'AES' FOR 2.4 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_ENCRYPTIONMETHOD_IN_2GHZ_PRIVATE_WIFI,
					WebPaDataTypes.STRING.getValue(),
					BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY ENCRYPTION METHOD TO 'AES' FOR 2.4 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 6 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE
			 * 5GHZ WITH OPERATING STANDARD AS ac
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 5 GHZ WITH OPERATING STANDARD AS ac.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET ");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET OPERATING STANDARD FOR OPERATIONAL TRANSMISSION RATE OF 5 GHZ FREQ BAND";

			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
					WebPaDataTypes.STRING.getValue(),
					BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
							BroadBandPropertyKeyConstants.WIFI_OPERATING_STANDARDS_FOR_DEVICE_AND_WIFI_FREQUENCY_SPECIFIC_5GHZ));

			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD FOR OPERATIONAL TRANSMISSION RATE OF 5 GHZ FREQ BAND.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 7 : validate the process crash when trying to set the invalid operating
			 * standard for DSL Device-5 GHz
			 */
			executeTestStepToValidateProcessCrash(device, testId, BroadBandTestConstants.BAND_5GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
					WifiOperatingStandard.OPERATING_STANDARD_AC.getOperatingmode());

			/**
			 * Step 8 : SET AND VERIFY THE CHANNEL VALUE TO 161 FOR 5 GHZ.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			if (!DeviceModeHandler.isDSLDevice(device)) {
				LOGGER.info("#######################################################################################");
				LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY THE CHANNEL VALUE TO 161 FOR 5 GHZ");
				LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE CHANNEL VALUE USING WEBPA PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL MUST BE CHANGED TO 161 FOR 5 GHZ");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO CHANGE THE CHANNEL VALUE AS 161 FOR 5 GHZ";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
						WebPaDataTypes.INTEGER.getValue(),
						DeviceModeHandler.isRPIDevice(device) ? BroadBandTestConstants.CHANNEL_NO_44
								: BroadBandTestConstants.CHANNEL_NO_161);
			} else {
				LOGGER.info("#######################################################################################");
				LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY THE CHANNEL VALUE TO 36 FOR 5 GHZ");
				LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE CHANNEL VALUE USING WEBPA PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL MUST BE CHANGED TO 36 FOR 5 GHZ");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO CHANGE THE CHANNEL VALUE AS 36 FOR 5 GHZ";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
						WebPaDataTypes.INTEGER.getValue(), BroadBandTestConstants.RADIO_CHANNEL_36);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE CHANNEL VALUE FOR 5 GHZ.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 9 : SET AND VERIFY THE SECURITY MODE "WPA2-Personal" FOR 5 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY  THE SECURITY MODE 'WPA3-Personal-Transition' for ADA and 'WPA2-Personal' FOR 5 GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE SECURITY MODE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : SECURITY MODE MUST SET TO 'WPA2-Personal' FOR 5 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY MODE TO 'WPA3-Personal-Transition'/'WPA2-Personal' FOR 5 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
					WebPaDataTypes.STRING.getValue(), BroadBandCommonUtils.getSecurityModeForDeviceModels(device));
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY MODE TO 'WPA2-Personal' FOR 5 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 10 : SET AND VERIFY THE SECURITY ENCRYPTION METHOD AS "AES" FOR 5 GHZ
			 * SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE SECURITY ENCRYPTION METHOD AS 'AES' FOR 5 GHZ SSID");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY  THE SECURITY ENCRYPTION METHOD AS AES FOR 5GHz SSID USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_FOR_ENCRYPTIONMETHOD_IN_5GHZ_PRIVATE_WIFI);
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED : SECURITY ENCRYPTION METHOD MUST SET TO 'AES' FOR 5 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY ENCRYPTION METHOD TO 'AES' FOR 5 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_ENCRYPTIONMETHOD_IN_5GHZ_PRIVATE_WIFI,
					WebPaDataTypes.STRING.getValue(),
					BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY ENCRYPTION METHOD TO 'AES' FOR 5 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 11 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4GHZ SSID
			 */

			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT INTO 2.4GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 2.4 GHZ SSID");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT TO 2.4 GHZ SSID";
			String deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			deviceConnectedWith2Ghz = BroadBandConnectedClientUtils
					.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			LOGGER.info("STEP " + stepNumber + " : ACTUAL deviceConnectedWith2Ghz is " + deviceConnectedWith2Ghz);
			status = (null != deviceConnectedWith2Ghz);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY CONNECTED THE CLIENT WITH 2.4 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 12- 13
			 */
			verifyConnectedclientDetails(device, testId, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_2_4GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_ASSOCIATED_DEVICES, deviceDateTime);

			/**
			 * Step 14 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 5 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT INTO 5 GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 5 GHz SSID AND PASSWORD");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 5 GHZ SSID");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT CONNECTED WITH 5 GHZ SSID";
			deviceConnectedWith5Ghz = BroadBandConnectedClientUtils.getOtherWiFiCapableClientDeviceAndConnect(device,
					tapEnv, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_5GHZ);
			status = (null != deviceConnectedWith5Ghz);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFYLLY CONNECTED THE CLIENT CONNECTED WITH 5 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 15- 16
			 */
			verifyConnectedclientDetails(device, testId, deviceConnectedWith5Ghz, BroadBandTestConstants.BAND_5GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_ASSOCIATED_DEVICES, deviceDateTime);

			/**
			 * SETP 17- 20
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					BroadBandTestConstants.BAND_2_4GHZ);
			/**
			 * SETP 21-24
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					BroadBandTestConstants.BAND_5GHZ);
			/**
			 * Step 25: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHZ
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 2.4GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO DISCONNECT THE CLIENT CONNECTED WITH 2.4GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith2Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ));
			if (status) {
				deviceConnectedWith2Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 2.4GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 26: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5
			 * GHZ.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 5GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO DISCONNECT THE CLIENT CONNECTED WITH 5GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith5Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ));
			if (status) {
				deviceConnectedWith5Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 5GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VALIDATING WIFI CONNECTIVITY ON DUAL BAND RADIO 802.11n CLIENT WITH SECURITY MODE WPA2 PERSONAL: "
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			executePostConditionToSetDefaultOperStandBandWidthFor2GHZ(device, defaultOperStandardBandWidth);
			executePostConditionToSetChannelSelectionModeAuto(device);
			executePostConditionToSetDefaultOperStandard(device);
			executePostConditionToSetDefaultChannel(device);
			executePostConditionToDisconnectClients(device, deviceConnectedWith2Ghz, deviceConnectedWith5Ghz);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CONFIG-CC-5002");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Post-Condition method to set the chanel selection mode as AUTO .
	 * 
	 * @param device {@link Dut}
	 * @refactor Athira
	 */
	private void executePostConditionToSetChannelSelectionModeAuto(Dut device) throws TestException {
		String errorMessage = null;
		boolean status = false;
		try {
			/**
			 * POSTCONDITION :SET THE CHANNEL SELECTION MODE TO AUTO FOR 2.4 GHz USING WEBPA
			 */
			postConStepNumber++;
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : DESCRIPTION : SET THE CHANNEL SELECTION MODE TO AUTO FOR 2.4 GHz ");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : ACTION : SET THE CHANNEL SELECTION MODE TO AUTO FOR 2.4 GHz USING WEBPA ");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : EXPTECTED : THE CHANNEL SELECTION MODE TO AUTO CHANGED SUCCESSFULLY FOR 2.4 GHz");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE CHANNEL SELECTION MODE TO AUTO FOR 2.4 GHz USING WEBPA. ";
			try {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_CHANNEL_SELECTION_MODE,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			} catch (Exception exception) {
				errorMessage = errorMessage + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTUAL : THE CHANNEL SELECTION MODE TO AUTO FOR 2.4 GHz CHANGED SUCCESSFULLY.");
			} else {
				LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
			}

			LOGGER.info("#######################################################################################");

			/**
			 * POSTCONDITION :SET THE CHANNEL SELECTION MODE TO AUTO FOR 5 GHz USING WEBPA
			 */
			postConStepNumber++;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : DESCRIPTION : SET THE CHANNEL SELECTION MODE TO AUTO FOR 5 GHz ");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : ACTION : SET THE CHANNEL SELECTION MODE TO AUTO FOR 5 GHz USING WEBPA ");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : EXPTECTED : THE CHANNEL SELECTION MODE TO AUTO CHANGED SUCCESSFULLY FOR 5 GHz");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE CHANNEL SELECTION MODE TO AUTO FOR 5 GHz USING WEBPA. ";
			try {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_CHANNEL_SELECTION_MODE,
						BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
			} catch (Exception exception) {
				errorMessage = errorMessage + exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTUAL : THE CHANNEL SELECTION MODE TO AUTO FOR 5 GHz CHANGED SUCCESSFULLY.");
			} else {
				LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("#######################################################################################");
		} catch (Exception exception) {
			LOGGER.error(
					"Execution error occurred while executing to Set Channel Selection Mode as Auto due to exception --> "
							+ exception.getMessage());
		}
	}

	/**
	 * Post-Condition method to set the default operating standard
	 * 
	 * @param device {@link Dut}
	 * @refactor Athira
	 */
	private void executePostConditionToSetDefaultOperStandard(Dut device) throws TestException {
		String errorMessage = null;
		boolean status = false;
		try {
			/**
			 * POSTCONDITION :SET THE DEFAULT VALUE FOR OPERATING STANDARD 2.4 GHZ USING
			 * WEBPA
			 */
			if (defaultOperStandard_2_4Ghz != null) {
				postConStepNumber++;
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": DESCRIPTION : SET THE DEFAULT VALUE FOR OPERATING STANDARD 2.4 GHZ.");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": ACTION : SET THE DEFAULT VALUE FOR OPERATING STANDARD 2.4 GHZ USING WEBPA PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": EXPECTED : THE 2.4 GHZ OPERATING STANDARD MUST BE SET TO DEFAULT VALUE");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO SET 2.4 GHZ OPERATING STANDARD DEFAULT VALUE.";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(), defaultOperStandard_2_4Ghz);
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : SUCCESSFULLY SET THE DEFAULT VALUE  FOR OPERATING STANDARD 2.4 GHZ.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
				LOGGER.info("#######################################################################################");
			}
			/**
			 * POSTCONDITION 4 :SET THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ USING
			 * WEBPA
			 */
			if (defaultOperStandard_5Ghz != null) {
				postConStepNumber++;
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": DESCRIPTION : SET THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ.");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": ACTION : SET THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ USING WEBPA PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD);
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": EXPECTED : THE 5 GHZ OPERATING STANDARD MUST BE SET TO DEFAULT VALUE");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO SET 5 GHZ OPERATING STANDARD DEFAULT VALUE.";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(), defaultOperStandard_5Ghz);
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : SUCCESSFULLY SET THE DEFAULT VALUE  FOR OPERATING STANDARD 5 GHZ.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
				LOGGER.info("#######################################################################################");
			}
		} catch (Exception exception) {
			LOGGER.error(
					"Execution error occurred while executing to Set Default Operating Standard post conditions due to exception --> "
							+ exception.getMessage());
		}
	}

	/**
	 * Post-Condition method to set the default operating standard Bandwidth for 2.4
	 * GHz
	 * 
	 * @param device                    {@link Dut}
	 * @param defaultOperStandBandWidth Instance for default operating standard
	 *                                  bandwidth for 2.4 GHz
	 * @refactor Athira
	 * 
	 */
	private void executePostConditionToSetDefaultOperStandBandWidthFor2GHZ(Dut device, String defaultOperStandBandWidth)
			throws TestException {
		try {
			/**
			 * POSTCONDITION :SET THE DEFAULT VALUE FOR OPERATING STANDARD BANDWIDTH FOR 2.4
			 * GHZ USING WEBPA
			 */
			Boolean isSpecificDevice = null;
			isSpecificDevice = BroadbandPropertyFileHandler.isSpecificDevice(device);

			if (bandwidthSetStatus && DeviceModeHandler.isFibreDevice(device)
					|| DeviceModeHandler.isBusinessClassDevice(device) || isSpecificDevice) {
				postConStepNumber++;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : DESCRIPTION : SET THE OPERATING BANDWIDTH TO DEFAULT " + defaultOperStandBandWidth
						+ " FOR 2.4 GHz ");
				LOGGER.info(
						"POST-CONDITION " + postConStepNumber + " : ACTION : SET THE OPERATING BANDWIDTH TO DEFAULT "
								+ defaultOperStandBandWidth + " FOR 2.4 GHz USING WEBPA ");
				LOGGER.info("POST-CONDITION " + postConStepNumber + " : EXPTECTED : THE OPERATING BANDWIDTH TO DEFAULT "
						+ defaultOperStandBandWidth + " CHANGED SUCCESSFULLY FOR 2.4 GHz");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO SET 2.4 GHZ OPERATING STANDARD DEFAULT VALUE AS " + defaultOperStandBandWidth;
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND,
						WebPaDataTypes.STRING.getValue(), defaultOperStandBandWidth);
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : SUCCESSFULLY SET OPERATING THE OPERATING STANDARD BADNWIDTH DEFAULT VALUE AS "
							+ defaultOperStandBandWidth + " 2.4 GHZ.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
				LOGGER.info("#######################################################################################");
			}
		} catch (Exception exception) {
			LOGGER.error(
					"Execution error occurred while executing to Set Default Operating Standard Bandwidth post conditions due to exception --> "
							+ exception.getMessage());
		}
	}

	/**
	 * Pre-Condition method to performing the channel selection mode as manual.
	 * 
	 * @param device {@link Dut}
	 * @refactor Athira
	 */
	private void executePreConditionToSetChannelSelectionModeManual(Dut device) throws TestException {
		String errorMessage = null;
		boolean status = false;
		/**
		 * PRECONDITION :SET THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 GHz USING
		 * WEBPA
		 */
		preConStepNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : DESCRIPTION : SET THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 GHz ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : ACTION : SET THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 GHz USING WEBPA ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : EXPTECTED : THE CHANNEL SELECTION MODE TO MANUAL CHANGED SUCCESSFULLY FOR 2.4 GHz");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO SET THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 GHz USING WEBPA. HENCE BLOCKING THE EXECUTION.";
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_CHANNEL_SELECTION_MODE, BroadBandTestConstants.CONSTANT_3,
				BroadBandTestConstants.FALSE);
		if (status) {
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTUAL : THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 GHz CHANGED SUCCESSFULLY.");
		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		/**
		 * PRECONDITION :SET THE CHANNEL SELECTION MODE TO MANUAL FOR 5 GHz USING WEBPA
		 */
		errorMessage = null;
		status = false;
		preConStepNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : DESCRIPTION : SET THE CHANNEL SELECTION MODE TO MANUAL FOR 5 GHz ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : ACTION : SET THE CHANNEL SELECTION MODE TO MANUAL FOR 5 GHz USING WEBPA ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : EXPTECTED : THE CHANNEL SELECTION MODE TO MANUAL CHANGED SUCCESSFULLY FOR 5 GHz");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO SET THE CHANNEL SELECTION MODE TO MANUAL FOR 5 GHz USING WEBPA. HENCE BLOCKING THE EXECUTION.";
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_CHANNEL_SELECTION_MODE, BroadBandTestConstants.CONSTANT_3,
				BroadBandTestConstants.FALSE);
		if (status) {
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTUAL : THE CHANNEL SELECTION MODE TO MANUAL FOR 5 GHz CHANGED SUCCESSFULLY.");
		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");

	}

	/**
	 * Pre-Condition method to get the default operating standard for 2.4 & 5 Ghz .
	 * 
	 * @param device {@link Dut}
	 * @refactor Athira
	 */
	private void executePreConditionToGetDefaultOperStandard(Dut device) throws TestException {
		defaultOperStandard_2_4Ghz = null;
		defaultOperStandard_5Ghz = null;
		String errorMessage = null;
		boolean status = false;
		/**
		 * PRECONDITION :GET THE DEFAULT VALUE FOR OPERATING STANDARD 2.4 GHZ
		 */
		errorMessage = null;
		status = false;
		preConStepNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : DESCRIPTION : GET THE DEFAULT VALUE FOR OPERATING STANDARD 2.4 GHZ ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : ACTION : GET THE DEFAULT VALUE FOR OPERATING STANDARD 'Device.WiFi.Radio.10000.OperatingStandards' 2.4 GHZ USING WEBPA ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : EXPTECTED : MUST RETURN THE DEFAULT OPERATING STANDARD VALUE FOR 2.4 GHZ");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO GET THE DEFAULT VALUE FOR OPERATING STANDARD 2.4 GHZ - HENCE BLOCKING THE EXECUTION.";
		defaultOperStandard_2_4Ghz = tapEnv.executeWebPaCommand(device,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);
		status = CommonMethods.isNotNull(defaultOperStandard_2_4Ghz);
		if (status) {
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTUAL : RETRIEVED THE DEFAULT VALUE FOR OPERATING STANDARD 2.4 GHZ SUCCESSFULLY.");
		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");

		/**
		 * PRECONDITION :GET THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ
		 */
		errorMessage = null;
		status = false;
		preConStepNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : DESCRIPTION : GET THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : ACTION : GET THE DEFAULT VALUE FOR OPERATING STANDARD 'Device.WiFi.Radio.10100.OperatingStandards' 5 GHZ USING WEBPA ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : EXPTECTED : MUST RETURN THE DEFAULT OPERATING STANDARD VALUE FOR 5 GHZ");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO GET THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ - HENCE BLOCKING THE EXECUTION.";
		defaultOperStandard_5Ghz = tapEnv.executeWebPaCommand(device,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD);
		status = CommonMethods.isNotNull(defaultOperStandard_5Ghz);
		if (status) {
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTUAL : RETRIEVED THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ SUCCESSFULLY.");
		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Pre-Condition method to get the default channel value for 2.4 & 5 Ghz .
	 * 
	 * @param device {@link Dut}
	 * @refactor Athira
	 */
	private void executePreConditionToGetDefaultChannel(Dut device) throws TestException {
		String errorMessage = null;
		boolean status = false;
		defaultChannel_2_4Ghz = 0;
		defaultChannel_5Ghz = 0;
		/**
		 * PRECONDITION :GET THE DEFAULT CHANNEL VALUE FOR 2.4 GHZ
		 */
		errorMessage = null;
		status = false;
		preConStepNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"PRE-CONDITION " + preConStepNumber + " : DESCRIPTION : GET THE DEFAULT CHANNEL VALUE FOR 2.4 GHZ ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : ACTION : GET THE DEFAULT CHANNEL VALUE 'Device.WiFi.Radio.10000.Channel' FOR 2.4 GHZ USING WEBPA ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : EXPTECTED : MUST RETURN THE DEFAULT CHANNEL VALUE FOR 2.4 GHZ");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO GET THE DEFAULT CHANNEL VALUE FOR 2.4 GHZ - HENCE BLOCKING THE EXECUTION.";
		try {
			defaultChannel_2_4Ghz = Integer.parseInt(tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ));
		} catch (Exception exception) {
			errorMessage = "NUMBER FORMAT EXCEPTION WHILE GETTING THE DEFAULT CHANNEL VALUE FOR 2.4Ghz ";
			defaultChannel_2_4Ghz = 0;
			LOGGER.error(errorMessage + exception.getMessage());
		}
		status = defaultChannel_2_4Ghz > 0;
		if (status) {
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTUAL : RETRIEVED THE DEFAULT VALUE CHANNEL VALUE FOR 2.4 GHZ SUCCESSFULLY.");
		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");

		/**
		 * PRECONDITION :GET THE DEFAULT CHANNEL VALUE FOR 5 GHZ
		 */
		errorMessage = null;
		status = false;
		preConStepNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber + " : DESCRIPTION : GET THE DEFAULT CHANNEL VALUE FOR 5 GHZ ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : ACTION : GET THE DEFAULT CHANNEL VALUE 'Device.WiFi.Radio.10100.Channel' FOR 5 GHZ USING WEBPA ");
		LOGGER.info(
				"PRE-CONDITION " + preConStepNumber + " : EXPTECTED : MUST RETURN THE DEFAULT CHANNEL VALUE FOR 5 GHZ");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO GET THE DEFAULT CHANNEL VALUE FOR 5 GHZ - HENCE BLOCKING THE EXECUTION.";
		try {
			defaultChannel_5Ghz = Integer.parseInt(tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ));
		} catch (Exception exception) {
			errorMessage = "NUMBER FORMAT EXCEPTION WHILE GETTING THE DEFAULT CHANNEL VALUE FOR 5Ghz ";
			defaultChannel_5Ghz = 0;
			LOGGER.error(errorMessage + exception.getMessage());
		}
		status = defaultChannel_5Ghz > 0;
		if (status) {
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTUAL : RETRIEVED THE DEFAULT VALUE CHANNEL VALUE FOR 5 GHZ SUCCESSFULLY.");
		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Pre-Condition method to verify the private ssid enabled status for 2.4 & 5
	 * Ghz .
	 * 
	 * @param device {@link Dut}
	 * @refactor Athira
	 */
	private void executePreConditionToVerifyPrivateSsidIsEnabled(Dut device) throws TestException {
		String errorMessage = null;
		boolean status = false;
		/**
		 * PRECONDITION :Enable Private 2.4 GHz SSID via WebPA
		 */
		preConStepNumber++;
		errorMessage = null;
		status = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : DESCRIPTION : SET AND VERIFY WHETHER PRIVATE 2.4 GHZ SSID 'DEVICE.WIFI.SSID.10001.ENABLE' IS ENABLED ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : ACTION : SET AND VERIFY WHETHER PRIVATE 2.4 GHZ SSID 'DEVICE.WIFI.SSID.10001.ENABLE' IS ENABLED USING WEBPA ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : EXPTECTED : DEVICE SHOULD BE ENABLED WITH PRIVATE 2.4 GHZ SSID AND RESPONSE SHOULD BE TRUE");
		LOGGER.info("#######################################################################################");
		errorMessage = "NOT ABLE TO ENABLE THE 2.4GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION.";
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
				BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
		if (status) {
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTUAL : PRIVATE 2.4 GHZ SSID ENABLED IN GATEWAY DEVICE.");
		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
		/**
		 * PRECONDITION :Enable Private 5 GHz SSID via WebPA
		 */
		preConStepNumber++;
		errorMessage = null;
		status = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : DESCRIPTION : SET AND VERIFY WHETHER PRIVATE 5 GHZ SSID 'DEVICE.WIFI.SSID.10101.ENABLE' IS ENABLED ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : ACTION : SET AND VERIFY WHETHER PRIVATE 5 GHZ SSID 'DEVICE.WIFI.SSID.10101.ENABLE' IS ENABLED USING WEBPA ");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : EXPTECTED : DEVICE SHOULD BE ENABLED WITH PRIVATE 5 GHZ SSID AND RESPONSE SHOULD BE TRUE");
		LOGGER.info("#######################################################################################");
		errorMessage = "NOT ABLE TO ENABLE THE 5GHZ PRIVATE SSID ON THIS DEVICE - HENCE BLOCKING THE EXECUTION.";
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
				BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
		if (status) {
			LOGGER.info(
					"PRE-CONDITION " + preConStepNumber + " : ACTUAL : PRIVATE 5 GHZ SSID ENABLED IN GATEWAY DEVICE.");
		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Post-Condition method to set the default channel value
	 * 
	 * @param device {@link Dut}
	 * @refactor Athira
	 */
	private void executePostConditionToSetDefaultChannel(Dut device) throws TestException {
		String errorMessage = null;
		boolean status = false;
		try {
			/**
			 * POSTCONDITION :SET AND VERIFY THE DEFAULT CHANNEL VALUE FOR 2.4 GHz USING
			 * WEBPA
			 */
			if (defaultChannel_2_4Ghz != 0) {
				postConStepNumber++;
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": DESCRIPTION : SET AND VERIFY THE DEFAULT CHANNEL VALUE FOR 2.4 GHz.");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": ACTION : SET AND VERIFY THE DEFAULT CHANNEL VALUE FOR 2.4 GHz USING WEBPA PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ);
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": EXPECTED : THE 2.4 GHZ CHANNEL MUST BE SET TO DEFAULT VALUE");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO SET THE DEFAULT CHANNEL VALUE FOR 2.4 GHz.";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ,
						WebPaDataTypes.INTEGER.getValue(), String.valueOf(defaultChannel_2_4Ghz));
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : SUCCESSFULLY SET THE DEFAULT CHANNEL VALUE FOR 2.4 GHz.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
				LOGGER.info("#######################################################################################");
			}
			/**
			 * POSTCONDITION :SET AND VERIFY THE DEFAULT CHANNEL VALUE FOR 5 GHz USING WEBPA
			 */
			if (defaultChannel_5Ghz != 0) {
				postConStepNumber++;
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": DESCRIPTION : SET AND VERIFY THE DEFAULT CHANNEL VALUE FOR 5 GHz.");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": ACTION : SET AND VERIFY THE DEFAULT CHANNEL VALUE FOR 5 GHz USING WEBPA PARAM "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ);
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": EXPECTED : THE 5 GHZ CHANNEL MUST BE SET TO DEFAULT VALUE");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO SET THE DEFAULT CHANNEL VALUE FOR 5 GHz.";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
						WebPaDataTypes.INTEGER.getValue(), String.valueOf(defaultChannel_5Ghz));
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : SUCCESSFULLY SET THE DEFAULT CHANNEL VALUE FOR 5 GHz.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
				LOGGER.info("#######################################################################################");
			}
		} catch (Exception exception) {
			LOGGER.error(
					"Execution error occurred while executing to Set Default Channel post conditions due to exception --> "
							+ exception.getMessage());
		}
	}

	/**
	 * Post-Condition method to change the securoty mode as WPA2-Persional
	 * 
	 * @param device               {@link Dut}
	 * @param isSecModeChanged2Ghz Instance for device security mode changed for 2.4
	 *                             Ghz SSID
	 * @param isSecModeChanged5Ghz Instance for device security mode changed for 5
	 *                             Ghz SSID
	 * @refactor Athira
	 * 
	 */
	private void executePostConditionToSetSecurityModeWPA2Personal(Dut device, boolean isSecModeChanged2Ghz,
			boolean isSecModeChanged5Ghz) throws TestException {
		String errorMessage = null;
		boolean status = false;
		try {
			if (isSecModeChanged2Ghz) {

				/**
				 * POSTCONDITION :SET THE SECURITY MODE 'WPA2-Personal' FOR 2.4 GHZ SSID USING
				 * WEBPA
				 */
				postConStepNumber++;
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POSTCONDITION " + postConStepNumber
						+ " : DESCRIPTION : SET THE SECURITY MODE 'WPA2-Personal' FOR 2.4 GHZ SSID ");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTION : SET THE SECURITY MODE 'WPA2-Personal' FOR 2.4 GHZ SSID USING WEBPA ");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : EXPTECTED : SECURITY MODE 'WPA2-Personal' CHANGED SUCCESSFULLY FOR 2.4 GHZ SSID");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO CHANGE THE SECURITY MODE 'WPA2-Personal' FOR 2.4GHz SSID USING WEBPA. ";
				try {
					status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
							WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
				} catch (TestException exception) {
					errorMessage = errorMessage + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : SECURITY MODE 'WPA2-Personal' CHANGED SUCCESSFULLY FOR 2.4 GHZ SSID.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
			}
			if (isSecModeChanged5Ghz) {
				/**
				 * POSTCONDITION :SET THE SECURITY MODE 'WPA2-Personal' FOR 5 GHZ SSID USING
				 * WEBPA
				 */
				postConStepNumber++;
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POSTCONDITION " + postConStepNumber
						+ " : DESCRIPTION : SET THE SECURITY MODE 'WPA2-Personal' FOR 5 GHZ SSID ");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTION : SET THE SECURITY MODE 'WPA2-Personal' FOR 5 GHZ SSID USING WEBPA ");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : EXPTECTED : SECURITY MODE 'WPA2-Personal' CHANGED SUCCESSFULLY FOR 5 GHZ SSID");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO CHANGE THE SECURITY MODE 'WPA2-Personal' FOR 5GHz SSID USING WEBPA. ";
				try {
					status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
							WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
				} catch (TestException exception) {
					errorMessage = errorMessage + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : SECURITY MODE 'WPA2-Personal' CHANGED SUCCESSFULLY FOR 5 GHZ SSID.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
			}
		} catch (Exception exception) {
			LOGGER.error(
					"Execution error occurred while executing to Set Security Mode WPA2-Personal post conditions due to exception --> "
							+ exception.getMessage());
		}
	}

	/**
	 * Post-Condition method to disconnect the connected clients
	 * 
	 * @param device                  {@link Dut}
	 * @param deviceConnectedWith2Ghz Instance for device connected with 2.4 Ghz
	 *                                SSID
	 * @param deviceConnectedWith5Ghz Instance for device connected with 5 Ghz SSID
	 * @refactor Athira
	 * 
	 */
	private void executePostConditionToDisconnectClients(Dut device, Dut deviceConnectedWith2Ghz,
			Dut deviceConnectedWith5Ghz) throws TestException {
		String errorMessage = null;
		boolean status = false;
		try {
			if (deviceConnectedWith2Ghz != null) {
				/**
				 * POSTCONDITION :DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID
				 * USING WEBPA
				 */
				postConStepNumber++;
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : DESCRIPTION : DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID ");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTION : EXECUTE COMMAND TO DISCONNECT THE CONNECTED CLIENT, FOR WINDOWS :netsh wlan disconnect ,FOR LINUX :nmcli con down id '<ssid>' ");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : EXPTECTED : CONNECTED CLIENT DISCONNECTED SUCCESSFULLY FOR 2.4 GHZ SSID");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO DISCONNECT CONNECTED CLIENT FOR 2.4 GHZ SSID. ";
				try {
					status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith2Ghz, tapEnv,
							BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
									WiFiFrequencyBand.WIFI_BAND_2_GHZ));
				} catch (Exception exception) {
					errorMessage = errorMessage + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : CONNECTED CLIENT DISCONNECTED SUCCESSFULLY FOR 2.4 GHZ SSID.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
				LOGGER.info("#######################################################################################");
			}
			if (deviceConnectedWith5Ghz != null) {
				/**
				 * POSTCONDITION :DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID USING
				 * WEBPA
				 */
				postConStepNumber++;
				status = false;
				errorMessage = null;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : DESCRIPTION : DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID ");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTION : EXECUTE COMMAND TO DISCONNECT THE CONNECTED CLIENT, FOR WINDOWS :netsh wlan disconnect ,FOR LINUX :nmcli con down id '<ssid>' ");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : EXPTECTED : CONNECTED CLIENT DISCONNECTED SUCCESSFULLY FOR 5 GHZ SSID");
				LOGGER.info("#######################################################################################");
				errorMessage = "UNABLE TO DISCONNECT CONNECTED CLIENT FOR 5 GHZ SSID. ";
				try {
					status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith5Ghz, tapEnv,
							BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
									WiFiFrequencyBand.WIFI_BAND_5_GHZ));
				} catch (Exception exception) {
					errorMessage = errorMessage + exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : CONNECTED CLIENT DISCONNECTED SUCCESSFULLY FOR 5 GHZ SSID.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
				LOGGER.info("#######################################################################################");
			}
		} catch (Exception exception) {
			LOGGER.error(
					"Execution error occurred while executing to disconnect client post conditions due to exception --> "
							+ exception.getMessage());
		}
	}

	/**
	 * Method to verify the IPv4 and IPv6 connection interface & Internet
	 * connectivity for 2.4/5 GHz .
	 * 
	 * @param stepNumber Step Number
	 * @param device     {@link Dut}
	 * @param testId     Test case ID
	 * @param device     Device Connected
	 * @refactor Athira
	 */
	public static void verifyIpv4AndIpV6ConnectionInterface(Dut device, String testId, Dut deviceConnected,
			String wifiFrequencyBand) throws TestException {
		LOGGER.debug("STARTING METHOD : verifyIpv4AndIpV6ConnectionInterface()");
		String step = null;
		boolean status = false;
		String errorMessage = null;
		BroadBandResultObject result = null;
		try {
			String wifiBand = wifiFrequencyBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
					? BroadBandTestConstants.BAND_2_4GHZ
					: BroadBandTestConstants.BAND_5GHZ;
			String osType = ((Device) deviceConnected).getOsType();
			String model = ((Device) deviceConnected).getModel();

			/**
			 * Step : VERIFY THE CORRECT IPV4 ADDRESS FOR CONNECTED CLIENT WITH GIVEN GHZ
			 * SSID .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION :VERIFY THE CORRECT IPV4 ADDRESS FOR CONNECTED CLIENT WITH "
							+ wifiBand + " SSID FOR DEVICE MODEL " + model);
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv4 Address' or LINUX : ifconfig | grep 'inet' or ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : IT SHOULD RETURN THE CORRECT IPV4 ADDRESS FOR DEVICE MODEL " + model);
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO GET THE CORRECT IPV4 ADDRESS FROM CLIENT WITH " + wifiBand
					+ " SSID FOR DEVICE MODEL " + model;
			status = BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
					deviceConnected, tapEnv);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFYLLY VERIFIED CORRECT IPV4 ADDRESS FROM CLIENT WITH " + wifiBand
						+ " SSID FOR DEVICE MODEL " + model);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step : VERIFY THE CORRECT IPV6 ADDRESS FOR CONNECTED CLIENT WITH GIVEN GHZ
			 * SSID .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				LOGGER.info("#####################################################################################");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION :VERIFY THE CORRECT IPV6 ADDRESS FOR CONNECTED CLIENT WITH " + wifiBand
						+ "  SSID FOR DEVICE MODEL " + model);
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv6 Address' or LINUX : ifconfig | grep 'inet6 ' ON THE CONNECTED CLIENT");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : IT SHOULD RETURN THE CORRECT IPV4 ADDRESS FOR DEVICE MODEL " + model);
				LOGGER.info("#####################################################################################");
				errorMessage = "UNABLE TO GET THE CORRECT IPV6 ADDRESS FROM CLIENT WITH " + wifiBand
						+ " SSID FOR DEVICE MODEL " + model;
				status = BroadBandConnectedClientUtils
						.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType, deviceConnected, tapEnv);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : SUCCESSFYLLY VERIFIED CORRECT IPV6 ADDRESS FROM CLIENT WITH " + wifiBand
							+ " SSID FOR DEVICE MODEL " + model);
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("#####################################################################################");
				tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : skipping teststep...");
				tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE, errorMessage,
						false);
			}

			/**
			 * Step : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH GIVEN GHZ SSID
			 * INTERFACE USING IPV4 .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT CONNECTED WITH " + wifiBand
					+ " SSID INTERFACE USING IPV4 FOR DEVICE MODEL " + model);
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -4 -n 5 google.com ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV4 ");
			LOGGER.info("#######################################################################################");
			errorMessage = "NOT ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT WITH " + wifiBand
					+ " SSID INTERFACE USING IPV4 FOR DEVICE MODEL " + model;
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
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : CONNECTED CLIENT HAS INTERNET CONNECTIVITY WITH "
						+ wifiBand + " SSID USING IPV4 FOR DEVICE MODEL " + model);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH GIVEN GHZ SSID
			 * INTERFACE USING IPV6 .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
				LOGGER.info("#######################################################################################");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT CONNECTED WITH " + wifiBand
						+ " SSID INTERFACE USING IPV6 FOR DEVICE MODEL " + model);
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -6 -v 'www.google.com' | grep '200 OK' OR ping -6 -n 5 google.com , LINUX : curl -6 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -6 -n 5 google.com ON THE CONNECTED CLIENT");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST BE AVAILABLE INTERFACE USING IPV6 ");
				LOGGER.info("#######################################################################################");
				errorMessage = "NOT ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT WITH " + wifiBand
						+ " SSID INTERFACE USING IPV6 FOR DEVICE MODEL " + model;
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
					LOGGER.info("STEP " + stepNumber + " : ACTUAL : CONNECTED CLIENT HAS INTERNET CONNECTIVITY WITH "
							+ wifiBand + " SSID USING IPV6 FOR DEVICE MODEL " + model);
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("#####################################################################################");
				tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
			} else {
				LOGGER.info("IPv6 is not available/disabled : skipping teststep...");
				tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE, errorMessage,
						false);
			}
		} catch (TestException e) {
			LOGGER.error(errorMessage);
			throw new TestException(errorMessage);
		}
		LOGGER.debug("ENDING METHOD : verifyIpv4AndIpV6ConnectionInterface()");
	}

	/**
	 * Method to verify the IPv4 and IPv6 connection interface & Internet
	 * connectivity for 2.4/5 GHz .
	 * 
	 * @param device            {@link Dut}
	 * @param testId            Test case ID
	 * @param deviceConnected   {@link Dut}
	 * @param wifiFrequencyBand
	 * @param webParameter
	 * @param deviceDateTime
	 * 
	 * @refactor Athira
	 */
	public static void verifyConnectedclientDetails(Dut device, String testId, Dut deviceConnected,
			String wifiFrequencyBand, String webParameter, String deviceDateTime) throws TestException {
		LOGGER.debug("STARTING METHOD : verifyConnectedclientDetails()");
		String step = null;
		boolean status = false;
		String errorMessage = null;
		String response = null;
		try {
			String wifiBand = wifiFrequencyBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
					? BroadBandTestConstants.BAND_2_4GHZ
					: BroadBandTestConstants.BAND_5GHZ;
			String hostName = ((Device) deviceConnected).getConnectedDeviceInfo().getUserName();
			String connectionType = ((Device) deviceConnected).getConnectedDeviceInfo().getConnectionType();
			String hostMacAddress = ((Device) deviceConnected).getConnectedDeviceInfo().getWifiMacAddress();
			LOGGER.info("CONNECTION TYPE :" + connectionType);
			LOGGER.info("HOST NAME :" + hostName);
			LOGGER.info("HOST MACADDRESS :" + hostMacAddress);

			/**
			 * Step : VERIFY NUMBER OF CONNECTED CLIENTS ASSOCIATED WITH GIVEN GHZ .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION :VERIFY NUMBER OF CONNECTED CLIENTS ASSOCIATED WITH "
					+ wifiBand);
			LOGGER.info("STEP " + stepNumber + ": ACTION : GET NUMBER OF CONNECTED CLIENTS ASSOCIATED WITH " + wifiBand
					+ " SSID USING WEBPA PARAM " + webParameter);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : IT SHOULD RETURN THE ASSOCIATED DEVICE COUNT FOR "
					+ wifiBand + " SSID");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO GET THE ASSOCIATED DEVICE COUNT FOR " + wifiBand + " SSID";
			status = BroadBandConnectedClientUtils.verifyAssociatedDeviceCount(device, tapEnv, webParameter,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.CONSTANT_1);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : ASSOCIATED DEVICE COUNT FOR " + wifiBand + " SSID IS :"
						+ status);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH GIVEN
			 * GHZ .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION :VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH " + wifiBand
					+ " SSID");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND :grep -i 'RDKB_CONNECTED_CLIENTS: Client type is <CLIENT_TYPE>, MacAddress is <MAC_ADDRESS> and HostName is <HOST_NAME> appeared online' /rdklogs/logs/LM.txt.0 ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : MUST RETURN THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH " + wifiBand
					+ " SSID");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH " + wifiBand;
			status = BroadBandConnectedClientUtils.verifyConnectedClientDetailsInLMlog(device, tapEnv, connectionType,
					hostMacAddress, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, deviceDateTime);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH "
						+ wifiBand + " SSID");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
		} catch (TestException e) {
			LOGGER.error(errorMessage);
			throw new TestException(errorMessage);
		}
		LOGGER.debug("ENDING METHOD : verifyConnectedclientDetails()");
	}

	/**
	 * Method to verify and set Operating Standard BandWidth and Channel value for
	 * 2.4 GHz
	 * 
	 * @param device {@link Dut}
	 * @param testId Test case ID
	 */
	public boolean verifyAndSetOperStandBandWidthAndChannelValue(Dut device, String testId) {

		String step = null;
		String errorMessage = null;
		try {
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY THE CHANNEL VALUE TO 11 FOR 2.4 GHz");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE CHANNEL VALUE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CHANNEL MUST BE CHANGED TO 11 FOR 2.4 GHz");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO CHANGE THE CHANNEL VALUE AS 11 FOR 2.4 GHz.";

			defaultOperStandardBandWidth = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND);
			LOGGER.info("CURRENT OPERATING CHANNEL BANDWIDTH-" + defaultOperStandardBandWidth);
			Boolean isSpecificDevice = null;
			isSpecificDevice = BroadbandPropertyFileHandler.isSpecificDevice(device);

			if (CommonMethods.isNotNull(defaultOperStandardBandWidth)
					&& !defaultOperStandardBandWidth.equalsIgnoreCase(BroadBandTestConstants.OPERATING_BANDWIDTH_20_MMZ)
					&& (DeviceModeHandler.isFibreDevice(device) || isSpecificDevice
							|| DeviceModeHandler.isBusinessClassDevice(device))) {

				errorMessage = "UNABLE TO CHANGE THE CHANNEL OPERATING BANDWIDTH VALUE AS 20MHz FOR 2.4 GHz";
				bandwidthSetStatus = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND,
						WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.OPERATING_BANDWIDTH_20_MMZ);
				if (bandwidthSetStatus) {
					errorMessage = "UNABLE TO CHANGE THE CHANNEL VALUE AS 11 FOR 2.4 GHz.";
					status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ,
							WebPaDataTypes.INTEGER.getValue(), BroadBandTestConstants.CHANNEL_NO_11);
				}

			} else {

				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ,
						WebPaDataTypes.INTEGER.getValue(), BroadBandTestConstants.CHANNEL_NO_11);
			}
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE CHANNEL VALUE AS 11 FOR 2.4 GHz.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

		} catch (TestException e) {
			LOGGER.error(errorMessage);
			throw new TestException(errorMessage);
		}
		LOGGER.debug("ENDING METHOD : verifyAndSetOperStandBandWidthAndChannelValue()");
		return status;
	}

	/**
	 *
	 * Test Case : Verify that when WIFI Radio is disabled in CM, wireless CPE
	 * should not get an ip address.
	 * 
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Verify 2.4 GHz SSID is enabled</li>
	 * <li>PRE-CONDITION 2 : Verify 5 GHz SSID is enabled</li>
	 * <li>Step 1 : Set and verify 2.4 GHz radio is disabled</li>
	 * <li>Step 2 : Set and verify 5 GHz radio is disabled</li>
	 * <li>Step 3 : Verify the private wifi 2.4 GHz SSID is broadcasting in
	 * connected client</li>
	 * <li>Step 4 : Verify the private wifi 5 GHz SSID is broadcasting in connected
	 * client</li>
	 * <li>Step 5 : Connect the connected client1 in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 6 : Connect the connected client2 in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 7 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 8 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 9 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 10 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 11 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 12 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 13 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 14 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 15 : Set and verify 2.4 GHz radio is enabled</li>
	 * <li>Step 16 : Set and verify 5 GHz radio is enabled</li>
	 * <li>Step 17 : Verify the private wifi 2.4 GHz SSID is broadcasting in
	 * connected client</li>
	 * <li>Step 18 : Verify the private wifi 5 GHz SSID is broadcasting in connected
	 * client</li>
	 * <li>Step 19 : Connect the connected client1 in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 20 : Connect the connected client2 in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 21 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 22 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 23 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 24 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 25 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 26 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 26 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 28 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 29 : Verify disconnecting the 2.4 GHz private wifi SSID</li>
	 * <li>Step 30 : Verify disconnecting the 5 GHz private wifi SSID</li>
	 * <li>POST-CONDITION 1 : Verify 2.4 GHz SSID is enabled</li>
	 * <li>POST-CONDITION 2 : Verify 5 GHz SSID is enabled</li>
	 * </ol>
	 * 
	 * @author Muthukumar
	 * @refactor Govardhan
	 * 
	 * @param device instance of {@link Dut}
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-WIFI-CC-IP-5001")
	public void testToVerifyIpAddressWhenWIFIRadioIsDisabledInCM(Dut device) {
		String testId = "TC-RDKB-WIFI-CC-IP-501";
		stepNumber = 1;
		preConStepNumber = 1;
		postConStepNumber = 1;
		String step = "S" + stepNumber;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWith2Ghz = null;
		Dut deviceConnectedWith5Ghz = null;
		BroadBandResultObject result = null;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CC-IP-5001");
			LOGGER.info(
					"TEST DESCRIPTION: Verify that when WIFI Radio is disabled in CM, wireless CPE should not get an ip address.");
			LOGGER.info("TEST STEPS : ");

			LOGGER.info(" 1 : Set and verify 2.4 GHz radio is disabled ");
			LOGGER.info(" 2 : Set and verify 5 GHz radio is disabled ");
			LOGGER.info(" 3 : Verify the private wifi 2.4 GHz SSID is broadcasting in connected client ");
			LOGGER.info(" 4 : Verify the private wifi 5 GHz  SSID is broadcasting in connected client ");
			LOGGER.info(
					" 5 : Connect  the connected client1  in the setup to 2.4 GHz SSID and verify connection status ");
			LOGGER.info(
					" 6 : Connect  the connected client2  in the setup to 5 GHz SSID and verify connection status ");
			LOGGER.info(" 7 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(" 8 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(
					" 9 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 10 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 11 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
			LOGGER.info(" 12 : Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
			LOGGER.info(
					" 13 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
			LOGGER.info(
					" 14 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
			LOGGER.info(" 15 : Set and verify 2.4 GHz radio is enabled ");
			LOGGER.info(" 16 : Set and verify 5 GHz radio is enabled ");
			LOGGER.info(" 17 : Verify the private wifi 2.4 GHz SSID is broadcasting in connected client ");
			LOGGER.info(" 18 : Verify the private wifi 5 GHz  SSID is broadcasting in connected client ");
			LOGGER.info(
					" 19 : Connect  the connected client1  in the setup to 2.4 GHz SSID and verify connection status ");
			LOGGER.info(
					" 20 : Connect  the connected client2  in the setup to 5 GHz SSID and verify connection status ");
			LOGGER.info(" 21 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(" 22 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(
					" 23 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 24 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 25 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
			LOGGER.info(" 26 : Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
			LOGGER.info(
					" 27 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
			LOGGER.info(
					" 28 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
			LOGGER.info(" 29 : Verify disconnecting the 2.4 GHz private wifi SSID ");
			LOGGER.info(" 30 : Verify disconnecting the 5 GHz private wifi SSID ");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			BroadBandPreConditionUtils.executePreConditionToVerifyRadioStatus(device, tapEnv, preConStepNumber);
			LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");
			/**
			 * Step 1 : SET AND VERIFY 2.4 GHZ RADIO IS DISABLED
			 */
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY 2.4 GHZ RADIO IS DISABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY 2.4 GHZ RADIO IS DISABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 2.4 GHZ RADIO MUST BE DISABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO DISABLE THE 2.4 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY DISABLED THE 2.4 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 2 : SET AND VERIFY 5 GHZ RADIO IS DISABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY 5 GHZ RADIO IS DISABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY 5 GHZ RADIO IS DISABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 5 GHZ RADIO MUST BE DISABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO DISABLE THE 5 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY DISABLED THE 5 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 3 : VERIFY THE PRIVATE WIFI 2.4 GHZ SSID IS BROADCASTING IN CONNECTED
			 * CLIENT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE PRIVATE WIFI 2.4 GHZ  SSID IS BROADCASTING IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS DISABLED ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND FOR WINDOWS: 'NETSH WLAN SHOW NETWORKS | GREP -I '<PRIVATE_SSID_2GHZ>' OR LINUX : 'SUDO IWLIST WLAN0 SCAN' ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : PRIVATE WIFI 2.4 GHZ SSID SHOULD NOT BE BROADCASTED IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS DISABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY PRIVATE WIFI 2.4 GHZ SSID BROADCASTING STATUS IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS DISABLED";
			List<Dut> listOfDevices = ((Device) device).getConnectedDeviceList();
			deviceConnectedWith2Ghz = BroadBandConnectedClientUtils.getConnectedClientBasedOnTypeAndBand(device, tapEnv,
					listOfDevices, BroadBandTestConstants.WIFI, BroadBandTestConstants.BAND_2_4GHZ);
			status = BroadBandWiFiUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDeviceWithPollDuration(device,
					tapEnv, deviceConnectedWith2Ghz, WiFiFrequencyBand.WIFI_BAND_2_GHZ, false,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : PRIVATE WIFI 2.4 GHZ SSID NOT BROADCASTED IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS DISABLED.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 4 : VERIFY THE PRIVATE WIFI 5 GHZ SSID IS BROADCASTING IN CONNECTED
			 * CLIENT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE PRIVATE WIFI 5 GHZ  SSID IS BROADCASTING IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS DISABLED ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND FOR WINDOWS: 'NETSH WLAN SHOW NETWORKS | GREP -I '<PRIVATE_SSID_2GHZ>' OR LINUX : 'SUDO IWLIST WLAN0 SCAN' ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : PRIVATE WIFI 5 GHZ SSID SHOULD NOT BE BROADCASTED IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS DISABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY PRIVATE WIFI 5 GHZ SSID BROADCASTING STATUS IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS DISABLED";
			deviceConnectedWith5Ghz = BroadBandConnectedClientUtils.getOtherConnectedClient(deviceConnectedWith2Ghz,
					tapEnv, listOfDevices, BroadBandTestConstants.WIFI, BroadBandTestConstants.BAND_5GHZ);
			status = BroadBandWiFiUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDeviceWithPollDuration(device,
					tapEnv, deviceConnectedWith5Ghz, WiFiFrequencyBand.WIFI_BAND_5_GHZ, false,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : PRIVATE WIFI 5 GHZ SSID NOT BROADCASTED IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS DISABLED.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 5 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHZ SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : CONNECT  THE CONNECTED CLIENT  IN THE SETUP TO 2.4 GHZ SSID AND VERIFY CONNECTION STATUS");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST NOT BE SUCCESSFUL FOR 2.4 GHZ SSID  AND PASSWORD");
			LOGGER.info("#####################################################################################");
			errorMessage = "CONNECTION SUCCESSFUL FOR 2.4 GHZ SSID WHEN 2.4 GHZ RADIO IS DISABLED";
			status = !BroadBandConnectedClientUtils.connectGivenWiFiCapableClientAndConnectWithGivenWiFiBand(device,
					tapEnv, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_2_4GHZ);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTION FAILED FOR FOR 2.4 GHZ SSID WHEN 2.4 GHZ RADIO IS DISABLED .");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 6 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHZ SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : CONNECT  THE CONNECTED CLIENT  IN THE SETUP TO 5 GHZ SSID AND VERIFY CONNECTION STATUS");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST NOT BE SUCCESSFUL FOR 5 GHZ SSID  AND PASSWORD");
			LOGGER.info("#####################################################################################");
			errorMessage = "CONNECTION SUCCESSFUL FOR 5 GHZ SSID WHEN 5 GHZ RADIO IS DISABLED";
			status = !BroadBandConnectedClientUtils.connectGivenWiFiCapableClientAndConnectWithGivenWiFiBand(device,
					tapEnv, deviceConnectedWith5Ghz, BroadBandTestConstants.BAND_5GHZ);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTION FAILED FOR FOR 5 GHZ SSID WHEN 5 GHZ RADIO IS DISABLED .");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 7 : VERIFY THE IPV4 ADDRESS FOR CLIENT 2.4 GHZ SSID WHEN 2.4 GHZ RADIO
			 * IS DISABLED .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE IPV4 ADDRESS FOR CLIENT 2.4 GHz SSID WHEN 2.4 GHZ RADIO IS DISABLED");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv4 Address' or LINUX : ifconfig | grep 'inet' or ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : IT SHOULD NOT RETURN THE IPV4 ADDRESS WHEN 2.4 GHZ RADIO IS DISABLED ");
			LOGGER.info("#####################################################################################");
			errorMessage = "ABLE TO GET THE CORRECT IPV4 ADDRESS FROM CLIENT 2.4 GHz SSID WHEN 2.4 GHZ RADIO IS DISABLED";
			status = !BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
					((Device) deviceConnectedWith2Ghz).getOsType(), deviceConnectedWith2Ghz, tapEnv);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : UNABLE TO GET IPV4 ADDRESS WHEN 2.4 GHZ RADIO IS DISABLED ");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 8 : VERIFY THE IPV6 ADDRESS FOR CLIENT 2.4 GHZ SSID WHEN 2.4 GHZ RADIO
			 * IS DISABLED .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE IPV6 ADDRESS FOR CLIENT 2.4 GHz SSID WHEN 2.4 GHZ RADIO IS DISABLED");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv4 Address' or LINUX : ifconfig | grep 'inet' or ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : IT SHOULD NOT RETURN THE IPV6 ADDRESS WHEN 2.4 GHZ RADIO IS DISABLED ");
			LOGGER.info("#####################################################################################");
			errorMessage = "ABLE TO GET THE CORRECT IPV6 ADDRESS FROM CLIENT 2.4 GHz SSID WHEN 2.4 GHZ RADIO IS DISABLED";
			status = !BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
					((Device) deviceConnectedWith2Ghz).getOsType(), deviceConnectedWith2Ghz, tapEnv);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : UNABLE TO GET IPV6 ADDRESS WHEN 2.4 GHZ RADIO IS DISABLED ");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 9: VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 2.4 GHZ SSID
			 * INTERFACE USING IPV4 .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 2.4 GHz SSID INTERFACE USING IPV4");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -4 -n 5 google.com ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST NOT BE AVAILABLE INTERFACE USING IPV4 ");
			LOGGER.info("#######################################################################################");
			errorMessage = " ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT 2.4GHz SSID INTERFACE USING IPV4 ";
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					deviceConnectedWith2Ghz,
					BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
					BroadBandTestConstants.IP_VERSION4);
			status = !result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTED CLIENT NOT HAVING INTERNET CONNECTIVITY WITH 2.4GHz SSID USING IPV4");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 10: VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 2.4 GHZ SSID
			 * INTERFACE USING IPV6.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 2.4 GHz SSID INTERFACE USING IPV6");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -6 -v 'www.google.com' | grep '200 OK' OR ping -6 -n 5 google.com , LINUX : curl -6 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -6 -n 5 google.com ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST NOT BE AVAILABLE INTERFACE USING IPV6 ");
			LOGGER.info("#######################################################################################");
			errorMessage = " ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT 2.4GHz SSID INTERFACE USING IPV6 ";
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					deviceConnectedWith2Ghz,
					BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
					BroadBandTestConstants.IP_VERSION6);
			status = !result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTED CLIENT NOT HAVING INTERNET CONNECTIVITY WITH 2.4GHz SSID USING IPV6");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 11 : VERIFY THE IPV4 ADDRESS FOR CLIENT 5 GHZ SSID WHEN 5 GHZ RADIO IS
			 * DISABLED .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE IPV4 ADDRESS FOR CLIENT 5 GHz SSID WHEN 5 GHZ RADIO IS DISABLED");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv4 Address' or LINUX : ifconfig | grep 'inet' or ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : IT SHOULD NOT RETURN THE IPV4 ADDRESS WHEN 5 GHZ RADIO IS DISABLED ");
			LOGGER.info("#####################################################################################");
			errorMessage = "ABLE TO GET THE CORRECT IPV4 ADDRESS FROM CLIENT 5 GHz SSID WHEN 5 GHZ RADIO IS DISABLED";
			status = !BroadBandConnectedClientUtils.verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
					((Device) deviceConnectedWith5Ghz).getOsType(), deviceConnectedWith5Ghz, tapEnv);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : UNABLE TO GET IPV4 ADDRESS WHEN 5 GHZ RADIO IS DISABLED ");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 12 : VERIFY THE IPV6 ADDRESS FOR CLIENT 5 GHZ SSID WHEN 5 GHZ RADIO IS
			 * DISABLED .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE IPV6 ADDRESS FOR CLIENT 5 GHz SSID WHEN 5 GHZ RADIO IS DISABLED");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv4 Address' or LINUX : ifconfig | grep 'inet' or ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : IT SHOULD NOT RETURN THE IPV6 ADDRESS WHEN 5 GHZ RADIO IS DISABLED ");
			LOGGER.info("#####################################################################################");
			errorMessage = "ABLE TO GET THE CORRECT IPV6 ADDRESS FROM CLIENT 5 GHz SSID WHEN 5 GHZ RADIO IS DISABLED";
			status = !BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(
					((Device) deviceConnectedWith5Ghz).getOsType(), deviceConnectedWith5Ghz, tapEnv);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : UNABLE TO GET IPV6 ADDRESS WHEN 5 GHZ RADIO IS DISABLED ");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 13: VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 5 GHZ SSID
			 * INTERFACE USING IPV4 .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 5 GHz SSID INTERFACE USING IPV4");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -4 -v 'www.google.com'  | grep '200 OK' OR ping -4 -n 5 google.com, LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -4 -n 5 google.com ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST NOT BE AVAILABLE INTERFACE USING IPV4 ");
			LOGGER.info("#######################################################################################");
			errorMessage = " ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT 5GHz SSID INTERFACE USING IPV4 ";
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					deviceConnectedWith5Ghz,
					BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
					BroadBandTestConstants.IP_VERSION4);
			status = !result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTED CLIENT NOT HAVING INTERNET CONNECTIVITY WITH 5GHz SSID USING IPV4");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 14: VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 5 GHZ SSID
			 * INTERFACE USING IPV6.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 5 GHz SSID INTERFACE USING IPV6");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND, WINDOWS : curl -6 -v 'www.google.com' | grep '200 OK' OR ping -6 -n 5 google.com , LINUX : curl -6 -f --interface <interfaceName> www.google.com | grep '200 OK' OR ping -6 -n 5 google.com ON THE CONNECTED CLIENT");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE INTERNET CONNECTIVITY MUST NOT BE AVAILABLE INTERFACE USING IPV6 ");
			LOGGER.info("#######################################################################################");
			errorMessage = " ABLE TO ACCESS THE SITE 'www.google.com' FROM CONNECTED CLIENT 5GHz SSID INTERFACE USING IPV6 ";
			result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
					deviceConnectedWith5Ghz,
					BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
					BroadBandTestConstants.IP_VERSION6);
			status = !result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTED CLIENT NOT HAVING INTERNET CONNECTIVITY WITH 2.4GHz SSID USING IPV6");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 15 : SET AND VERIFY 2.4 GHZ RADIO IS ENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY 2.4 GHZ RADIO IS ENABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY 2.4 GHZ RADIO IS ENABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 2.4 GHZ RADIO MUST BE ENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO ENABLE THE 2.4 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY ENABLED THE 2.4 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 16 : SET AND VERIFY 5 GHZ RADIO IS ENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY 5 GHZ RADIO IS ENABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY 5 GHZ RADIO IS ENABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 5 GHZ RADIO MUST BE ENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO ENABLE THE 5 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY ENABLED THE 5 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 17 : VERIFY THE PRIVATE WIFI 2.4 GHZ SSID IS BROADCASTING IN CONNECTED
			 * CLIENT,WHEN 2.4 GHZ RADIO IS REENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE PRIVATE WIFI 2.4 GHZ  SSID IS BROADCASTING IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS REENABLED ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND FOR WINDOWS: 'NETSH WLAN SHOW NETWORKS | GREP -I '<PRIVATE_SSID_2GHZ>' OR LINUX : 'SUDO IWLIST WLAN0 SCAN' ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : PRIVATE WIFI 2.4 GHZ SSID SHOULD BE BROADCASTED IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS REENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY PRIVATE WIFI 2.4 GHZ SSID BROADCASTING STATUS IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS REENABLED";
			status = BroadBandWiFiUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDeviceWithPollDuration(device,
					tapEnv, deviceConnectedWith2Ghz, WiFiFrequencyBand.WIFI_BAND_2_GHZ, true,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : PRIVATE WIFI 2.4 GHZ SSID IS BROADCASTED IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS REENABLED.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 18 : VERIFY THE PRIVATE WIFI 5 GHZ SSID IS BROADCASTING IN CONNECTED
			 * CLIENT,WHEN 5 GHZ RADIO IS REENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE PRIVATE WIFI 5 GHZ  SSID IS BROADCASTING IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS REENABLED ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND FOR WINDOWS: 'NETSH WLAN SHOW NETWORKS | GREP -I '<PRIVATE_SSID_2GHZ>' OR LINUX : 'SUDO IWLIST WLAN0 SCAN' ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : PRIVATE WIFI 5 GHZ SSID SHOULD BE BROADCASTED IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS REENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY PRIVATE WIFI 5 GHZ SSID BROADCASTING STATUS IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS REENABLED";
			status = BroadBandWiFiUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDeviceWithPollDuration(device,
					tapEnv, deviceConnectedWith5Ghz, WiFiFrequencyBand.WIFI_BAND_5_GHZ, true,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : PRIVATE WIFI 5 GHZ SSID IS BROADCASTED IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS REENABLED.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 19 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 2.4GHz SSID";
			status = BroadBandConnectedClientUtils.connectGivenWiFiCapableClientAndConnectWithGivenWiFiBand(device,
					tapEnv, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_2_4GHZ);
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 2.4GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 20 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHZ SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : CONNECT  THE CONNECTED CLIENT  IN THE SETUP TO 5 GHZ SSID AND VERIFY CONNECTION STATUS");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST NOT BE SUCCESSFUL FOR 5 GHZ SSID  AND PASSWORD");
			LOGGER.info("#####################################################################################");
			errorMessage = "CONNECTION SUCCESSFUL FOR 5 GHZ SSID WHEN 5 GHZ RADIO IS DISABLED";
			status = BroadBandConnectedClientUtils.connectGivenWiFiCapableClientAndConnectWithGivenWiFiBand(device,
					tapEnv, deviceConnectedWith5Ghz, BroadBandTestConstants.BAND_5GHZ);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTION FAILED FOR FOR 5 GHZ SSID WHEN 5 GHZ RADIO IS DISABLED .");
			} else {
				deviceConnectedWith2Ghz = null;
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
			/**
			 * SETP 21- 24
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					stepNumber);
			/**
			 * SETP 25- 28
			 */
			stepNumber = 25;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					stepNumber);
			/**
			 * Step 29: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHZ
			 */
			stepNumber = 29;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 2.4GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 2.4GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith2Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ));
			if (status) {
				deviceConnectedWith2Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 2.4GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 30: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5
			 * GHZ
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 5GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 5GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith5Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ));
			if (status) {
				deviceConnectedWith5Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 5GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE EXECUTING TEST CASE 'TC-RDKB-WIFI-CC-IP-5001' : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			BroadBandPostConditionUtils.executePostConditionToVerifyDefaultRadioStatus(device, tapEnv,
					postConStepNumber);
			postConStepNumber = 2;
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith2Ghz(device, tapEnv,
					deviceConnectedWith2Ghz, postConStepNumber);
			postConStepNumber = 3;
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith5Ghz(device, tapEnv,
					deviceConnectedWith5Ghz, postConStepNumber);

			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CC-IP-5001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 *
	 * Test Case : Verify enable one radio & disable another radio and check the
	 * internet connectivity
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : PERFORM FACTORY RESET ON THE DEVICE</li>
	 * <li>PRE-CONDITION 2 : REACTIVATE THE ROUTER DEVICE</li>
	 * <li>PRE-CONDITION 3 : GET THE DEFAULT VALUE FOR OPERATING STANDARD 2.4
	 * GHZ</li>
	 * <li>PRE-CONDITION 4 : GET THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ</li>
	 * <li>Step 1 : Verify the 2.4Ghz radio Automatic Channel selection is enabled
	 * by default</li>
	 * <li>Step 2 : Verify the 5Ghz radio Automatic Channel selection is enabled by
	 * default</li>
	 * <li>Step 3 : Verify for 2.4Ghz radio is enabled by default</li>
	 * <li>Step 4 : Verify for 5Ghz radio is enabled by default</li>
	 * <li>Step 5 : Set and verify the value of operational transmission rate of the
	 * 2.4 GHz with operating standard as g/n</li>
	 * <li>Step 6 : Set and verify the value of operational transmission rate of the
	 * 5GHz with operating standard as a,n,ac</li>
	 * <li>Step 7 : Verify 5 GHz radio is disabled</li>
	 * <li>Step 8 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 9 : Verify number of clients associated with 2.4 GHz using
	 * webpa</li>
	 * <li>Step 10 : Verify the client mac address and connection type connected
	 * with 2.4 GHz in LM.txt.0 log file</li>
	 * <li>Step 11 : Verify the connected clients details are IP and MAC in
	 * Gateway</li>
	 * <li>Step 12 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 13 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 14 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 15 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 16 : Set and verify new SSID name is broadcasted for 2.4 GHz</li>
	 * <li>Step 17 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 18 : Verify number of clients associated with 2.4 GHz using
	 * webpa</li>
	 * <li>Step 19 : Verify the client mac address and connection type connected
	 * with 2.4 GHz in LM.txt.0 log file</li>
	 * <li>Step 20 : Verify the connected clients details are IP and MAC in
	 * Gateway</li>
	 * <li>Step 21 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 22 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 23 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 24 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 25 : Verify 5 GHz radio is reenabled</li>
	 * <li>Step 26 : Verify 2.4 GHz radio is disabled</li>
	 * <li>Step 27 : Connect the connected client in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 28 : Verify number of clients associated with 5 GHz using webpa</li>
	 * <li>Step 29 : Verify the client mac address and connection type connected
	 * with 5 GHz in LM.txt.0 log file</li>
	 * <li>Step 30 : Verify the connected clients details are IP and MAC in
	 * Gateway</li>
	 * <li>Step 31 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 32 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 33 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 34 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 35 : Set and verify new SSID name is broadcasted for 5 GHz</li>
	 * <li>Step 36 : Connect the connected client in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 37 : Verify number of clients associated with 5 GHz using webpa</li>
	 * <li>Step 38 : Verify the client mac address and connection type connected
	 * with 5 GHz in LM.txt.0 log file</li>
	 * <li>Step 39 : Verify the connected clients details are IP and MAC in
	 * Gateway</li>
	 * <li>Step 40 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 41 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 42 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 43 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 44 : Verify 2.4 GHz radio is reenabled</li>
	 * <li>Step 45 : Verify disconnecting the 2.4 GHz private wifi SSID</li>
	 * <li>Step 46 : Verify disconnecting the 5 GHz private wifi SSID</li>
	 * 
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Muthukumar
	 * @refactor Athira
	 * 
	 **/
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-WIFI-CC-RADIO-5001")
	public void testToVerifyInternetConnectionByEnableOneDisableAnotherRadios(Dut device) {
		String testId = "TC-RDKB-WIFI-CC-RADIO-501";
		String step = null;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWith2Ghz = null;
		Dut deviceConnectedWith5Ghz = null;
		stepNumber = 1;
		preConStepNumber = 0;
		postConStepNumber = 0;
		step = "S" + stepNumber;
		String deviceDateTime = null;
		isReactivated = false;
		defaultOperStandard_2_4Ghz = null;
		defaultOperStandard_5Ghz = null;
		String defaultSsidFor_2_4Ghz = null;
		String defaultSsidFor_5Ghz = null;
		try {

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CC-RADIO-5001");
			LOGGER.info(
					"TEST DESCRIPTION: Verify enable one radio & disable another radio and check the internet connectivity.");

			LOGGER.info("TEST STEPS : ");
			LOGGER.info(" 1 : Verify the 2.4Ghz radio Automatic Channel selection is enabled by default");
			LOGGER.info(" 2 : Verify the 5Ghz radio Automatic Channel selection is enabled by default");
			LOGGER.info(" 3 : Verify for 2.4Ghz radio is enabled by default");
			LOGGER.info(" 4 : Verify for 5Ghz radio is enabled by default");
			LOGGER.info(
					" 5 : Set and verify  the value of operational transmission rate of the 2.4 GHz with operating standard as g/n");
			LOGGER.info(
					" 6 : Set and verify  the value of operational transmission rate of the 5GHz with operating standard as a,n,ac");
			LOGGER.info(" 7 : Verify 5 GHz radio is disabled ");
			LOGGER.info(
					" 8 : Connect  the connected client  in the setup to 2.4 GHz SSID and verify connection status");
			LOGGER.info(" 9 : Verify number of  clients associated with  2.4 GHz using webpa");
			LOGGER.info(
					" 10 : Verify the client mac address and connection type connected with 2.4 GHz in LM.txt.0 log file");
			LOGGER.info(" 11 : Verify the connected clients details are IP and MAC in Gateway");
			LOGGER.info(" 12 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(" 13 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(
					" 14 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 15 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 16 : Set and verify new SSID name is broadcasted for  2.4 GHz ");
			LOGGER.info(
					" 17 : Connect  the connected client  in the setup to 2.4 GHz SSID and verify connection status");
			LOGGER.info(" 18 : Verify number of  clients associated with  2.4 GHz using webpa");
			LOGGER.info(
					" 19 : Verify the client mac address and connection type connected with 2.4 GHz in LM.txt.0 log file");
			LOGGER.info(" 20 : Verify the connected clients details are IP and MAC in Gateway");
			LOGGER.info(" 21 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(" 22 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(
					" 23 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 24 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 25 : Verify 5 GHz radio is reenabled");
			LOGGER.info(" 26 : Verify 2.4 GHz radio is disabled ");
			LOGGER.info(" 27 : Connect  the connected client  in the setup to 5 GHz SSID and verify connection status");
			LOGGER.info(" 28 : Verify number of  clients associated with 5 GHz using webpa");
			LOGGER.info(
					" 29 : Verify the client mac address and connection type connected with 5 GHz in LM.txt.0 log file");
			LOGGER.info(" 30 : Verify the connected clients details are IP and MAC in Gateway");
			LOGGER.info(" 31 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
			LOGGER.info(" 32 : Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
			LOGGER.info(
					" 33 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
			LOGGER.info(
					" 34 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
			LOGGER.info(" 35 : Set and verify new SSID name is broadcasted for  5 GHz ");
			LOGGER.info(" 36 : Connect  the connected client  in the setup to 5 GHz SSID and verify connection status");
			LOGGER.info(" 37 : Verify number of  clients associated with 5 GHz using webpa");
			LOGGER.info(
					" 38 : Verify the client mac address and connection type connected with 5 GHz in LM.txt.0 log file");
			LOGGER.info(" 39 : Verify the connected clients details are IP and MAC in Gateway");
			LOGGER.info(" 40 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
			LOGGER.info(" 41 : Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
			LOGGER.info(
					" 42 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
			LOGGER.info(
					" 43 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
			LOGGER.info(" 44 : Verify 2.4 GHz radio is reenabled ");
			LOGGER.info(" 45 : Verify disconnecting the 2.4 GHz private wifi SSID ");
			LOGGER.info(" 46 : Verify disconnecting the 5 GHz private wifi SSID ");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			executePreConditionToFactoryResetAndReacitivateDevice(device);
			executePreConditionToGetDefaultOperStandard(device);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1 : VERIFY THE 2.4GHZ RADIO AUTOMATIC CHANNEL SELECTION IS ENABLED BY
			 * DEFAULT
			 * 
			 */

			errorMessage = null;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE 2.4GHZ RADIO AUTOMATIC CHANNEL SELECTION IS ENABLED BY DEFAULT.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : VERIFY THE 2.4GHZ RADIO AUTOMATIC CHANNEL SELECTION IS ENABLED BY DEFAULT USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_CHANNEL_SELECTION_MODE);
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : MUST RETURN THE DEFAULT STATUS AS 'TRUE' 2.4GHZ RADIO AUTOMATIC CHANNEL SELECTION");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO RETIREVE THE DEFAULT VALUE FOR 2.4GHZ RADIO AUTOMATIC CHANNEL SELECTION.";
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_CHANNEL_SELECTION_MODE, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY RETIREVED THE FOR 2.4GHZ RADIO AUTOMATIC CHANNEL SELECTION.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 2 : VERIFY THE 5GHZ RADIO AUTOMATIC CHANNEL SELECTION IS ENABLED BY
			 * DEFAULT
			 * 
			 */
			stepNumber++;
			step = "S" + stepNumber;
			errorMessage = null;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE 5GHZ RADIO AUTOMATIC CHANNEL SELECTION IS ENABLED BY DEFAULT.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : VERIFY THE 5GHZ RADIO AUTOMATIC CHANNEL SELECTION IS ENABLED BY DEFAULT USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_CHANNEL_SELECTION_MODE);
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : MUST RETURN THE DEFAULT STATUS AS 'TRUE' 5GHZ RADIO AUTOMATIC CHANNEL SELECTION");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO RETIREVE THE DEFAULT VALUE FOR 5GHZ RADIO AUTOMATIC CHANNEL SELECTION.";
			if (!DeviceModeHandler.isRPIDevice(device)) {
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_CHANNEL_SELECTION_MODE, BroadBandTestConstants.TRUE);
			} else {
				status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_CHANNEL_SELECTION_MODE, BroadBandTestConstants.FALSE);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY RETIREVED THE FOR 5GHZ RADIO AUTOMATIC CHANNEL SELECTION.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 3 : VERIFY FOR 2.4GHZ RADIO IS ENABLED BY DEFAULT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY FOR 2.4GHZ RADIO IS ENABLED BY DEFAULT");
			LOGGER.info(
					"STEP " + stepNumber + ": ACTION : VERIFY FOR 2.4GHZ RADIO IS ENABLED BY DEFAULT USING WEBPA PARAM "
							+ BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN THE DEFAULT STATUS AS 'TRUE'");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO RETIREVE THE DEFAULT VALUE AS 'TRUE' FOR 2.4 GHZ RADIO.";
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY RETIREVED THE DEFAULT STATUS AS 'TRUE' FOR 2.4 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 4 : VERIFY FOR 5GHZ RADIO IS ENABLED BY DEFAULT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY FOR 5GHZ RADIO IS ENABLED BY DEFAULT");
			LOGGER.info(
					"STEP " + stepNumber + ": ACTION : VERIFY FOR 5GHZ RADIO IS ENABLED BY DEFAULT USING WEBPA PARAM "
							+ BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN THE DEFAULT STATUS AS 'TRUE'");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO RETIREVE THE DEFAULT VALUE AS 'TRUE' FOR 5 GHZ RADIO.";
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY RETIREVED THE DEFAULT STATUS AS 'TRUE' FOR 5 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 5 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4
			 * GHZ WITH OPERATING STANDARD AS g/n
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4 GHZ WITH OPERATING STANDARD AS g/n.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET TO g/n");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET OPERATING STANDARD AS g/n.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
					WebPaDataTypes.STRING.getValue(), WifiOperatingStandard.OPERATING_STANDARD_G_N.getOperatingmode());
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS g/n.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 6 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 5
			 * GHZ WITH OPERATING STANDARD AS a,n,ac
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 5 GHZ WITH OPERATING STANDARD AS a,n,ac.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET TO a,n,ac");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET OPERATING STANDARD AS a,n,ac.";
			if (!DeviceModeHandler.isRPIDevice(device)) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(),
						WifiOperatingStandard.OPERATING_STANDARD_A_N_AC.getOperatingmode());
			} else {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(), BroadbandPropertyFileHandler.get5GhzOperatingModeForRPi());
			}
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS a,n,ac.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 7 : VERIFY 5 GHZ RADIO IS DISABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY 5 GHZ RADIO IS DISABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : VERIFY 5 GHZ RADIO IS DISABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 5 GHZ RADIO MUST BE DISABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO DISABLE THE 5 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : "
					+ (status ? " SUCCESSFYLLY DISABLED THE 5 GHZ RADIO" : errorMessage));
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 8 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 2.4GHz SSID";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			try {
				deviceConnectedWith2Ghz = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWith2Ghz != null);
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 2.4GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 9-10 : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
			 * /rdklogs/logs/LM.txt.0 LOG
			 */
			verifyConnectedclientDetails(device, testId, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_2_4GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_ASSOCIATED_DEVICES, deviceDateTime);
			/**
			 * STEP 11 : VERIFY 2GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			verifyClientMacInGateway(device, testId, deviceConnectedWith2Ghz, WiFiFrequencyBand.WIFI_BAND_2_GHZ,
					stepNumber);

			/**
			 * SETP : 12-15
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					BroadBandTestConstants.BAND_2_4GHZ);

			/**
			 * SETP 16 : SET AND VERIFY NEW SSID NAME IS BROADCASTED FOR 2.4 GHZ IN
			 * CONNECTED CLIENT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY NEW SSID NAME IS BROADCASTED FOR 2.4 GHZ IN CONNECTED CLIENT ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY NEW SSID NAME IS BROADCASTED FOR 2.4 GHZ IN CONNECTED CLIENT USING WEBPA ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : NEW SSID NAME IS BROADCASTED FOR 2.4 GHZ IN CONNECTED CLIENT ");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE SET NEW SSID NAME FOR 2.4 GHZ IN CONNECTED CLIENT";
			defaultSsidFor_2_4Ghz = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device,
					tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.PRIVATEWIFI_NAME_FOR_2GHZ_BAND)) {
				errorMessage = "UNABLE TO VERIFY THE NEW SSID NAME IS BROADCASTED FOR 2.4 GHZ IN CONNECTED CLIENT";
				status = BroadBandWiFiUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDeviceWithPollDuration(
						device, tapEnv, deviceConnectedWith2Ghz, WiFiFrequencyBand.WIFI_BAND_2_GHZ, true,
						BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : NEW SSID NAME IS BROADCASTED FOR 2.4 GHZ IN CONNECTED CLIENT.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
			/**
			 * Step 17 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 2.4GHz SSID";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			status = BroadBandConnectedClientUtils.connectGivenWiFiCapableClientAndConnectWithGivenWiFiBand(device,
					tapEnv, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_2_4GHZ);
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 2.4GHz SSID SUCCESSFULLY");
			} else {
				deviceConnectedWith2Ghz = null;
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 18-19 : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
			 * /rdklogs/logs/LM.txt.0 LOG
			 */
			verifyConnectedclientDetails(device, testId, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_2_4GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_ASSOCIATED_DEVICES, deviceDateTime);
			/**
			 * STEP 20 : VERIFY 5GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			verifyClientMacInGateway(device, testId, deviceConnectedWith2Ghz, WiFiFrequencyBand.WIFI_BAND_2_GHZ,
					stepNumber);

			/**
			 * SETP : 21-24
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					BroadBandTestConstants.BAND_2_4GHZ);

			/**
			 * Step 25: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHZ
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 2.4GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 2.4GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith2Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ));
			if (status) {
				deviceConnectedWith2Ghz = null;
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL: SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 2.4GHZ SSID");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 26 : VERIFY 5 GHZ RADIO IS REENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY 5 GHZ RADIO IS REENABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : VERIFY 5 GHZ RADIO IS REENABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 5 GHZ RADIO MUST BE REENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO REENABLE THE 5 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY REENABLED THE 5 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 27 : SET AND VERIFY 2.4 GHZ RADIO IS DISABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY 2.4 GHZ RADIO IS DISABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY 2.4 GHZ RADIO IS DISABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 2.4 GHZ RADIO MUST BE DISABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO DISABLE THE 2.4 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY DISABLED THE 2.4 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 28 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 5 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 5GHz SSID";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			try {
				deviceConnectedWith5Ghz = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWith5Ghz != null);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 5GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 29-30 : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
			 * /rdklogs/logs/LM.txt.0 LOG
			 */
			verifyConnectedclientDetails(device, testId, deviceConnectedWith5Ghz, BroadBandTestConstants.BAND_5GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_ASSOCIATED_DEVICES, deviceDateTime);

			/**
			 * STEP 31 : VERIFY 5GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			verifyClientMacInGateway(device, testId, deviceConnectedWith5Ghz, WiFiFrequencyBand.WIFI_BAND_5_GHZ,
					stepNumber);

			/**
			 * SETP : 32- 35
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					BroadBandTestConstants.BAND_5GHZ);

			/**
			 * SETP 36: SET AND VERIFY NEW SSID NAME IS BROADCASTED FOR 5 GHZ IN CONNECTED
			 * CLIENT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY NEW SSID NAME IS BROADCASTED FOR 5 GHZ IN CONNECTED CLIENT ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY NEW SSID NAME IS BROADCASTED FOR 5 GHZ IN CONNECTED CLIENT USING WEBPA ");
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED : NEW SSID NAME IS BROADCASTED FOR 5 GHZ IN CONNECTED CLIENT ");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE SET NEW SSID NAME FOR 5 GHZ IN CONNECTED CLIENT";
			defaultSsidFor_5Ghz = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.PRIVATEWIFI_NAME_FOR_5GHZ_BAND)) {
				errorMessage = "UNABLE TO VERIFY THE NEW SSID NAME IS BROADCASTED FOR 5 GHZ IN CONNECTED CLIENT";
				status = BroadBandWiFiUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDeviceWithPollDuration(
						device, tapEnv, deviceConnectedWith5Ghz, WiFiFrequencyBand.WIFI_BAND_5_GHZ, true,
						BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : NEW SSID NAME IS BROADCASTED FOR 5 GHZ IN CONNECTED CLIENT.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 37 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 5 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 5GHz SSID";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			status = BroadBandConnectedClientUtils.connectGivenWiFiCapableClientAndConnectWithGivenWiFiBand(device,
					tapEnv, deviceConnectedWith5Ghz, BroadBandTestConstants.BAND_5GHZ);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 5GHz SSID SUCCESSFULLY");
			} else {
				deviceConnectedWith5Ghz = null;
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 38-39 : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
			 * /rdklogs/logs/LM.txt.0 LOG
			 */
			verifyConnectedclientDetails(device, testId, deviceConnectedWith5Ghz, BroadBandTestConstants.BAND_5GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_ASSOCIATED_DEVICES, deviceDateTime);

			/**
			 * STEP 40 : VERIFY 5GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			verifyClientMacInGateway(device, testId, deviceConnectedWith5Ghz, WiFiFrequencyBand.WIFI_BAND_5_GHZ,
					stepNumber);

			/**
			 * SETP : 41- 44
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					BroadBandTestConstants.BAND_5GHZ);

			/**
			 * Step 45 : VERIFY 2.4 GHZ RADIO IS REENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY 2.4 GHZ RADIO IS REENABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : VERIFY 2.4 GHZ RADIO IS REENABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 2.4 GHZ RADIO MUST BE REENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO REENABLE THE 2.4 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			LOGGER.info("STEP " + stepNumber + " : ACTUAL : "
					+ (status ? " SUCCESSFYLLY REENABLED THE 2.4 GHZ RADIO" : errorMessage));
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 46: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5
			 * GHZ
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 5GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 5GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith5Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ));
			if (status) {
				deviceConnectedWith5Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 5GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VALIDATING THE ENABLE ONE RADIO & DISABLE ANOTHER RADIO AND CHECK THE INTERNET CONNECTIVITY "
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS:");
			executePostConditionToVerifyDefaultRadioStatus(device);
			executePostConditionToSetDefaultOperStandard(device);
			executePostConditionToDisconnectClients(device, deviceConnectedWith2Ghz, deviceConnectedWith5Ghz);
			executePostConditionToReActivateDevice(device, isReactivated);
			if (defaultSsidFor_2_4Ghz != null) {
				/**
				 * POSTCONDITION : SET AND VERIFY THE DEFAULT SSID NAME FOR 2.4 GHZ USING WEBPA
				 */
				postConStepNumber++;
				status = false;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POSTCONDITION " + postConStepNumber
						+ " : DESCRIPTION : SET AND VERIFY THE DEFAULT SSID NAME FOR 2.4 .");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTION : SET AND VERIFY THE DEFAULT SSID NAME FOR 2.4 USING WEBPA.");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : EXPTECTED : SUCCESSFULLY SET THE DEFAULT SSID NAME FOR 2.4 GHZ.");
				LOGGER.info("#######################################################################################");
				status = false;
				errorMessage = "UNABLE TO SET THE DEFAULT SSID NAME FOR 2.4 GHZ";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
						BroadBandTestConstants.CONSTANT_0, defaultSsidFor_2_4Ghz);
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : SUCCESSFULLY SET THE DEFAULT SSID NAME FOR 2.4 GHZ.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
			}
			if (defaultSsidFor_5Ghz != null) {
				/**
				 * POSTCONDITION : SET AND VERIFY THE DEFAULT SSID NAME FOR 5 GHZ USING WEBPA
				 */
				postConStepNumber++;
				status = false;
				LOGGER.info("#######################################################################################");
				LOGGER.info("POSTCONDITION " + postConStepNumber
						+ " : DESCRIPTION : SET AND VERIFY THE DEFAULT SSID NAME FOR 5 GHz");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTION : SET AND VERIFY THE DEFAULT SSID NAME FOR 5 USING WEBPA.");
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : EXPTECTED : SUCCESSFULLY SET THE DEFAULT SSID NAME FOR 5 GHZ.");
				LOGGER.info("#######################################################################################");
				status = false;
				errorMessage = "UNABLE TO SET THE DEFAULT SSID NAME FOR 5GHZ";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
						BroadBandTestConstants.CONSTANT_0, defaultSsidFor_5Ghz);
				if (status) {
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL : SUCCESSFULLY SET THE DEFAULT SSID NAME FOR 5 GHZ.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
				}
			}
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CC-RADIO-5001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Pre-Condition method to perform Factory reset and reactivate the device .
	 * 
	 * @param device {@link Dut}
	 * @refactor Athira
	 */
	private void executePreConditionToFactoryResetAndReacitivateDevice(Dut device) throws TestException {
		String errorMessage = null;
		boolean status = false;
		/**
		 * PRECONDITION :Factory Reset
		 */
		preConStepNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber + " : DESCRIPTION : PERFORM FACTORY RESET ON THE DEVICE.");
		LOGGER.info("PRE-CONDITION " + preConStepNumber + " : ACTION : PERFORM FACTORY RESET USING WEBPA.");
		LOGGER.info("PRE-CONDITION " + preConStepNumber + " : EXPTECTED : DEVICE MUST UNDERGO FACTORY RESET.");
		LOGGER.info("#######################################################################################");
		errorMessage = "UNABLE TO PERFORM WIFI FACTORY RESET OPERATION ON THE DEVICE. HENCE BLOCKING THE EXECUTION.";
		status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
		if (status) {
			LOGGER.info("PRE-CONDITION " + preConStepNumber + " : ACTUAL : FACTORY RESET SUCCESSFULLY PERFORMED.");
		} else {
			LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
		/**
		 * PRECONDITION 2 :WebPA Process Status
		 */
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
			isReactivated = status;
		} catch (TestException e) {
			errorMessage = e.getMessage();
		}
		if (status) {
			LOGGER.info("PRE-CONDITION " + stepNumber + " : ACTUAL: THE ROUTER DEVICE REACTIVATED SUCCESSFULLY.");
		} else {
			LOGGER.error("PRE-CONDITION " + stepNumber + " : ACTUAL: " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
	}

	/**
	 * Method to VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE
	 * 
	 * @param device          {@link Dut}
	 * @param testId          Test case ID
	 * @param deviceConnected Device Connected
	 * @param wifiBand
	 * @param stepNumber      Step Number
	 * @refactor Athira
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
	 * Post-Condition method to verify the default radio status
	 * 
	 * @param device {@link Dut}
	 * @refactor Athira
	 */
	private void executePostConditionToVerifyDefaultRadioStatus(Dut device) throws TestException {
		String errorMessage = null;
		boolean status = false;
		try {/**
				 * POSTCONDITION : VERIFY THE 2.4 GHZ RAIDO STATUS AS ENABLED
				 */
			postConStepNumber++;
			LOGGER.info("#######################################################################################");
			LOGGER.info("POSTCONDITION " + postConStepNumber
					+ " : DESCRIPTION : VERIFY THE 2.4 GHZ RADIO STATUS TO BE ENABLED .");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : ACTION : VERIFY THE 2.4 GHZ RADIO STATUS TO BE ENABLED USING WEBPA.");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : EXPTECTED : SUCCESSFULLY VERIFIED THE 2.4 GHZ RADIO STATUS AS 'TRUE' ");
			LOGGER.info("#######################################################################################");
			status = false;
			errorMessage = "UNABLE TO VERIFY THE 2.4 GHZ RADIO STATUS AS 'TRUE'.";
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.TRUE);
			if (!status) {
				errorMessage = "UNABLE TO SET THE 2.4 GHZ RADIO STATUS AS 'TRUE'.";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
						BroadBandTestConstants.TRUE);
			}
			if (status) {
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE 2.4 GHZ RADIO STATUS AS 'TRUE'");
			} else {
				LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
			}

			/**
			 * POSTCONDITION :VERIFY THE THE 5 GHZ RAIDO STATUS AS ENABLED USING WEBPA
			 */
			postConStepNumber++;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("POSTCONDITION " + postConStepNumber
					+ " : DESCRIPTION : VERIFY THE 5 GHZ RADIO STATUS TO BE ENABLED .");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : ACTION : VERIFY THE 5 GHZ RADIO STATUS TO BE ENABLED USING WEBPA.");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : EXPTECTED : SUCCESSFULLY VERIFIED THE 5 GHZ RADIO STATUS AS 'TRUE' ");
			LOGGER.info("#######################################################################################");
			status = false;
			errorMessage = "UNABLE TO VERIFY THE 5 GHZ RADIO STATUS AS 'TRUE'.";
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.TRUE);
			if (!status) {
				errorMessage = "UNABLE TO SET THE 5 GHZ RADIO STATUS AS 'TRUE'.";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
						BroadBandTestConstants.TRUE);
			}
			if (status) {
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE 5 GHZ RADIO STATUS AS 'TRUE'");
			} else {
				LOGGER.error("POST-CONDITION " + postConStepNumber + " : ACTUAL :" + errorMessage);
			}
		} catch (Exception exception) {
			LOGGER.error(
					"Execution error occurred while executing to verify default radio status post conditions due to exception --> "
							+ exception.getMessage());
		}
	}

	/**
	 * Post-Condition method to reactivate the device
	 * 
	 * @param device        {@link Dut}
	 * @param isReactivated It is 'True' Device already activated.'False' Device Not
	 *                      reactivated
	 * @refactor Athira
	 */
	private void executePostConditionToReActivateDevice(Dut device, boolean isReactivated) throws TestException {
		String errorMessage = null;
		boolean status = false;
		try {
			/**
			 * POSTCONDITION : BEGIN BROAD BAND DEVICE REACTIVATION
			 */
			postConStepNumber++;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION " + postConStepNumber + " : DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION.");
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : ACTION : BROAD BAND DEVICE REACTIVATION USING WEBPA OR SNMP. ");
			LOGGER.info("POST-CONDITION " + postConStepNumber + " : EXPECTED : DEVICE SHOULD GET REACTIVATED");
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
					LOGGER.info("POST-CONDITION " + postConStepNumber
							+ " : ACTUAL: THE ROUTER DEVICE REACTIVATED SUCCESSFULLY.");
				} else {
					LOGGER.error("POST-CONDITION " + postConStepNumber + ": ACTUAL: " + errorMessage);
				}
			} else {
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ ": ACTUAL: SKIPPING REACTIVATION STEP AS THE DEVICE WAS ALREADY REACTIVATED SUCCESSFULLY IN PRE-CONDITION.");
			}
		} catch (Exception exception) {
			LOGGER.error(
					"Execution error occurred while executing to verify default radio status post conditions due to exception --> "
							+ exception.getMessage());
		}
	}

	/**
	 * Test step method used to validate the process crash when trying to set the
	 * invalid operating standard for DSL Devices Model
	 * 
	 * @param device            {@link Dut}
	 * @param testId            Test case ID
	 * @param stepNumber        Step Number
	 * @param wifiFrequencyBand WiFi frequency band
	 * @param webParameter      Web parameter to set
	 * @param operatingStd      Operating standard to set
	 */
	public void executeTestStepToValidateProcessCrash(Dut device, String testId, String wifiFrequencyBand,
			String webParameter, String operatingStd) {
		/**
		 * Step : Verify the process crash while setting invalid operating standard for
		 * DSL devices
		 */
		String command = null;
		stepNumber++;
		stepNum = "S" + stepNumber;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY THE INVALID OPERATING STANDARD FOR "
				+ wifiFrequencyBand + " VALUE AS " + operatingStd + " AND VERIFY THE PROCESS CRASH");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL STANDARD USING WEBPA PARAM " + webParameter);
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD SHOULD NOT BE SET TO " + operatingStd);
		errorMessage = "ABLE TO SET AND VERIFY THE INVALID OPERATING STANDARD FOR " + wifiFrequencyBand + "GHz";
		if (DeviceModeHandler.isDSLDevice(device)
		/*
		 * && !DeviceModeHandler.isGivenModel(device,
		 * BroadBandTestConstants.MODEL_NAME_ADA)
		 */) {
			status = !BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, webParameter,
					WebPaDataTypes.STRING.getValue(), operatingStd);
			if (status) {
				command = BroadBandCommonUtils.concatStringUsingStringBuffer(LinuxCommandConstants.COMMAND_CAT,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_SYSTEMD_PROCESS);
				String response = tapEnv.executeCommandUsingSsh(device, command);
				status = !(CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
						BroadBandTraceConstants.LOG_STOPPING_RESTARTING));
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED INVLAID OPERATING STANDARD SET OPERATION AND PROCESS CRASH NOT HAPPEN.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);
		} else {
			errorMessage = "STEP " + stepNumber + " : ACTUAL : TEST STEP APPLICABLE ONLY FOR DSL DEVICE MODEL";
			LOGGER.info(errorMessage);
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionForAllStatus(device, testId, stepNum, ExecutionStatus.NOT_APPLICABLE, errorMessage,
					false);
		}
	}

	/**
	 *
	 * Test Case : Verify the WIFI Connectivity on Dual Band Radio for Client for
	 * 2.4 and 5 GHz with security mode Open
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * 
	 * <li>PRE CONDITION 1 : Verify the private wifi 2.4 & 5 GHz SSID's is
	 * broadcasting in connected client.</li>
	 * <li>PRE CONDITION 2 : Get the default value for operating standard 2.4
	 * GHz.</li>
	 * <li>PRE CONDITION 3 : Get the default value for operating standard 5
	 * GHz.</li>
	 * <li>PRE CONDITION 4 : Get the default channel value for 2.4 GHz</li>
	 * <li>PRE CONDITION 5 : Get the default channel value for 5 GHz.</li>
	 * <li>PRE CONDITION 6 : Set the channel selection mode to manual and set the
	 * respective channel value for 2.4 GHz and 5 GHz via webpa.</li>
	 * <li>PRE CONDITION 7 : Verify the security mode as 'None' for 2.4 GHz
	 * SSID</li>
	 * <li>PRE CONDITION 8 : Verify the security mode as 'None' for 5 GHz SSID</li>
	 * <li>PRE CONDITION 9 : Perform apply settings for 2.4 and 5 GHz radio's</li>
	 * <li>Step 1 : Set and verify the value of operation standard as a for 5
	 * GHz.</li>
	 * <li>Step 2 : Set and verify the value of operation standard as g for 2.4
	 * GHz.</li>
	 * <li>Step 3 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 4 : Connect the another client in the setup to 5 GHz SSID and verify
	 * connection status.</li>
	 * <li>Step 5 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 6 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 7 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 8 : Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 9 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 10 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 11 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 12 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 13 : Set and verify the value of operation standard as b for 2.4
	 * GHz</li>
	 * <li>Step 14 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 15 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 16 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 17 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 18 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 19 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 20 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 21 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 22 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 23 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 24 : Set and verify the value of operation standard as ac for
	 * 5GHz</li>
	 * <li>Step 25 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 26 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 27 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 28 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 29 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 30 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 31 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 32 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 33 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 34 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 35 : Set and verify the value of operation standard as n for 2.4
	 * GHz</li>
	 * <li>Step 36 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 37 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 38 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 39 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 40 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 41 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 42 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 43 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 44 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 45 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 46 : Set and verify the value of operation standard as g for
	 * 2.4GHz</li>
	 * <li>Step 47 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 48 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 49 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 50 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 51 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 52 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 53 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 54 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 55 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 56 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 57 : Verify disconnecting the client connected with 2.4 GHz SSID in
	 * the setup.</li>
	 * <li>Step 58 : Verify disconnecting the client connected with 5 GHz SSID in
	 * the setup.</li>
	 * <li>POST CONDITION 1 : Revert the default channel selection mode,operating
	 * standard,default channel value,security mode for 2.4 GHz and 5 GHz via
	 * webpa</li>
	 * <li>POST CONDITION 2 : Set and verify the security mode as 'WPA2-Personal'
	 * for 2.4 GHz private wiFi</li>
	 * <li>POST CONDITION 3 : Set and verify the security mode as 'WPA2-Personal'
	 * for 5 GHz private wiFi</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @refactor Sruthi Santhosh
	 *
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-SECTY-OPEN-5001")
	public void testToVerifyWifiOnDualBandSecurityOpen(Dut device) {
		String testCaseId = "TC-RDKB-WIFI-SECTY-OPEN-501";
		String errorMessage = null;
		boolean status = false;
		stepNumber = 1;
		postConStepNumber = 0;
		preConStepNumber = 0;
		String step = "S" + stepNumber;
		List<Dut> ssidVisibleDevices = new ArrayList<Dut>();
		Dut ssidVisibleDeviceOne = null;
		Dut ssidVisibleDeviceTwo = null;
		boolean isSecurityModeChanged = false;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-SECTY-OPEN-5001");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the WIFI Connectivity on Dual Band Radio for Client for 2.4 and 5 GHz with security mode open.");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"PRE CONDITION 1 : Verify the private wifi 2.4 & 5 GHz SSID's is broadcasting in connected client.");
			LOGGER.info("PRE CONDITION 2 : Get the default value for operating standard 2.4 GHz.");
			LOGGER.info("PRE CONDITION 3 : Get the default value for operating standard 5 GHz.");
			LOGGER.info("PRE CONDITION 4 : Get the default channel value for 2.4 GHz ");
			LOGGER.info("PRE CONDITION 5 : Get the default channel value for 5 GHz.");
			LOGGER.info(
					"PRE CONDITION 6 : Set the channel selection mode to manual and set the respective channel value for 2 GHz and 5 GHz via webpa.");
			LOGGER.info("PRE CONDITION 7 : Verify the security mode as 'None' for 2.4 GHz SSID");
			LOGGER.info("PRE CONDITION 8 : Verify the security mode as 'None' for 5 GHz SSID");
			LOGGER.info("PRE CONDITION 9 : Perform apply settings for 2.4 and 5 GHz radio's");
			LOGGER.info("Step 1 : Set and verify the value of operation standard as a for 5 GHz.");
			LOGGER.info("Step 2 : Set and verify the value of operation standard as g for 2.4 GHz.");
			LOGGER.info(
					"Step 3 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info("Step 4 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 5 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 6 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 7 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 8 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 9 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 10 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 11 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 12 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 13 : Set and verify the value of operation standard as b for 2.4 GHz");
			LOGGER.info(
					"Step 14 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 15 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 16 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 17 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 18 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 19 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 20 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 21 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 22 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 23 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 24 : Set and verify the value of operation standard as ac for 5 GHz");
			LOGGER.info(
					"Step 25 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 26 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 27 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 28 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 29 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 30 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 31 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 32 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 33 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 34 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 35 : Set and verify the value of operation standard as n for 2.4 GHz ");
			LOGGER.info(
					"Step 36 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 37 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 38 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 39 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 40 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 41 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 42 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 43 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 44 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 45 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 46 : Set and verify the value of operation standard as g for 2.4 GHz ");
			LOGGER.info(
					"Step 47 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 48 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 49 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 50 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 51 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 52 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 53 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 54 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 55 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 56 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 57 : Verify disconnecting the client connected with 2.4 GHz SSID in the setup.");
			LOGGER.info("Step 58 : Verify disconnecting the client connected with 5 GHz SSID in the setup.");
			LOGGER.info(
					" POST CONDITION 1 : Revert the default channel selection mode,operating standard,default channel value,security mode for 2.4 GHz and 5 GHz");
			LOGGER.info(
					" POST CONDITION 2 : Set and verify the security mode as 'WPA2-Personal' for 2.4 GHz private WiFi");
			LOGGER.info(
					" POST CONDITION 3 : Set and verify the security mode as 'WPA2-Personal' for 5 GHz private WiF");
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			/**
			 * PRE-CONDITION 1 : VERIFY THE PRIVATE WIFI 2.4 GHz AND 5 GHz SSID'S ARE
			 * BROADCASTING IN CONNECTED CLIENT
			 */
			preConStepNumber++;
			ssidVisibleDevices = BroadBandPreConditionUtils.executePreConditionToGetBothPrivateWiFiSsidsVisibleDevices(
					device, tapEnv, preConStepNumber, BroadBandTestConstants.CONSTANT_2);
			ssidVisibleDeviceOne = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_0);
			ssidVisibleDeviceTwo = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_1);

			/**
			 * PRE-CONDITION 2-3 : GET THE DEFAULT VALUES
			 */
			executePreConditionToGetDefaultOperStandard(device);

			/**
			 * PRE-CONDITION 4-5 : GET THE DEFAULT VALUES
			 */
			executePreConditionToGetDefaultChannel(device);

			/**
			 * PRE CONDITION 6 : SET THE CHANNEL SELECTION MODE TO MANUAL AND RESPECTIVE
			 * CHANNEL VALUE FOR 2.4 & 5 GHz AND GHz VIA WEBPA.
			 */
			executePreConditionToSetChannel(device);

			/**
			 * PRE-CONDITION 7-8 : VERIFY THE SECURITY MODE "None" FOR 2.4 and 5 GHz SSID
			 */
			preConStepNumber++;
			BroadBandPreConditionUtils.executePreConditionToSetSecurityMode(device, tapEnv, preConStepNumber,
					BroadBandTestConstants.SECURITY_MODE_NONE);
			isSecurityModeChanged = true;

			/**
			 * PRE-CONDITION 9 : PERFORM APPLY SETTINGS FOR 2.4 and 5 GHz RADIO's
			 */
			preConStepNumber = 9;
			BroadBandPreConditionUtils.executePreConditionToRadioApplySetting(device, tapEnv, preConStepNumber);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1 : SET AND VERIFY THE VALUE OF 5 GHz WITH OPERATING STANDARD AS a
			 */
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_A);

			/**
			 * STEP 2 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS g
			 */
			stepNumber++;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_G);

			/**
			 * Step 3 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4 GHz SSID
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFiOpenSecurityMode(device, testCaseId,
					ssidVisibleDeviceOne, BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 4 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 5 GHz SSID
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFiOpenSecurityMode(device, testCaseId,
					ssidVisibleDeviceTwo, BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 5 - 8 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 9 - 12 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 9;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 13 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS b
			 */
			stepNumber = 13;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_B);

			/**
			 * Step 14 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFiOpenSecurityMode(device, testCaseId,
					ssidVisibleDeviceOne, BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 15 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFiOpenSecurityMode(device, testCaseId,
					ssidVisibleDeviceTwo, BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 16 - 19 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * SETP 20 - 23 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 23;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 24 : SET AND VERIFY THE VALUE OF 5 GHz WITH OPERATING STANDARD AS ac
			 */
			stepNumber = 24;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_AC);

			/**
			 * Step 25 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFiOpenSecurityMode(device, testCaseId,
					ssidVisibleDeviceOne, BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 26 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFiOpenSecurityMode(device, testCaseId,
					ssidVisibleDeviceTwo, BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 27 - 30 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 31 - 34 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 31;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 35 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS n
			 */
			stepNumber = 35;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_N);

			/**
			 * Step 36 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFiOpenSecurityMode(device, testCaseId,
					ssidVisibleDeviceOne, BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 37 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFiOpenSecurityMode(device, testCaseId,
					ssidVisibleDeviceTwo, BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 38 - 41 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 42 - 45 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 42;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 46 : SET AND VERIFY THE VALUE OF 2.4 GHz WITH OPERATING STANDARD AS g
			 */
			stepNumber = 46;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_G);

			/**
			 * Step 47 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFiOpenSecurityMode(device, testCaseId,
					ssidVisibleDeviceOne, BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 48 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFiOpenSecurityMode(device, testCaseId,
					ssidVisibleDeviceTwo, BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 49 - 52 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 53-56 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 53;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * Step 57: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHz
			 */
			stepNumber = 57;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 58: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHz
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VALIDATING WIFI CONNECTIVITY ON DUAL BAND RADIO FOR CLIENT FOR 2.4 AND 5 GHz WITH SECURITY MODE OPEN : "
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS:");

			/**
			 * POST CONDITION 1 : REVERT THE DEFAULT CHANNEL SELECTION MODE, OPERATING
			 * STANDARD, DEFAULT CHANNEL VALUE, SECURITY MODE FOR 2 GHz AND 5 GHz
			 */
			executePostConditionToSetChannelAndOprStd(device);

			/**
			 * POST CONDITION 2 : SET AND VERIFY THE SECURITY MODE FOR 2.4 AND 5 GHz PRIVATE
			 * WIFI
			 */
			if (isSecurityModeChanged) {
				postConStepNumber++;
				BroadBandPostConditionUtils.executePostConditionToSetSecurityMode(device, tapEnv, postConStepNumber,
						BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-SECTY-OPEN-5001");
	}

	/**
	 * PRE-CONDITION METHOD TO SET THE CHANNEL SELECTION MODE TO MANUAL AND
	 * RESPECTIVE CHANNEL VALUE FOR 2.4 GHz AND 5 GHz.
	 * 
	 * @param device {@link Dut}
	 */
	private void executePreConditionToSetChannel(Dut device) {
		preConStepNumber++;
		status = false;
		LOGGER.info("#######################################################################################");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : DESCRIPTION : SET THE CHANNEL SELECTION MODE TO MANUAL AND RESPECTIVE CHANNEL VALUE FOR 2.4 GHz AND 5 GHz");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : SET THE CHANNEL SELECTION MODE TO MANUAL AND RESPECTIVE CHANNEL VALUE FOR 2.4 GHz AND 5 GHz USING WEBPA");
		LOGGER.info("PRE-CONDITION " + preConStepNumber
				+ " : EXPECTED : MUST SET THE CHANNEL SELECTION MODE TO MANUAL AND RESPECTIVE CHANNEL VALUE FOR 2.4 GHz AND 5 GHz");
		LOGGER.info("#######################################################################################");
		errorMessage = "FAILED TO SET CHANNEL SELECTION MODE TO MANUAL AND RESPECTIVE CHANNEL VALUE FOR 2.4 GHz AND 5 GHz";
		List<WebPaParameter> webPaParameters = new ArrayList<WebPaParameter>();
		WebPaParameter channelManual2ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
				BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_CHANNEL_SELECTION_MODE, BroadBandTestConstants.FALSE,
				WebPaDataTypes.BOOLEAN.getValue());
		webPaParameters.add(channelManual2ghz);
		WebPaParameter channelManual5ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
				BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_CHANNEL_SELECTION_MODE, BroadBandTestConstants.FALSE,
				WebPaDataTypes.BOOLEAN.getValue());
		webPaParameters.add(channelManual5ghz);
		WebPaParameter Channel2ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ,
				String.valueOf(BroadBandTestConstants.CHANNEL_NO_11), WebPaDataTypes.INTEGER.getValue());
		webPaParameters.add(Channel2ghz);
		WebPaParameter Channel5ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
				String.valueOf(DeviceModeHandler.isRPIDevice(device) ? BroadBandTestConstants.CHANNEL_NO_44
						: BroadBandTestConstants.CHANNEL_NO_161),
				WebPaDataTypes.INTEGER.getValue());
		webPaParameters.add(Channel5ghz);
		status = BroadBandWebPaUtils.setVerifyMultipleWebPAInPolledDuration(device, tapEnv, webPaParameters,
				BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		if (status) {
			isAutoChnl2Ghz = true;
			isAutoChnl5Ghz = true;
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTUAL : SUCCESSFULLY SET CHANNEL SELECTION MODE TO MANUAL AND RESPECTIVE CHANNEL VALUE FOR 2.4 GHz AND 5 GHz");
		} else {
			LOGGER.info("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
			throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : " + preConStepNumber
					+ " FAILED : " + errorMessage);
		}
	}

	/**
	 * Test method used to set and validate the operating standard for 2.4 and 5 GHz
	 * 
	 * @param device                Instance of{@link Dut}
	 * @param testCaseId            Test case ID
	 * @param wifiBand              Frequency band 2.4/5 GHz
	 * @param stepNumber            Test step number
	 * @param wifiOperatingStandard Operating standard for 2.4/5 GHz
	 * 
	 */
	public static void executeTestStepToChangeOperatingStandard(Dut device, String testCaseId, String wifiBand,
			int stepNumber, String wifiOperatingStandard) {
		String stepNum = "S" + stepNumber;
		boolean status = false;
		String errorMessage = null;

		if (wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ) && DeviceModeHandler.isRPIDevice(device)
				&& wifiOperatingStandard.equalsIgnoreCase(BroadBandTestConstants.OPERATING_STANDARDS_B)) {
			LOGGER.info("Current RPI setup only supports g,n as 2.4GHz operating standands");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
					errorMessage, false);
		} else if (wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_5GHZ) && DeviceModeHandler.isRPIDevice(device)
				&& (wifiOperatingStandard.equalsIgnoreCase(BroadBandTestConstants.OPERATING_STANDARDS_A)
						|| wifiOperatingStandard.equalsIgnoreCase(BroadBandTestConstants.OPERATING_STANDARDS_N))) {
			LOGGER.info("Current RPI setup only supports ac as 5GHz operating standands");
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
					errorMessage, false);
		} else {
			String operStdWepParam = wifiBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
					? BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD
					: BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD;
			/**
			 * Step : SET AND VERIFY THE VALUE OF OPERATING STANDARD FOR 2.4/5 GHZ
			 * 
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY THE VALUE OF " + wifiBand
					+ " WITH OPERATING STANDARD AS " + wifiOperatingStandard);
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATING STANDARD FOR USING WEBPA PARAM "
					+ operStdWepParam);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET TO "
					+ wifiOperatingStandard);
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET OPERATING STANDARD AS " + wifiOperatingStandard;
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv, operStdWepParam,
					BroadBandTestConstants.CONSTANT_0, wifiOperatingStandard);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS "
						+ wifiOperatingStandard);
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
		}
	}

	/**
	 * PRE-CONDITION METHOD TO REVERT THE DEFAULT CHANNEL SELECTION MODE,OPERATING
	 * STANDARD,DEFAULT CHANNEL VALUE FOR 2.4 GHz AND 5 GHz VIA WEBPA.
	 * 
	 * @param device {@link Dut}
	 */
	private void executePostConditionToSetChannelAndOprStd(Dut device) {
		status = false;
		postConStepNumber++;
		LOGGER.info("#######################################################################################");
		LOGGER.info("POST-CONDITION " + postConStepNumber
				+ " : DESCRIPTION : REVERT THE DEFAULT CHANNEL SELECTION MODE,OPERATING STANDARD,DEFAULT CHANNEL VALUE FOR 2.4 GHz AND 5 GHz.");
		LOGGER.info("POST-CONDITION " + postConStepNumber
				+ " : SET THE DEFAULT CHANNEL SELECTION MODE,OPERATING STANDARD,DEFAULT CHANNEL VALUE FOR 2.4 GHz AND 5 GHz USING WEBPA");
		LOGGER.info("POST-CONDITION " + postConStepNumber
				+ " : EXPECTED : MUST SET THE DEFAULT CHANNEL SELECTION MODE,OPERATING STANDARD,DEFAULT CHANNEL VALUE FOR 2.4 GHz AND 5 GHz");
		LOGGER.info("#######################################################################################");
		errorMessage = "FAILED TO SET DEFAULT CHANNEL SELECTION MODE,OPERATING STANDARD,DEFAULT CHANNEL VALUE FOR 2.4 GHz AND 5 GHz";
		List<WebPaParameter> webPaParameters = new ArrayList<WebPaParameter>();
		if (isAutoChnl2Ghz) {
			WebPaParameter channelAuto2ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
					BroadBandWebPaConstants.WEBPA_PARAM_2_4_GHZ_CHANNEL_SELECTION_MODE, BroadBandTestConstants.TRUE,
					WebPaDataTypes.BOOLEAN.getValue());
			webPaParameters.add(channelAuto2ghz);
		}
		if (isAutoChnl5Ghz) {
			WebPaParameter channelAuto5ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
					BroadBandWebPaConstants.WEBPA_PARAM_5_GHZ_CHANNEL_SELECTION_MODE, BroadBandTestConstants.TRUE,
					WebPaDataTypes.BOOLEAN.getValue());
			webPaParameters.add(channelAuto5ghz);
		}
		if (defaultOperStandard_2_4Ghz != null) {
			WebPaParameter defaultOperStd2ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
					defaultOperStandard_2_4Ghz, WebPaDataTypes.STRING.getValue());
			webPaParameters.add(defaultOperStd2ghz);
		}
		if (defaultOperStandard_5Ghz != null) {
			WebPaParameter defaultOperStd5ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
					defaultOperStandard_5Ghz, WebPaDataTypes.STRING.getValue());
			webPaParameters.add(defaultOperStd5ghz);
		}
		if (defaultChannel_2_4Ghz != 0) {
			WebPaParameter defaultChannel2ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_2GHZ,
					String.valueOf(defaultChannel_2_4Ghz), WebPaDataTypes.INTEGER.getValue());
			webPaParameters.add(defaultChannel2ghz);
		}
		if (defaultChannel_5Ghz != 0) {
			WebPaParameter defaultChannel5ghz = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL_IN_5GHZ,
					String.valueOf(defaultChannel_5Ghz), WebPaDataTypes.INTEGER.getValue());
			webPaParameters.add(defaultChannel5ghz);
		}
		if (isAutoChnl2Ghz || isAutoChnl5Ghz || defaultOperStandard_2_4Ghz != null || defaultOperStandard_5Ghz != null
				|| defaultChannel_2_4Ghz != 0 || defaultChannel_5Ghz != 0) {
			status = BroadBandWebPaUtils.setVerifyMultipleWebPAInPolledDuration(device, tapEnv, webPaParameters,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("POST-CONDITION " + postConStepNumber
						+ " : ACTUAL : SUCCESSFULLY SET DEFAULT CHANNEL SELECTION MODE,OPERATING STANDARD,DEFAULT CHANNEL VALUE FOR 2 GHz AND 5 GHz");
			} else {
				LOGGER.info("POST-CONDITION " + postConStepNumber + " : ACTUAL : " + errorMessage);
			}
		} else {
			LOGGER.info("POST-CONDITION " + postConStepNumber
					+ " : ACTUAL : NO CHNAGES IN DEFAULT CHANNEL SELECTION MODE,OPERATING STANDARD,CHANNEL VALUE FOR 2 GHz AND 5 GHz");
		}
	}

	/**
	 *
	 * Test Case : Verify the default channel bandwidth and enable & disable the
	 * radio for 2.4GHz & 5GHz with connected clients
	 * 
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : PERFORM FACTORY RESET ON THE DEVICE</li>
	 * <li>PRE-CONDITION 2 : REACTIVATE THE ROUTER DEVICE</li>
	 * <li>Step 1 : Verify for 2.4Ghz radio default channel bandwidth</li>
	 * <li>Step 2 : Verify for 5Ghz radio default channel bandwidth</li>
	 * <li>Step 3 : Verify for 2.4Ghz radio is enabled by default</li>
	 * <li>Step 4 : Verify for 5Ghz radio is enabled by default</li>
	 * <li>Step 5 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 6 : Verify client associated with 2.4 GHz using webpa</li>
	 * <li>Step 7 : Verify the client mac address and connection type connected with
	 * 2.4 GHz in LM.txt.0 log file</li>
	 * <li>Step 8 : Verify the connected clients details are IP and MAC in
	 * Gateway</li>
	 * <li>Step 9 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 10: Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 11: Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 12: Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 13: Verify 2.4 GHz radio is disabled</li>
	 * <li>Step 14: Verify the private wifi 2.4 GHz SSID is broadcasting in
	 * connected client</li>
	 * <li>Step 15: Verify 2.4 GHz radio is reenabled</li>
	 * <li>Step 16: Verify the private wifi 2.4 GHz SSID is broadcasting in
	 * connected client</li>
	 * <li>Step 17: Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status after 2.4 GHz radio is reenabled</li>
	 * <li>Step 18: Verify client associated with 2.4 GHz using webpa</li>
	 * <li>Step 19: Verify the client mac address and connection type connected with
	 * 2.4 GHz in LM.txt.0 log file</li>
	 * <li>Step 20: Verify the connected clients details are IP and MAC in
	 * Gateway</li>
	 * <li>Step 21: Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 22: Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 23: Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 24: Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 25: Verify disconnecting the 2.4 GHz private wifi SSID</li>
	 * <li>Step 26: Connect the connected client in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 27: Verify client associated with 5 GHz using webpa</li>
	 * <li>Step 28: Verify the client mac address and connection type connected with
	 * 5 GHz in LM.txt.0 log file</li>
	 * <li>Step 29: Verify the connected clients details are IP and MAC in
	 * Gateway</li>
	 * <li>Step 30: Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 31: Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 32: Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 33: Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 34: Verify 5 GHz radio is disabled</li>
	 * <li>Step 35: Verify the private wifi 5 GHz SSID is broadcasting in connected
	 * client</li>
	 * <li>Step 36: Verify 5 GHz radio is reenabled</li>
	 * <li>Step 37: Verify the private wifi 5 GHz SSID is broadcasting in connected
	 * client</li>
	 * <li>Step 38: Connect the connected client in the setup to 5 GHz SSID and
	 * verify connection status after 5 GHz radio is reenabled</li>
	 * <li>Step 39: Verify client associated with 5 GHz using webpa</li>
	 * <li>Step 40: Verify the client mac address and connection type connected with
	 * 5 GHz in LM.txt.0 log file</li>
	 * <li>Step 41: Verify the connected clients details are IP and MAC in
	 * Gateway</li>
	 * <li>Step 42: Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 43: Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 44: Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 45: Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 46: Verify disconnecting the 5 GHz private wifi SSID</li>
	 * 
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * @author Muthukumar
	 * @refactor Athira
	 * 
	 **/
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-WIFI-CHANNEL-5001")
	public void testToVerifyInternetConnectionByEnableDisableBothRadios(Dut device) {
		String testId = "TC-RDKB-WIFI-CHANNEL-501";
		String step = null;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWith2Ghz = null;
		Dut deviceConnectedWith5Ghz = null;
		String response = null;
		stepNumber = 1;
		preConStepNumber = 0;
		postConStepNumber = 0;
		step = "S" + stepNumber;
		String deviceDateTime = null;
		isReactivated = false;
		try {

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CHANNEL-5001");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the default channel bandwidth and enable & disable the radio for 2.4GHz & 5GHz with connected clients.");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(" 1 : Verify for 2.4Ghz radio default channel bandwidth");
			LOGGER.info(" 2 : Verify for 5Ghz radio default channel bandwidth");
			LOGGER.info(" 3 : Verify for 2.4Ghz radio is enabled by default");
			LOGGER.info(" 4 : Verify for 5Ghz radio is enabled by default");
			LOGGER.info(
					" 5 : Connect  the connected client  in the setup to 2.4 GHz SSID and verify connection status");
			LOGGER.info(" 6 : Verify client associated associated with  2.4 GHz using webpa");
			LOGGER.info(
					" 7 : Verify the client mac address and connection type connected with 2.4 GHz in LM.txt.0 log file");
			LOGGER.info(" 8 : Verify the connected clients details are IP and MAC in Gateway");
			LOGGER.info(" 9 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(" 10: Verify  the correct IPv6  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(
					" 11: Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 12: Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 13: Veirify 2.4 GHz radio is disabled ");
			LOGGER.info(" 14: Verify the private wifi 2.4 GHz  SSID is broadcasting in connected client");
			LOGGER.info(" 15: Veirify 2.4 GHz radio is reenabled");
			LOGGER.info(" 16: Verify the private wifi 2.4 GHz  SSID is broadcasting in connected client");
			LOGGER.info(
					" 17: Connect  the connected client  in the setup to 2.4 GHz SSID and verify connection status after 2.4 GHz radio is reenabled");
			LOGGER.info(" 18: Verify client associated associated with  2.4 GHz using webpa");
			LOGGER.info(
					" 19: Verify the client mac address and connection type connected with 2.4 GHz in LM.txt.0 log file");
			LOGGER.info(" 20: Verify the connected clients details are IP and MAC in Gateway");
			LOGGER.info(" 21: Verify  the correct IPv4  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(" 22: Verify  the correct IPv6  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(
					" 23: Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 24: Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 25: Verify disconnecting the 2.4 GHz private wifi SSID");
			LOGGER.info(" 26: Connect  the connected client  in the setup to 5 GHz SSID and verify connection status");
			LOGGER.info(" 27: Verify client associated associated with 5 GHz using webpa");
			LOGGER.info(
					" 28: Verify the client mac address and connection type connected with 5 GHz in LM.txt.0 log file");
			LOGGER.info(" 29: Verify the connected clients details are IP and MAC in Gateway");
			LOGGER.info(" 30: Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
			LOGGER.info(" 31: Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
			LOGGER.info(
					" 32: Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
			LOGGER.info(
					" 33: Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
			LOGGER.info(" 34: Verify 5 GHz radio is disabled ");
			LOGGER.info(" 35: Verify the private wifi 5 GHz  SSID is broadcasting in connected client");
			LOGGER.info(" 36: Verify 5 GHz radio is reenabled ");
			LOGGER.info(" 37: Verify the private wifi 5 GHz  SSID is broadcasting in connected client ");
			LOGGER.info(
					" 38: Connect  the connected client  in the setup to 5 GHz SSID and verify connection status after 5 GHz radio is reenabled");
			LOGGER.info(" 39: Verify client associated associated with 5 GHz using webpa");
			LOGGER.info(
					" 40: Verify the client mac address and connection type connected with 5 GHz in LM.txt.0 log file");
			LOGGER.info(" 41: Verify the connected clients details are IP and MAC in Gateway");
			LOGGER.info(" 42: Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
			LOGGER.info(" 43: Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
			LOGGER.info(
					" 44: Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
			LOGGER.info(
					" 45: Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
			LOGGER.info(" 46: Verify disconnecting the 5 GHz private wifi SSID ");

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			executePreConditionToFactoryResetAndReacitivateDevice(device);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1 : VERIFY FOR 2.4GHZ RADIO DEFAULT CHANNEL BANDWIDTH
			 * 
			 */

			errorMessage = null;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY FOR 2.4GHZ RADIO DEFAULT CHANNEL BANDWIDTH.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : VERIFY FOR 2.4GHZ RADIO DEFAULT CHANNEL BANDWIDTH USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN DEFAULT CHANNEL BANDWIDTH 20GHZ-40GHZ");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO RETIREVE THE DEFAULT CHANNEL BANDWIDTH FOR 2.4GHZ RADIO.";
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND);
			status = BroadBandWiFiUtils.validateDefaultOperatingBandwidth(device, tapEnv, response,
					BroadBandTestConstants.BAND_2_4GHZ);

			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY RETRIEVED DEFAULT CHANNEL BANDWIDTH FOR 2.4GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
			/**
			 * Step 2 : VERIFY FOR 5GHZ RADIO DEFAULT CHANNEL BANDWIDTH
			 * 
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY FOR 5GHZ RADIO DEFAULT CHANNEL BANDWIDTH.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : VERIFY FOR 5GHZ RADIO DEFAULT CHANNEL BANDWIDTH USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_5GHZ_BAND);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN DEFAULT CHANNEL BANDWIDTH 20GHZ-40GHZ-40GHZ");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO RETIREVE THE DEFAULT CHANNEL BANDWIDTH FOR FOR 5GHZ RADIO.";
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_5GHZ_BAND);
			status = BroadBandWiFiUtils.validateDefaultOperatingBandwidth(device, tapEnv, response,
					BroadBandTestConstants.BAND_5GHZ);

			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY RETRIEVED DEFAULT CHANNEL BANDWIDTH FOR 5GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
			/**
			 * Step 3 : VERIFY FOR 2.4GHZ RADIO IS ENABLED BY DEFAULT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY FOR 2.4GHZ RADIO IS ENABLED BY DEFAULT");
			LOGGER.info(
					"STEP " + stepNumber + ": ACTION : VERIFY FOR 2.4GHZ RADIO IS ENABLED BY DEFAULT USING WEBPA PARAM "
							+ BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN THE DEFAULT STATUS AS 'TRUE'");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO RETIREVE THE DEFAULT VALUE AS 'TRUE' FOR 2.4 GHZ RADIO.";
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY RETIREVED THE DEFAULT STATUS AS 'TRUE' FOR 2.4 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
			/**
			 * Step 4 : VERIFY FOR 5GHZ RADIO IS ENABLED BY DEFAULT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY FOR 5GHZ RADIO IS ENABLED BY DEFAULT");
			LOGGER.info(
					"STEP " + stepNumber + ": ACTION : VERIFY FOR 5GHZ RADIO IS ENABLED BY DEFAULT USING WEBPA PARAM "
							+ BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN THE DEFAULT STATUS AS 'TRUE'");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO RETIREVE THE DEFAULT VALUE AS 'TRUE' FOR 5 GHZ RADIO.";
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY RETIREVED THE DEFAULT STATUS AS 'TRUE' FOR 5 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
			/**
			 * Step 5 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 2.4GHz SSID";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			try {
				deviceConnectedWith2Ghz = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWith2Ghz != null);
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 2.4GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 6-7 :
			 */
			verifyConnectedclientDetailsWithGivenClient(device, testId, deviceConnectedWith2Ghz,
					BroadBandTestConstants.BAND_2_4GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_CONNECTED_DEVICES_DETAILS.replace(
							BroadBandTestConstants.TR181_NODE_REF,
							BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID),
					deviceDateTime);

			/**
			 * STEP 8 : VERIFY 2.4GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			verifyClientMacInGateway(device, testId, deviceConnectedWith2Ghz, WiFiFrequencyBand.WIFI_BAND_2_GHZ,
					stepNumber);

			/**
			 * SETP : 9- 12
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					BroadBandTestConstants.BAND_2_4GHZ);
			/**
			 * Step 13 : SET AND VERIFY 2.4 GHZ RADIO IS DISABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY 2.4 GHZ RADIO IS DISABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY 2.4 GHZ RADIO IS DISABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 2.4 GHZ RADIO MUST BE DISABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO DISABLE THE 2.4 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY DISABLED THE 2.4 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
			/**
			 * Step 14 : VERIFY THE PRIVATE WIFI 2.4 GHZ SSID IS BROADCASTING IN CONNECTED
			 * CLIENT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE PRIVATE WIFI 2.4 GHZ  SSID IS BROADCASTING IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS DISABLED ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND FOR WINDOWS: 'NETSH WLAN SHOW NETWORKS | GREP -I '<PRIVATE_SSID_2GHZ>' OR LINUX : 'SUDO IWLIST WLAN0 SCAN' ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : PRIVATE WIFI 2.4 GHZ SSID SHOULD NOT BE BROADCASTED IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS DISABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY PRIVATE WIFI 2.4 GHZ SSID BROADCASTING STATUS IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS DISABLED";
			status = BroadBandWiFiUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDeviceWithPollDuration(device,
					tapEnv, deviceConnectedWith2Ghz, WiFiFrequencyBand.WIFI_BAND_2_GHZ, false,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : PRIVATE WIFI 2.4 GHZ SSID NOT BROADCASTED IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS DISABLED.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
			/**
			 * Step 15 : VERIFY 2.4 GHZ RADIO IS REENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY 2.4 GHZ RADIO IS REENABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : VERIFY 2.4 GHZ RADIO IS REENABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 2.4 GHZ RADIO MUST BE REENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO REENABLE THE 2.4 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY REENABLED THE 2.4 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
			/**
			 * Step 16 : VERIFY THE PRIVATE WIFI 2.4 GHZ SSID IS BROADCASTING IN CONNECTED
			 * CLIENT,WHEN 2.4 GHZ RADIO IS REENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE PRIVATE WIFI 2.4 GHZ  SSID IS BROADCASTING IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS REENABLED ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND FOR WINDOWS: 'NETSH WLAN SHOW NETWORKS | GREP -I '<PRIVATE_SSID_2GHZ>' OR LINUX : 'SUDO IWLIST WLAN0 SCAN' ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : PRIVATE WIFI 2.4 GHZ SSID SHOULD BE BROADCASTED IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS REENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY PRIVATE WIFI 2.4 GHZ SSID BROADCASTING STATUS IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS REENABLED";
			status = BroadBandWiFiUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDeviceWithPollDuration(device,
					tapEnv, deviceConnectedWith2Ghz, WiFiFrequencyBand.WIFI_BAND_2_GHZ, true,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : PRIVATE WIFI 2.4 GHZ SSID IS BROADCASTED IN CONNECTED CLIENT,AFTER 2.4 GHZ RADIO IS REENABLED.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
			/**
			 * Step 17 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHZ SSID AND
			 * VERIFY CONNECTION STATUS AFTER 2.4 GHZ RADIO IS REENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHZ SSID AND VERIFY CONNECTION STATUS AFTER 2.4 GHZ RADIO IS REENABLED");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD,AFTER 2.4 GHZ RADIO IS REENABLED");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 2.4 GHZ SSID,AFTER 2.4 GHZ RADIO IS REENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT TO 2.4 GHZ SSID,AFTER 2.4 GHZ RADIO IS REENABLED";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			status = BroadBandConnectedClientUtils.connectGivenWiFiCapableClientAndConnectWithGivenWiFiBand(device,
					tapEnv, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_2_4GHZ);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFYLLY CONNECTED THE CLIENT WITH 2.4 GHZ SSID,AFTER 2.4 GHZ RADIO IS REENABLED.");
			} else {
				deviceConnectedWith2Ghz = null;
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 18-19 : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
			 * /rdklogs/logs/LM.txt.0 LOG
			 */
			verifyConnectedclientDetailsWithGivenClient(device, testId, deviceConnectedWith2Ghz,
					BroadBandTestConstants.BAND_2_4GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_CONNECTED_DEVICES_DETAILS.replace(
							BroadBandTestConstants.TR181_NODE_REF,
							BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID),
					deviceDateTime);

			/**
			 * STEP 20 : VERIFY 2.4GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			verifyClientMacInGateway(device, testId, deviceConnectedWith2Ghz, WiFiFrequencyBand.WIFI_BAND_2_GHZ,
					stepNumber);

			/**
			 * SETP : 21- 24
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					BroadBandTestConstants.BAND_2_4GHZ);

			/**
			 * Step 25: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHZ
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 2.4GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 2.4GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith2Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ));
			if (status) {
				deviceConnectedWith2Ghz = null;
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL: SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 2.4GHZ SSID");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 26 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 5 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY CONNECTING THE WI-FI CLIENT IN THE SETTOP TO 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD");
			LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: THE CONNECTION MUST BE SUCCESSFUL");
			LOGGER.info("#######################################################################################");
			errorMessage = "FAILED TO CONNECTED THE WIFI CLIENT TO 5GHz SSID";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			try {
				deviceConnectedWith5Ghz = BroadBandConnectedClientUtils
						.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWith5Ghz != null);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 5GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 27-28 : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
			 * /rdklogs/logs/LM.txt.0 LOG
			 */
			verifyConnectedclientDetailsWithGivenClient(device, testId, deviceConnectedWith5Ghz,
					BroadBandTestConstants.BAND_5GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_CONNECTED_DEVICES_DETAILS.replace(
							BroadBandTestConstants.TR181_NODE_REF,
							BroadBandWebPaConstants.WEBPA_INDEX_5_GHZ_PRIVATE_SSID),
					deviceDateTime);

			/**
			 * STEP 29 : VERIFY 5GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			verifyClientMacInGateway(device, testId, deviceConnectedWith5Ghz, WiFiFrequencyBand.WIFI_BAND_5_GHZ,
					stepNumber);

			/**
			 * SETP : 30- 33
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					BroadBandTestConstants.BAND_5GHZ);
			/**
			 * Step 34 : SET AND VERIFY 5 GHZ RADIO IS DISABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY 5 GHZ RADIO IS DISABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY 5 GHZ RADIO IS DISABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 5 GHZ RADIO MUST BE DISABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO DISABLE THE 5 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY DISABLED THE 5 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
			/**
			 * Step 35 : VERIFY THE PRIVATE WIFI 5 GHZ SSID IS BROADCASTING IN CONNECTED
			 * CLIENT
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE PRIVATE WIFI 5 GHZ  SSID IS BROADCASTING IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS DISABLED ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND FOR WINDOWS: 'NETSH WLAN SHOW NETWORKS | GREP -I '<PRIVATE_SSID_2GHZ>' OR LINUX : 'SUDO IWLIST WLAN0 SCAN' ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : PRIVATE WIFI 5 GHZ SSID SHOULD NOT BE BROADCASTED IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS DISABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY PRIVATE WIFI 5 GHZ SSID BROADCASTING STATUS IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS DISABLED";
			status = BroadBandWiFiUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDeviceWithPollDuration(device,
					tapEnv, deviceConnectedWith5Ghz, WiFiFrequencyBand.WIFI_BAND_5_GHZ, false,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : PRIVATE WIFI 5 GHZ SSID NOT BROADCASTED IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS DISABLED.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
			/**
			 * Step 36 : VERIFY 5 GHZ RADIO IS REENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY 5 GHZ RADIO IS REENABLED ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : VERIFY 5 GHZ RADIO IS REENABLED USING WEBPA ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : 5 GHZ RADIO MUST BE REENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO REENABLE THE 5 GHZ RADIO";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY REENABLED THE 5 GHZ RADIO.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
			/**
			 * Step 37 : VERIFY THE PRIVATE WIFI 5 GHZ SSID IS BROADCASTING IN CONNECTED
			 * CLIENT,WHEN 5 GHZ RADIO IS REENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY THE PRIVATE WIFI 5 GHZ  SSID IS BROADCASTING IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS REENABLED ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND FOR WINDOWS: 'NETSH WLAN SHOW NETWORKS | GREP -I '<PRIVATE_SSID_2GHZ>' OR LINUX : 'SUDO IWLIST WLAN0 SCAN' ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : PRIVATE WIFI 5 GHZ SSID SHOULD BE BROADCASTED IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS REENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY PRIVATE WIFI 5 GHZ SSID BROADCASTING STATUS IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS REENABLED";
			status = BroadBandWiFiUtils.scanAndVerifyVisibleSsidFromWiFiConnectedClientDeviceWithPollDuration(device,
					tapEnv, deviceConnectedWith5Ghz, WiFiFrequencyBand.WIFI_BAND_5_GHZ, true,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : PRIVATE WIFI 5 GHZ SSID IS BROADCASTED IN CONNECTED CLIENT,AFTER 5 GHZ RADIO IS REENABLED.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
			/**
			 * Step 38 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHZ SSID AND VERIFY
			 * CONNECTION STATUS AFTER 5 GHZ RADIO IS REENABLED
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHZ SSID AND VERIFY CONNECTION STATUS AFTER 5 GHZ RADIO IS REENABLED");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD,AFTER 5 GHZ RADIO IS REENABLED");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 5 GHZ SSID,AFTER 5 GHZ RADIO IS REENABLED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT TO 5 GHZ SSID,AFTER 5 GHZ RADIO IS REENABLED";
			deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			status = BroadBandConnectedClientUtils.connectGivenWiFiCapableClientAndConnectWithGivenWiFiBand(device,
					tapEnv, deviceConnectedWith5Ghz, BroadBandTestConstants.BAND_5GHZ);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFYLLY CONNECTED THE CLIENT WITH 5 GHZ SSID,AFTER 5 GHZ RADIO IS REENABLED.");
			} else {
				deviceConnectedWith5Ghz = null;
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 39-40 : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED IN
			 * /rdklogs/logs/LM.txt.0 LOG
			 */
			verifyConnectedclientDetailsWithGivenClient(device, testId, deviceConnectedWith5Ghz,
					BroadBandTestConstants.BAND_5GHZ,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_CONNECTED_DEVICES_DETAILS.replace(
							BroadBandTestConstants.TR181_NODE_REF,
							BroadBandWebPaConstants.WEBPA_INDEX_5_GHZ_PRIVATE_SSID),
					deviceDateTime);

			/**
			 * STEP 41 : VERIFY 5GHz CONNECTED CLIENT'S MAC ADDRESS IN GATEWAY
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			verifyClientMacInGateway(device, testId, deviceConnectedWith5Ghz, WiFiFrequencyBand.WIFI_BAND_5_GHZ,
					stepNumber);

			/**
			 * SETP : 42- 45
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					BroadBandTestConstants.BAND_5GHZ);

			/**
			 * Step 46: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5
			 * GHZ
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 5GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 5GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith5Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ));
			if (status) {
				deviceConnectedWith5Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 5GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VALIDATING THE DEFAULT CHANNEL BANDWIDTH AND ENABLE & DISABLE THE RADIO FOR 2.4GHZ & 5GHZ WITH CONNECTED CLIENTS "
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS:");
			executePostConditionToVerifyDefaultRadioStatus(device);
			executePostConditionToDisconnectClients(device, deviceConnectedWith2Ghz, deviceConnectedWith5Ghz);
			executePostConditionToReActivateDevice(device, isReactivated);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CHANNEL-5001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 * Method to verify the Given client mac using webpa and Log file
	 * 
	 * @param device            Dut instance
	 * @param testId            String test case Id
	 * @param deviceConnected   String DeviceConnected
	 * @param wifiFrequencyBand String wififrequency
	 * @param webParameter      String param
	 * @param deviceDateTime    String device time
	 * @throws TestException
	 * 
	 * @author prasanthreddy.a
	 * @refactor Athira
	 */
	public static void verifyConnectedclientDetailsWithGivenClient(Dut device, String testId, Dut deviceConnected,
			String wifiFrequencyBand, String webParameter, String deviceDateTime) throws TestException {
		LOGGER.debug("STARTING METHOD : verifyConnectedclientDetailsWithGivenClient()");
		String step = null;
		boolean status = false;
		String errorMessage = null;
		String response = null;
		try {
			String wifiBand = wifiFrequencyBand.equalsIgnoreCase(BroadBandTestConstants.BAND_2_4GHZ)
					? BroadBandTestConstants.BAND_2_4GHZ
					: BroadBandTestConstants.BAND_5GHZ;
			String hostName = ((Device) deviceConnected).getConnectedDeviceInfo().getUserName();
			String connectionType = ((Device) deviceConnected).getConnectedDeviceInfo().getConnectionType();
			String hostMacAddress = ((Device) deviceConnected).getConnectedDeviceInfo().getWifiMacAddress();
			LOGGER.info("CONNECTION TYPE :" + connectionType);
			LOGGER.info("HOST NAME :" + hostName);
			LOGGER.info("HOST MACADDRESS :" + hostMacAddress);

			/**
			 * Step : VERIFY CONNECTED CLIENTS ASSOCIATED WITH GIVEN GHZ USING WEBPA.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION :VERIFY CONNECTED CLIENT MAC ASSOCIATED WITH " + wifiBand);
			LOGGER.info("STEP " + stepNumber + ": ACTION : GET CONNECTED CLIENT DETAILS ASSOCIATED WITH " + wifiBand
					+ " SSID USING WEBPA PARAM " + webParameter);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : IT SHOULD RETURN THE ASSOCIATED DEVICE DETAILS FOR "
					+ wifiBand + " SSID AND MAC SHOULD BE VALIDATED");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO GET THE ASSOCIATED DEVICE DETAILS FOR " + wifiBand + " SSID";

			long startTime = System.currentTimeMillis();
			do {
				try {
					response = tapEnv.executeWebPaCommand(device, webParameter);

				} catch (Exception e) {
					LOGGER.error("Exception caught while executing webpa command " + e.getMessage());
				}
				status = CommonMethods.isNotNull(response) && CommonUtils
						.patternSearchFromTargetString(response.toUpperCase(), hostMacAddress.toUpperCase());

			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : ASSOCIATED DEVICE MAC FOR " + wifiBand
						+ " SSID IS VERIFIED");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH GIVEN
			 * GHZ .
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION :VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH " + wifiBand
					+ " SSID");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : EXECUTE COMMAND :grep -i 'RDKB_CONNECTED_CLIENTS: Client type is <CLIENT_TYPE>, MacAddress is <MAC_ADDRESS> and HostName is <HOST_NAME> appeared online' /rdklogs/logs/LM.txt.0 ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : MUST RETURN THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH " + wifiBand
					+ " SSID");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH " + wifiBand;
			status = BroadBandConnectedClientUtils.verifyConnectedClientDetailsInLMlog(device, tapEnv, connectionType,
					hostMacAddress, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, deviceDateTime);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY VERIFIED THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH "
						+ wifiBand + " SSID");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#####################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
		} catch (TestException e) {
			LOGGER.error(errorMessage);
			throw new TestException(errorMessage);
		}
		LOGGER.debug("ENDING METHOD : verifyConnectedclientDetailsWithGivenClient()");
	}

	/**
	 *
	 * Test Case : Verify the WIFI Connectivity on Dual Band Radio for Client for
	 * 2.4 and 5 GHz with security mode WPA2-PSK
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE CONDITION 1 : Verify the private wifi 2.4 GHz & 5 GHz SSID's is
	 * broadcasting in connected client.</li>
	 * <li>PRE CONDITION 2 : Get the default value for operating standard 2.4
	 * GHz.</li>
	 * <li>PRE CONDITION 3 : Get the default value for operating standard 5
	 * GHz.</li>
	 * <li>PRE CONDITION 4 : Get the default channel value for 2.4 GHz</li>
	 * <li>PRE CONDITION 5 : Get the default channel value for 5 GHz.</li>
	 * <li>PRE CONDITION 6 : Set the channel selection mode to manual and respective
	 * channel value for 2.4 GHz and 5 GHz via webpa.</li>
	 * <li>PRE CONDITION 7 : Verify the security mode as 'WPA2-Personal' for 2.4 GHz
	 * SSID</li>
	 * <li>PRE CONDITION 8 : Verify the security mode as 'WPA2-Personal' for 5 GHz
	 * SSID</li>
	 * <li>Step 1 : Set and verify the value of operation standard as a for 5
	 * GHz.</li>
	 * <li>Step 2 : Set and verify the value of operation standard as b for 2.4
	 * GHz.</li>
	 * <li>Step 3 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 4 : Connect the another client in the setup to 5 GHz SSID and verify
	 * connection status.</li>
	 * <li>Step 5 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 6 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 7 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 8 : Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 9 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 10 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 11 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 12 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 13 : Set and verify the value of operation standard as n for 2.4
	 * GHz</li>
	 * <li>Step 14 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 15 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 16 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 17 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 18 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 19 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 20 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 21 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 22 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 23 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 24 : Set and verify the value of operation standard as ac for 5
	 * GHz</li>
	 * <li>Step 25 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 26 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 27 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 28 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 29 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 30 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 31 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 32 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 33 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 34 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 35 : Set and verify the value of operation standard as b for 2.4
	 * GHz</li>
	 * <li>Step 36 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 37 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 38 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 39 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 40 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 41 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 42 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 43 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 44 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 45 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 46 : Set and verify the value of operation standard as g for 2.4
	 * GHz</li>
	 * <li>Step 47 : Set and verify the value of operation standard as n for 5
	 * GHz</li>
	 * <li>Step 48 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 49 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 50 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 51 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 52 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 53 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 54 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 55 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 56 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 Ghz.</li>
	 * <li>Step 57 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 Ghz.</li>
	 * <li>Step 58 : Set and verify the value of operation standard as n for 2.4
	 * GHz</li>
	 * <li>Step 59 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 60 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 61 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 62 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 63 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 64 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 65 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 66 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 67 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 68 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 69 : Verify disconnecting the client connected with 2.4 GHz SSID in
	 * the setup.</li>
	 * <li>Step 70 : Verify disconnecting the client connected with 5 GHz SSID in
	 * the setup.</li>
	 * <li>POST CONDITION 1 : Revert the default channel selection mode,operating
	 * standard,default channel value for 2.4 GHz and 5 GHz</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 *
	 */

	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-SECTY-WPA2-5002")
	public void testToVerifyWifiOnDualBandSecurityWpa2(Dut device) {
		String testCaseId = "TC-RDKB-WIFI-SECTY-WPA2-502";
		String errorMessage = null;
		boolean status = false;
		stepNumber = 1;
		postConStepNumber = 0;
		preConStepNumber = 0;
		String step = "S" + stepNumber;
		List<Dut> ssidVisibleDevices = new ArrayList<Dut>();
		Dut ssidVisibleDeviceOne = null;
		Dut ssidVisibleDeviceTwo = null;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-SECTY-WPA2-5002");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the WIFI Connectivity on Dual Band Radio for Client for 2.4 and 5 GHz with security mode WPA2-PSK.");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					"PRE CONDITION 1 : Verify the private wifi 2.4 GHz & 5 GHz SSID's is broadcasting in connected client.");
			LOGGER.info("PRE CONDITION 2 : Get the default value for operating standard 2.4 GHz.");
			LOGGER.info("PRE CONDITION 3 : Get the default value for operating standard 5 GHz.");
			LOGGER.info("PRE CONDITION 4 : Get the default channel value for 2.4 GHz ");
			LOGGER.info("PRE CONDITION 5 : Get the default channel value for 5 GHz.");
			LOGGER.info(
					"PRE CONDITION 6 : Set the channel selection mode to manual and respective channel value for 2.4 GHz and 5 GHz via webpa.");
			LOGGER.info("PRE CONDITION 7 : Verify the security mode as 'WPA2-Personal' for 2.4 GHz SSID");
			LOGGER.info("PRE CONDITION 8 : Verify the security mode as 'WPA2-Personal' for 5 GHz SSID");
			LOGGER.info("Step 1 : Set and verify the value of operation standard as a for 5 GHz.");
			LOGGER.info("Step 2 : Set and verify the value of operation standard as b for 2.4 GHz.");
			LOGGER.info(
					"Step 3 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info("Step 4 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 5 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 6 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 7 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 8 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 9 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 10 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 11 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 12 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 13 : Set and verify the value of operation standard as n for 2.4 GHz");
			LOGGER.info(
					"Step 14 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 15 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 16 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 17 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 18 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 19 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 20 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 21 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 22 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 23 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 24 : Set and verify the value of operation standard as ac for 5 GHz");
			LOGGER.info(
					"Step 25 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 26 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 27 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 28 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 29 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 30 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 31 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 32 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 33 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 34 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 35 : Set and verify the value of operation standard as b for 2.4 GHz ");
			LOGGER.info(
					"Step 36 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 37 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 38 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 39 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 40 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 41 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 42 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 43 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 44 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 45 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 46 : Set and verify the value of operation standard as g for 2.4 GHz ");
			LOGGER.info("Step 47 : Set and verify the value of operation standard as n for 5 GHz ");
			LOGGER.info(
					"Step 48 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 49 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 50 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 51 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 52 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 53 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 54 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 55 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 56 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 57 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 58 : Set and verify the value of operation standard as n for 2.4 GHz ");
			LOGGER.info(
					"Step 59 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 60 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 61 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 62 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 63 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 64 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 65 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 66 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 67 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz.");
			LOGGER.info(
					"Step 68 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz.");
			LOGGER.info("Step 69 : Verify disconnecting the client connected with 2.4 GHz SSID in the setup.");
			LOGGER.info("Step 70 : Verify disconnecting the client connected with 5 GHz SSID in the setup.");
			LOGGER.info(
					"POST CONDITION 1 : Revert the default channel selection mode,operating standard,default channel value for 2.4 GHz and 5 GHz.");
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			/**
			 * PRE-CONDITION 1 : VERIFY THE PRIVATE WIFI 2.4 GHz AND 5 GHz SSID'S ARE
			 * BROADCASTING IN CONNECTED CLIENT
			 */
			preConStepNumber++;
			ssidVisibleDevices = BroadBandPreConditionUtils.executePreConditionToGetBothPrivateWiFiSsidsVisibleDevices(
					device, tapEnv, preConStepNumber, BroadBandTestConstants.CONSTANT_2);
			ssidVisibleDeviceOne = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_0);
			ssidVisibleDeviceTwo = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_1);
			/**
			 * PRE-CONDITION 2-3 : GET THE DEFAULT VALUES
			 */
			executePreConditionToGetDefaultOperStandard(device);

			/**
			 * PRE-CONDITION 4-5 : GET THE DEFAULT VALUES
			 */
			executePreConditionToGetDefaultChannel(device);

			/**
			 * PRE CONDITION 6 : SET THE CHANNEL SELECTION MODE TO MANUAL AND RESPECTIVE
			 * CHANNEL VALUE FOR 2.4 GHz AND 5 GHz VIA WEBPA.
			 */
			executePreConditionToSetChannel(device);

			/**
			 * PRE-CONDITION 7-8 : VERIFY THE SECURITY MODE "WPA2-Personal" FOR 2.4 and 5
			 * GHz SSID
			 */
			preConStepNumber++;
			BroadBandPreConditionUtils.executePreConditionToSetSecurityMode(device, tapEnv, preConStepNumber,
					BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1 : SET AND VERIFY THE VALUE OF 5 GHz WITH OPERATING STANDARD AS a
			 */
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_A);

			/**
			 * STEP 2 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS b
			 */
			stepNumber++;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_B);

			/**
			 * Step 3 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 4 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 5 - 8 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 9 - 12 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 9;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 13 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS n
			 */
			stepNumber = 13;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_N);

			/**
			 * Step 14 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 15 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);
			/**
			 * SETP 16 - 19 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 20 - 23 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 20;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 24 : SET AND VERIFY THE VALUE OF 5 GHz WITH OPERATING STANDARD AS ac
			 */
			stepNumber = 24;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_AC);

			/**
			 * Step 25 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 26 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);
			/**
			 * SETP 27 - 30 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 31 - 34 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 31;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 35 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS b
			 */
			stepNumber = 35;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_B);

			/**
			 * Step 36 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 37 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 38 - 41 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 42 - 45 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 42;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 46 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS g
			 */
			stepNumber = 46;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_G);

			/**
			 * STEP 47 : SET AND VERIFY THE VALUE OF 5 GHz WITH OPERATING STANDARD AS n
			 */
			stepNumber++;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_N);

			/**
			 * Step 48 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 49 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 50 - 53 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 54 - 57 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 54;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 58 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS n
			 */
			stepNumber = 58;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_N);

			/**
			 * Step 59 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 60 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 61 - 64 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 65 - 68 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 65;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * Step 69 : VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR
			 * 2.4 GHz
			 */
			stepNumber = 69;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 70 : VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR
			 * 2.4 GHz
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VALIDATING WIFI CONNECTIVITY ON DUAL BAND RADIO FOR CLIENT FOR 2.4 AND 5 GHz WITH SECURITY MODE 'WPA2-Personal' : "
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS:");
			/**
			 * POST CONDITION 1 : REVERT THE DEFAULT CHANNEL SELECTION MODE,OPERATING
			 * STANDARD,DEFAULT CHANNEL VALUE FOR 2.4 GHz AND 5 GHz VIA WEBPA.
			 */
			executePostConditionToSetChannelAndOprStd(device);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-SECTY-WPA2-5002");
	}

	/**
	 * 
	 * Test Case : Verify WG supports Open Security mode for 2.4 &5 GHZ frequency
	 * band.
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : VERIFY 2.4 GHZ PRIVATE SSID ENABLED</li>
	 * <li>PRE-CONDITION 2 : VERIFY 5 GHZ PRIVATE SSID ENABLED</li>
	 * <li>PRE-CONDITION 3 : GET THE DEFAULT VALUE FOR OPERATING STANDARD 2.4
	 * GHZ</li>
	 * <li>PRE-CONDITION 4 : GET THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ</li>
	 * <li>Step 1 : Set and verify the value of operational transmission rate of the
	 * 2.4 GHz with operating standard as b/g/n</li>
	 * <li>Step 2 : Set and verify the security mode as "none" for 2.4 GHz SSID</li>
	 * <li>Step 3 : Set and verify the value of operational transmission rate of the
	 * 5GHz with operating standard as a/n/ac</li>
	 * <li>Step 4 : Set and verify the security mode as "none" for 5 GHz SSID</li>
	 * <li>Step 5 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 6 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 7 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 8 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 9 : Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 10 : Connect the connected client in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 11 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 12 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 13 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 14 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 15 : Verify disconnecting the 2.4 GHz SSID</li>
	 * <li>Step 16 : Verify disconnecting the 5 GHz SSID</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @refactor Alan_Bivera
	 *
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-WIFI-CC-OPEN-5001")
	public void testToVerifyWiFiConnSecurityModeOpen(Dut device) {
		String testId = "TC-RDKB-WIFI-CC-OPEN-501";
		String step = null;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWith2Ghz = null;
		Dut deviceConnectedWith5Ghz = null;
		boolean isSecModeChanged2Ghz = false;
		boolean isSecModeChanged5Ghz = false;
		stepNumber = 1;
		preConStepNumber = 0;
		postConStepNumber = 0;
		step = "S" + stepNumber;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CC-OPEN-5001");
			LOGGER.info("TEST DESCRIPTION: Verify WG supports Open Security mode for 2.4 &5 GHZ frequency band.");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					" 1 : Set and verify  the value of operational transmission rate of the 2.4 GHz with operating standard as b/g/n");
			LOGGER.info(" 2 : Set and verify  the security mode as 'none' for 2.4 GHz SSID");
			LOGGER.info(
					" 3 : Set and verify  the value of operational transmission rate of the 5GHz with operating standard as a/n/ac");
			LOGGER.info(" 4 : Set and verify  the security mode as 'none' for 5 GHz SSID");
			LOGGER.info(
					" 5 : Connect  the connected client  in the setup to 2.4 GHz SSID and verify connection status");
			LOGGER.info(" 6 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(" 7 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(
					" 8 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 9 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 10 : Connect the connected client  in the setup to 5 GHz SSID and verify connection status");
			LOGGER.info(" 11 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
			LOGGER.info(" 12 : Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
			LOGGER.info(
					" 13 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
			LOGGER.info(
					" 14 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
			LOGGER.info(" 15 : Verify disconnecting the 2.4 GHz SSID");
			LOGGER.info(" 16 : Verify disconnecting the 5 GHz SSID");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			executePreConditionToVerifyPrivateSsidIsEnabled(device);
			executePreConditionToGetDefaultOperStandard(device);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4
			 * GHZ WITH OPERATING STANDARD AS b/g/n
			 */

			errorMessage = null;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4 GHZ WITH OPERATING STANDARD AS b/g/n.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET TO b/g/n");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET OPERATING STANDARD AS b/g/n.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
					WebPaDataTypes.STRING.getValue(),
					WifiOperatingStandard.OPERATING_STANDARD_B_G_N.getOperatingmode());
			if (!status && DeviceModeHandler.isRPIDevice(device)) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(), BroadbandPropertyFileHandler.get2GhzOperatingModeForRPi());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS b/g/n.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 2 : SET AND VERIFY THE SECURITY MODE "OPEN" FOR 2.4 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY  THE SECURITY MODE 'OPEN' FOR 2.4 GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE SECURITY MODE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED);

			LOGGER.info("STEP " + stepNumber + ": EXPECTED : SECURITY MODE MUST SET TO 'OPEN' FOR 2.4 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY MODE TO 'OPEN' FOR 2.4 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
					WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_NONE);
			if (status) {
				isSecModeChanged2Ghz = true;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY MODE TO 'OPEN' FOR 2.4 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 3 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 5
			 * GHZ WITH OPERATING STANDARD AS a/n/ac
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 5 GHZ WITH OPERATING STANDARD AS a/n/ac.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET TO a/n/ac");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET OPERATING STANDARD AS a/n/ac.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
					WebPaDataTypes.STRING.getValue(),
					WifiOperatingStandard.OPERATING_STANDARD_A_N_AC.getOperatingmode());
			if (!status && DeviceModeHandler.isRPIDevice(device)) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(), BroadbandPropertyFileHandler.get5GhzOperatingModeForRPi());
			}
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS a/n/ac.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 4 : SET AND VERIFY THE SECURITY MODE "NONE" FOR 5 GHZ SSID
			 * 
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : SET AND VERIFY  THE SECURITY MODE 'OPEN' FOR 5 GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE SECURITY MODE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : SECURITY MODE MUST SET TO 'OPEN' FOR 5 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY MODE TO 'OPEN' FOR 5 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
					WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_NONE);
			if (status) {
				isSecModeChanged5Ghz = true;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY MODE TO 'OPEN' FOR 5 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 5 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT INTO 2.4GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 2.4 GHZ SSID");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT WITH 2.4 GHZ SSID";
			try {
				deviceConnectedWith2Ghz = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWith2Ghz != null);
			if (status) {
				LOGGER.info(
						"STEP :  " + stepNumber + " : ACTUAL: CONNECTED THE WIFI CLIENT TO 2.4GHz SSID SUCCESSFULLY");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP : 6-9
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					BroadBandTestConstants.BAND_2_4GHZ);
			/**
			 * Step 10: VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 5 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT INTO 5 GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 5 GHz SSID AND PASSWORD");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 5 GHZ SSID");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT WITH 5 GHZ SSID";
			try {
				deviceConnectedWith5Ghz = BroadBandConnectedClientUtils.getOtherWiFiCapableClientDeviceAndConnect(
						device, tapEnv, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_5GHZ);
				status = (null != deviceConnectedWith5Ghz);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY CONNECTED THE CLIENT WITH 5 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 11-14 :
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					BroadBandTestConstants.BAND_5GHZ);

			/**
			 * Step 15: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHZ
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 2.4GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 2.4GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith2Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ));
			if (status) {
				deviceConnectedWith2Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 2.4GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 16: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5
			 * GHZ.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 5GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 5GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith5Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ));
			if (status) {
				deviceConnectedWith5Ghz = null;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 5GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VALIDATING THE WG SUPPORTS OPEN SECURITY MODE FOR 2.4 &5 GHZ FREQUENCY BAND : "
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS:");
			executePostConditionToSetDefaultOperStandard(device);
			executePostConditionToSetSecurityModeWPA2Personal(device, isSecModeChanged2Ghz, isSecModeChanged5Ghz);
			executePostConditionToDisconnectClients(device, deviceConnectedWith2Ghz, deviceConnectedWith5Ghz);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CC-OPEN-5001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 *
	 * Test Case : Verify WG supports WAP2(AES) Security mode for 2.4 &5 GHZ
	 * frequency band with valid & invalid key passpharse.
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : VERIFY 2.4 GHZ PRIVATE SSID ENABLED</li>
	 * <li>PRE-CONDITION 2 : VERIFY 5 GHZ PRIVATE SSID ENABLED</li>
	 * <li>PRE-CONDITION 3 : GET THE DEFAULT VALUE FOR OPERATING STANDARD 2.4
	 * GHZ</li>
	 * <li>PRE-CONDITION 4 : GET THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ</li>
	 * <li>Step 1 : Set and verify the value of operational transmission rate of the
	 * 2.4 GHz with operating standard as b/g/n</li>
	 * <li>Step 2 : Set and verify the security mode "WPA2-Personal" for 2.4 GHz
	 * SSID</li>
	 * <li>Step 3 : Set and verify the security encryption method as "AES" for 2.4
	 * GHz SSID</li>
	 * <li>Step 4 : Set and verify the value of operational transmission rate of the
	 * 5GHz with operating standard as a/n/ac</li>
	 * <li>Step 5 : Set and verify the security mode "WPA2-Personal" for 5 GHz
	 * SSID</li>
	 * <li>Step 6 : Set and verify the security encryption method as "AES" for 5 GHz
	 * SSID</li>
	 * <li>Step 7 : Connect the connected client in the setup to 2.4 GHz SSID with
	 * valid key passpharse and verify connection status</li>
	 * <li>Step 8 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 9 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 10 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 11 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 12 : Connect the connected client in the setup to 5 GHz SSID with
	 * valid key passpharse and verify connection status</li>
	 * <li>Step 13 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 14 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 15 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz</li>
	 * <li>Step 16 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz</li>
	 * <li>Step 17 : Verify disconnecting the 2.4 GHz SSID</li>
	 * <li>Step 18 : Verify disconnecting the 5 GHz SSID</li>
	 * <li>Step 19 : Connect the connected client in the setup to 2.4 GHz SSID with
	 * invalid key passpharse and verify connection status</li>
	 * <li>Step 20 : Verify whether have connectivity using that particular
	 * interface using IPV4/IPV6</li>
	 * <li>Step 21 : Connect the connected client in the setup to 5 GHz SSID with
	 * invalid key passpharse and verify connection status</li>
	 * <li>Step 22 : Verify whether have connectivity using that particular
	 * interface using IPV4/IPV6</li>
	 * </ol>
	 * 
	 * @param Dut {@link device}
	 * 
	 * @author Muthukumar
	 * @refactor Alan_Bivera
	 *
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-WIFI-CC-WPA2-5001")
	public void testToVerifyWiFiConnSecurityModeWAP2(Dut device) {
		String testId = "TC-RDKB-WIFI-CC-WPA2-501";
		String step = null;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWith2Ghz = null;
		Dut deviceConnectedWith5Ghz = null;
		BroadBandResultObject result = null;
		WifiOperatingStandard defaultOperatingStandard = null;
		stepNumber = 1;
		preConStepNumber = 0;
		postConStepNumber = 0;
		step = "S" + stepNumber;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-CC-WPA2-5001");
			LOGGER.info("TEST DESCRIPTION: Verify WG supports Open Security mode for 2.4 &5 GHZ frequency band.");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(
					" 1 : Set and verify  the value of operational transmission rate of the 2.4 GHz with operating standard as b/g/n");
			LOGGER.info(" 2 : Set and verify  the security mode 'WPA2-Personal' for 2.4 GHz SSID");
			LOGGER.info(" 3 : Set and verify  the security encryption method as 'AES' for 2.4 GHz SSID");
			LOGGER.info(
					" 4 : Set and verify  the value of operational transmission rate of the 5GHz with operating standard as a/n/ac");
			LOGGER.info(" 5 : Set and verify  the security mode 'WPA2-Personal' for 5 GHz SSID");
			LOGGER.info(" 6 : Set and verify  the security encryption method as 'AES' for 5 GHz SSID");
			LOGGER.info(
					" 7 : Connect  the connected client  in the setup to 2.4 GHz SSID with valid key passpharse and verify connection status");
			LOGGER.info(" 8 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(" 9 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(
					" 10 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 11 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 12 : Connect  the connected client  in the setup to 5 GHz SSID  with valid key passpharse and verify connection status");
			LOGGER.info(" 13 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
			LOGGER.info(" 14 : Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
			LOGGER.info(
					" 15 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 GHz ");
			LOGGER.info(
					" 16 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 GHz ");
			LOGGER.info(" 17 : Verify disconnecting the 2.4 GHz SSID ");
			LOGGER.info(" 18 : Verify disconnecting the 5 GHz SSID ");
			LOGGER.info(
					" 19 : Connect  the connected client  in the setup to 2.4 GHz SSID with invalid key passpharse and verify connection status ");
			LOGGER.info(" 20 : Verify whether have connectivity using that particular interface using IPV4/IPV6 ");
			LOGGER.info(
					" 21 : Connect  the connected client  in the setup to 5 GHz SSID with invalid key passpharse and verify connection status ");
			LOGGER.info(" 22 : Verify whether have connectivity using that particular interface using IPV4/IPV6 ");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			executePreConditionToVerifyPrivateSsidIsEnabled(device);
			executePreConditionToGetDefaultOperStandard(device);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			LOGGER.info("#######################################################################################");

			/**
			 * Step 1 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4
			 * GHZ WITH OPERATING STANDARD AS b/g/n
			 */
			errorMessage = null;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4 GHZ WITH OPERATING STANDARD AS b/g/n.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET TO b/g/n");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET OPERATING STANDARD AS b/g/n.";
			if (!DeviceModeHandler.isRPIDevice(device)) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(),
						WifiOperatingStandard.OPERATING_STANDARD_B_G_N.getOperatingmode());
			} else {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_2_4_GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(), BroadbandPropertyFileHandler.get2GhzOperatingModeForRPi());
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS b/g/n.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 2 : SET AND VERIFY THE SECURITY MODE WPA2-Personal" FOR 2.4 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY  THE SECURITY MODE 'WPA2-Personal' FOR 2.4 GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE SECURITY MODE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED);
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED : SECURITY MODE MUST SET TO 'WPA2-Personal' FOR 2.4 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY MODE TO 'WPA2-Personal' FOR 2.4 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
					WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY MODE TO 'WPA2-Personal' FOR 2.4 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 3 : SET AND VERIFY THE SECURITY ENCRYPTION METHOD AS "AES" FOR 2.4 GHZ
			 * SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE SECURITY ENCRYPTION METHOD AS 'AES' FOR 2.4 GHZ SSID");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY  THE SECURITY ENCRYPTION METHOD AS AES FOR 2.4GHz SSID USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_FOR_ENCRYPTIONMETHOD_IN_2GHZ_PRIVATE_WIFI);
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : SECURITY ENCRYPTION METHOD MUST SET TO 'AES' FOR 2.4 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY ENCRYPTION METHOD TO 'AES' FOR 2.4 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_ENCRYPTIONMETHOD_IN_2GHZ_PRIVATE_WIFI,
					WebPaDataTypes.STRING.getValue(),
					BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY ENCRYPTION METHOD TO 'AES' FOR 2.4 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 4 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 5
			 * GHZ WITH OPERATING STANDARD AS a/n/ac
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 5 GHZ WITH OPERATING STANDARD AS a/n/ac.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE OPERATING STANDARD MUST BE SET TO a/n/ac");
			LOGGER.info("#######################################################################################");
			defaultOperatingStandard = CommonMethods.isAtomSyncAvailable(device, tapEnv)
					|| (CommonMethods.isAtomSyncAvailable(device, tapEnv)
							&& DeviceModeHandler.isBusinessClassDevice(device))
									? WifiOperatingStandard.OPERATING_STANDARD_A_N
									: WifiOperatingStandard.OPERATING_STANDARD_A_N_AC;
			errorMessage = "UNABLE TO SET OPERATING STANDARD AS " + defaultOperatingStandard.getOperatingmode();

			if (!DeviceModeHandler.isRPIDevice(device)) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(), defaultOperatingStandard.getOperatingmode());
			} else {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
						WebPaDataTypes.STRING.getValue(), BroadbandPropertyFileHandler.get5GhzOperatingModeForRPi());
			}

			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + " : ACTUAL : SUCCESSFULLY CHANGED THE OPERATING STANDARD AS a/n/ac.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 5 : SET AND VERIFY THE SECURITY MODE "WPA2-Personal" FOR 5 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY  THE SECURITY MODE 'WPA2-Personal' FOR 5 GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : SET AND VERIFY THE SECURITY MODE USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED);
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : SECURITY MODE MUST SET TO 'WPA2-Personal' FOR 5 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY MODE TO 'WPA2-Personal' FOR 5 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5_GHZ_PRIVATE_SECURITY_MODEENABLED,
					WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY MODE TO 'WPA2-Personal' FOR 5 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 6 : SET AND VERIFY THE SECURITY ENCRYPTION METHOD AS "AES" FOR 5 GHZ
			 * SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : SET AND VERIFY THE SECURITY ENCRYPTION METHOD AS 'AES' FOR 5 GHZ SSID");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : SET AND VERIFY  THE SECURITY ENCRYPTION METHOD AS AES FOR 5GHz SSID USING WEBPA PARAM "
					+ BroadBandWebPaConstants.WEBPA_PARAM_FOR_ENCRYPTIONMETHOD_IN_5GHZ_PRIVATE_WIFI);
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED : SECURITY ENCRYPTION METHOD MUST SET TO 'AES' FOR 5 GHZ SSID");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO SET THE SECURITY ENCRYPTION METHOD TO 'AES' FOR 5 GHZ SSID.";
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_ENCRYPTIONMETHOD_IN_5GHZ_PRIVATE_WIFI,
					WebPaDataTypes.STRING.getValue(),
					BroadBandConnectedClientTestConstants.SECURITY_ENCRYPTION_METHOD_AES);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY CHANGED THE SECURITY ENCRYPTION METHOD TO 'AES' FOR 5 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 7 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT INTO 2.4GHZ SSID AND VALID PASSWORD");
			LOGGER.info(
					"STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND VALID PASSWORD");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 2.4 GHZ SSID AND VALID PASSWORD");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT WITH 2.4 GHZ SSID USING VALID PASSWORD";
			try {
				deviceConnectedWith2Ghz = BroadBandConnectedClientUtils
						.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (deviceConnectedWith2Ghz != null);
			if (status) {
				LOGGER.info("STEP :  " + stepNumber
						+ " : ACTUAL: CLIENT CONNECTED SUCCESSFULLYTHE TO 2.4GHz SSID USING VALID PASSWORD ");
			} else {
				LOGGER.error("STEP :  " + stepNumber + " : ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP : 8-11
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					BroadBandTestConstants.BAND_2_4GHZ);
			/**
			 * Step 12: VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 5 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT INTO 5 GHZ SSID AND VALID PASSWORD");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : CONNECT THE WI-FI CLIENT WITH 5 GHz SSID AND PASSWORD AND VALID PASSWORD");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 5 GHZ SSID AND VALID PASSWORD");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT WITH 5 GHZ SSID USING VALID PASSWORD";
			try {
				deviceConnectedWith5Ghz = BroadBandConnectedClientUtils.getOtherWiFiCapableClientDeviceAndConnect(
						device, tapEnv, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_5GHZ);
				status = (null != deviceConnectedWith5Ghz);
			} catch (TestException exception) {
				// Log & Suppress the exception
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CLIENT CONNECTED SUCCESSFULLYTHE TO 5GHz SSID USING VALID PASSWORD.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 13-16 :
			 */
			verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					BroadBandTestConstants.BAND_5GHZ);

			/**
			 * Step 17: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHZ
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 2.4GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 2.4GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith2Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_2_GHZ));
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 2.4GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 18: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5
			 * GHZ.
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP :  " + stepNumber
					+ " : DESCRIPTION : VERIFY DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber + " : ACTION : DISCONNECT THE WI-FI CLIENT CONNECTED WITH 5GHz SSID");
			LOGGER.info("STEP :  " + stepNumber
					+ " : EXPECTED: THE CLIENT MUST BE DISCONNECTED FROM 5GHz SSID SUCCESSFULLY");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABEL TO DISCONNECT THE CLIENT CONNECTED WITH 5GHZ SSID";
			status = ConnectedNattedClientsUtils.disconnectSSID(deviceConnectedWith5Ghz, tapEnv,
					BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							WiFiFrequencyBand.WIFI_BAND_5_GHZ));
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : SUCCESSFULLY DISCONNECTED THE CLIENT CONNECTED WITH 5GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 19 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHZ SSID WITH
			 * INVALID KEY PASSPHARSE AND VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : CONNECT  THE CONNECTED CLIENT  IN THE SETUP TO 2.4 GHZ SSID WITH INVALID KEY PASSPHARSE AND VERIFY CONNECTION STATUS");
			LOGGER.info(
					"STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND INVALID PASSWORD");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST NOT BE SUCCESSFUL FOR 2.4 GHZ SSID  AND INVALID PASSWORD");
			LOGGER.info("#####################################################################################");
			errorMessage = "CONNECTION SUCCESSFUL FOR 2.4 GHZ SSID WITH INVALID PASSWORD";
			String ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			if (CommonMethods.isNotNull(ssid)) {
				status = !ConnectedNattedClientsUtils.connectToSSID(deviceConnectedWith2Ghz, tapEnv, ssid,
						BroadBandTestConstants.INVALID_WIFI_PASSWORD);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTION FAILED FOR FOR 2.4 GHZ SSID WITH INVALID PASSWORD .");
			} else {
				deviceConnectedWith2Ghz = null;
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 20 : VERIFY INTERNET ACCESS BY USING WWW.FACEBOOK.COM WITH 2.4 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			if (!DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info("#####################################################################################");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : VERIFY INTERNET ACCESS BY USING WWW.FACEBOOK.COM WITH 2.4 GHZ SSID");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : EXECUTE COMMAND : curl --connect-timeout 20 -v https://www.facebook.com");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE INTERNET CONNECTION MUST NOT BE SUCCESSFUL");
				LOGGER.info("#####################################################################################");
				errorMessage = "INTERNET CONNECTION IS SUCCESSFUL WITH 2.4 GHZ SSID";
				try {
					result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv,
							deviceConnectedWith2Ghz, BroadBandTestConstants.URL_W3SCHOOLS);
					status = result.isStatus();
					errorMessage = result.getErrorMessage();
				} catch (Exception exception) {
					// Log & Suppress the exception
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber + " : ACTUAL : THE INTERNET CONNECTION MUST NOT BE SUCCESSFUL .");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
			} else {
				LOGGER.info("skipping teststep due to device setup dependency...");
				tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE, errorMessage,
						false);
			}

			/**
			 * Step 21 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHZ SSID WITH
			 * INVALID KEY PASSPHARSE AND VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : CONNECT  THE CONNECTED CLIENT  IN THE SETUP TO 5 GHZ SSID WITH INVALID KEY PASSPHARSE AND VERIFY CONNECTION STATUS");
			LOGGER.info(
					"STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND INVALID PASSWORD");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST NOT BE SUCCESSFUL FOR 5 GHZ SSID  AND INVALID PASSWORD");
			LOGGER.info("#####################################################################################");
			errorMessage = "CONNECTION SUCCESSFUL FOR 5 GHZ SSID WITH INVALID PASSWORD";
			ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			if (CommonMethods.isNotNull(ssid)) {
				status = !ConnectedNattedClientsUtils.connectToSSID(deviceConnectedWith5Ghz, tapEnv, ssid,
						BroadBandTestConstants.INVALID_WIFI_PASSWORD);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTION FAILED FOR FOR 5 GHZ SSID WITH INVALID PASSWORD .");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 22 : VERIFY INTERNET ACCESS BY USING WWW.FACEBOOK.COM WITH 5 GHZ SSID
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			if (!DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info("#####################################################################################");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : VERIFY INTERNET ACCESS BY USING WWW.FACEBOOK.COM WITH 5 GHZ SSID");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : EXECUTE COMMAND : curl --connect-timeout 20 -v https://www.facebook.com");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE INTERNET CONNECTION MUST NOT BE SUCCESSFUL ");
				LOGGER.info("#####################################################################################");
				errorMessage = "INTERNET CONNECTION IS SUCCESSFUL WITH 5 GHZ SSID";
				try {
					result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv,
							deviceConnectedWith5Ghz, BroadBandTestConstants.URL_W3SCHOOLS);
					status = result.isStatus();
					errorMessage = result.getErrorMessage();
				} catch (Exception exception) {
					// Log & Suppress the exception
					errorMessage = exception.getMessage();
					LOGGER.error(errorMessage);
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber + " : ACTUAL : THE INTERNET CONNECTION MUST NOT BE SUCCESSFUL .");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
			} else {
				LOGGER.info("skipping teststep due to device setup dependency...");
				tapEnv.updateExecutionForAllStatus(device, testId, step, ExecutionStatus.NOT_APPLICABLE, errorMessage,
						false);
			}

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VALIDATING THE WG SUPPORTS WAP2(AES) SECURITY MODE FOR 2.4 & 5 GHZ FREQUENCY BAND WITH VALID & INVALID KEY PASSPHARSE : "
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS:");
			executePostConditionToSetDefaultOperStandard(device);
			executePostConditionToDisconnectClients(device, deviceConnectedWith2Ghz, deviceConnectedWith5Ghz);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-CC-WPA2-5001");
		LOGGER.info("#######################################################################################");
	}

	/**
	 *
	 * Test Case : Verify the WIFI Connectivity on Dual Band Radio for Client for
	 * 2.4 and 5 GHz with security mode WPAWPA2-PSK
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE CONDITION 1 : Verify the private wifi 2.4 GHz & 5 GHz SSID's is
	 * broadcasting in connected client.</li> *
	 * <li>PRE CONDITION 2 : Get the default value for operating standard 2.4
	 * GHz.</li>
	 * <li>PRE CONDITION 3 : Get the default value for operating standard 5
	 * GHz.</li>
	 * <li>PRE CONDITION 4 : Get the default channel value for 2.4 GHz</li>
	 * <li>PRE CONDITION 5 : Get the default channel value for 5 GHz.</li>
	 * <li>PRE CONDITION 6 : Set the channel selection mode to manual and respective
	 * channel value for 2.4 GHz and 5 GHz via webpa.</li>
	 * <li>PRE CONDITION 7 : Verify the security mode as 'WPA-WPA2-Personal' for 2.4
	 * GHz SSID</li>
	 * <li>PRE CONDITION 8 : Verify the security mode as 'WPA-WPA2-Personal' for 5
	 * GHz SSID</li>
	 * <li>PRE CONDITION 9 : Perform apply settings for 2.4 and 5 GHz radio's</li>
	 * <li>Step 1 : Set and verify the value of operation standard as n for 5
	 * GHz.</li>
	 * <li>Step 2 : Set and verify the value of operation standard as n for 2
	 * GHz.</li>
	 * <li>Step 3 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 4 : Connect the another client in the setup to 5 GHz SSID and verify
	 * connection status.</li>
	 * <li>Step 5 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 6 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 7 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 8 : Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 9 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 10 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 11 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 12 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 13 : Set and verify the value of operation standard as g for 2.4
	 * GHz</li>
	 * <li>Step 14 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 15 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 16 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 17 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 18 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 19 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 20 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 21 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 22 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 23 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 24 : Set and verify the value of operation standard as b for 2.4
	 * GHz</li>
	 * <li>Step 25 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 26 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 27 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 28 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 29 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 30 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 31 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 32 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 33 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 34 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 35 : Set and verify the value of operation standard as ac for 5
	 * GHz</li>
	 * <li>Step 36 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 37 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 38 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 39 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 40 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 41 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 42 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 43 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 43 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 45 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 46 : Set and verify the value of operation standard as a for 5
	 * GHz</li>
	 * <li>Step 47 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 48 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 49 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 50 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 51 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 52 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 53 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 54 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 55 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 Ghz.</li>
	 * <li>Step 56 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 Ghz.</li>
	 * <li>Step 57 : Set and verify the value of operation standard as g for 2.4
	 * GHz</li>
	 * <li>Step 58 : Connect the connected client in the setup to 2.4 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 59 : Connect the another client in the setup to 5 GHz SSID and
	 * verify connection status.</li>
	 * <li>Step 60 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 61 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID.</li>
	 * <li>Step 62 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz.</li>
	 * <li>Step 63 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz.</li>
	 * <li>Step 64 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 65 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID.</li>
	 * <li>Step 66 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 GHz.</li>
	 * <li>Step 67 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 GHz.</li>
	 * <li>Step 68 : Verify disconnecting the client connected with 2.4 GHz SSID in
	 * the setup.</li>
	 * <li>Step 69 : Verify disconnecting the client connected with 5 GHz SSID in
	 * the setup.</li>
	 * <li>POST CONDITION 1 : Revert the default channel selection mode,operating
	 * standard,default channel value,security mode for 2.4 GHz and 5 GHz</li>
	 * <li>POST CONDITION 2 : Set and verify the security mode as 'WPA2-Personal'
	 * for 2.4 GHz private WiFi</li>
	 * <li>POST CONDITION 3 : Set and verify the security mode as 'WPA2-Personal'
	 * for 5 GHz private WiFi</li>
	 * 
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @refactor Said Hisham
	 *
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-SECTY-WPA-WPA2-5003")
	public void testToVerifyWifiOnDualBandSecurityWpaWpa2(Dut device) {
		String testCaseId = "TC-RDKB-WIFI-SECTY-WPA-WPA2-503";
		String errorMessage = null;
		boolean status = false;
		stepNumber = 1;
		postConStepNumber = 0;
		preConStepNumber = 0;
		String step = "S" + stepNumber;
		List<Dut> ssidVisibleDevices = new ArrayList<Dut>();
		Dut ssidVisibleDeviceOne = null;
		Dut ssidVisibleDeviceTwo = null;
		boolean isSecurityModeChanged = false;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-SECTY-WPA-WPA2-5003");
			LOGGER.info(
					"TEST DESCRIPTION: Verify the WIFI Connectivity on Dual Band Radio for Client for 2.4 and 5 GHz with security mode WPA-WPA2-PSK.");
			LOGGER.info("TEST STEPS : ");

			LOGGER.info(
					"PRE CONDITION 1 : Verify the private wifi 2.4 GHz & 5 GHz SSID's is broadcasting in connected client.");
			LOGGER.info("PRE CONDITION 2 : Get the default value for operating standard 2.4 GHz.");
			LOGGER.info("PRE CONDITION 3 : Get the default value for operating standard 5 GHz.");
			LOGGER.info("PRE CONDITION 4 : Get the default channel value for 2.4 GHz ");
			LOGGER.info("PRE CONDITION 5 : Get the default channel value for 5 GHz.");
			LOGGER.info(
					"PRE CONDITION 6 : Set the channel selection mode to manual and respective channel value for 2ghz and 5ghz via webpa.");
			LOGGER.info("PRE CONDITION 7 : Verify the security mode as 'WPA-WPA2-Personal' for 2.4 GHz SSID");
			LOGGER.info("PRE CONDITION 8 : Verify the security mode as 'WPA-WPA2-Personal' for 5 GHz SSID");
			LOGGER.info("PRE CONDITION 9 : Perform apply settings for 2.4 and 5 GHz radio's");
			LOGGER.info("Step 1 : Set and verify the value of operation standard as n for 5GHz.");
			LOGGER.info("Step 2 : Set and verify the value of operation standard as n for 2GHz.");
			LOGGER.info(
					"Step 3 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info("Step 4 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 5 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 6 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 7 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 8 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 9 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 10 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 11 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 Ghz.");
			LOGGER.info(
					"Step 12 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 Ghz.");
			LOGGER.info("Step 13 : Set and verify the value of operation standard as g for 2.4GHz");
			LOGGER.info(
					"Step 14 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 15 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 16 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 17 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 18 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 19 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 20 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 21 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 22 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 Ghz.");
			LOGGER.info(
					"Step 23 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 Ghz.");
			LOGGER.info("Step 24 : Set and verify the value of operation standard as b for 2.4GHz ");
			LOGGER.info(
					"Step 25 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 26 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 27 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 28 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 29 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 30 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 31 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 32 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 33 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 Ghz.");
			LOGGER.info(
					"Step 34 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 Ghz.");
			LOGGER.info("Step 35 : Set and verify the value of operation standard as ac for 5GHz");
			LOGGER.info(
					"Step 36 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 37 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 38 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 39 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 40 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 41 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 42 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 43 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 43 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 Ghz.");
			LOGGER.info(
					"Step 45 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 Ghz.");
			LOGGER.info("Step 46 : Set and verify the value of operation standard as a for 5 GHz");
			LOGGER.info(
					"Step 47 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 48 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 49 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 50 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 51 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 52 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 53 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 54 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 55 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 Ghz.");
			LOGGER.info(
					"Step 56 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 Ghz.");
			LOGGER.info("Step 57 : Set and verify the value of operation standard as g for 2.4 GHz ");
			LOGGER.info(
					"Step 58 : Connect the connected client in the setup to 2.4 GHz SSID and verify connection status.");
			LOGGER.info(
					"Step 59 : Connect the another client in the setup to 5 GHz SSID and verify connection status.");
			LOGGER.info("Step 60 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID.");
			LOGGER.info("Step 61 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID.");
			LOGGER.info(
					"Step 62 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz.");
			LOGGER.info(
					"Step 63 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz.");
			LOGGER.info("Step 64 : Verify the correct IPv4 address for client connected with 5 GHz SSID.");
			LOGGER.info("Step 65 : Verify the correct IPv6 address for client connected with 5 GHz SSID.");
			LOGGER.info(
					"Step 66 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 Ghz.");
			LOGGER.info(
					"Step 67 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 Ghz.");
			LOGGER.info("Step 68 : Verify disconnecting the client connected with 2.4 GHz SSID in the setup.");
			LOGGER.info("Step 69 : Verify disconnecting the client connected with 5 GHz SSID in the setup.");
			LOGGER.info(
					"POST CONDITION 1 : Revert the default channel selection mode,operating standard,default channel value,security mode for 2.4 GHz and 5 GHz via webpa");
			LOGGER.info(
					"POST CONDITION 2 : Set and verify the security mode as 'WPA2-Personal' for 2.4 GHz private WiFi");
			LOGGER.info(
					"POST CONDITION 3 : Set and verify the security mode as 'WPA2-Personal' for 5 GHz private WiFi");
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			/**
			 * PRE-CONDITION 1 : VERIFY THE PRIVATE WIFI 2.4 GHz AND 5 GHz SSID'S ARE
			 * BROADCASTING IN CONNECTED CLIENT
			 */
			preConStepNumber++;
			ssidVisibleDevices = BroadBandPreConditionUtils.executePreConditionToGetBothPrivateWiFiSsidsVisibleDevices(
					device, tapEnv, preConStepNumber, BroadBandTestConstants.CONSTANT_2);
			ssidVisibleDeviceOne = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_0);
			ssidVisibleDeviceTwo = ssidVisibleDevices.get(BroadBandTestConstants.CONSTANT_1);

			/**
			 * PRE-CONDITION 2-3 : GET THE DEFAULT VALUES
			 */
			executePreConditionToGetDefaultOperStandard(device);

			/**
			 * PRE-CONDITION 4-5 : GET THE DEFAULT VALUES
			 */
			executePreConditionToGetDefaultChannel(device);

			/**
			 * PRE CONDITION 6 : SET THE CHANNEL SELECTION MODE TO MANUAL AND RESPECTIVE
			 * CHANNEL VALUE FOR 2 GHz AND 5 GHz VIA WEBPA.
			 */
			executePreConditionToSetChannel(device);

			/**
			 * PRE-CONDITION 7-8 : VERIFY THE SECURITY MODE "WPA-WPA2-Personal" FOR 2.4 and
			 * 5 GHz SSID
			 */
			preConStepNumber++;
			BroadBandPreConditionUtils.executePreConditionToSetSecurityMode(device, tapEnv, preConStepNumber,
					BroadBandTestConstants.WPA_WPA2_SECURITY_MODE);
			isSecurityModeChanged = true;

			/**
			 * PRE-CONDITION 9 : PERFORM APPLY SETTINGS FOR 2.4 AND 5 GHz RADIO's
			 */
			preConStepNumber = 9;
			BroadBandPreConditionUtils.executePreConditionToRadioApplySetting(device, tapEnv, preConStepNumber);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1 : SET AND VERIFY THE VALUE OF 5 GHz WITH OPERATING STANDARD AS n
			 */
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_N);

			/**
			 * STEP 2 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS n
			 */
			stepNumber++;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_N);

			/**
			 * Step 3 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4 GHz SSID
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 4 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 5 GHz SSID
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 5 - 8 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 9 - 12 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 9;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 13 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS g
			 */
			stepNumber = 13;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_G);

			/**
			 * Step 14 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 15 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 16 - 19 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 20 - 23 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 20;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 24 : SET AND VERIFY THE VALUE OF 2 GHz WITH OPERATING STANDARD AS b
			 */
			stepNumber = 24;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_B);

			/**
			 * Step 25 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 26 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 27 - 30 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * SETP 31 - 34 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 31;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 35 : SET AND VERIFY THE VALUE OF 5 GHz WITH OPERATING STANDARD AS ac
			 */
			stepNumber = 35;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_AC);

			/**
			 * Step 36 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 37 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 38 - 41 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 42 - 45 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 42;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 46 : SET AND VERIFY THE VALUE OF 5 GHz WITH OPERATING STANDARD AS a
			 */
			stepNumber++;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_5GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_A);

			/**
			 * Step 47 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 48 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 49 - 52 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 53 - 56 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 53;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * STEP 57 : SET AND VERIFY THE VALUE OF 2.4 GHz WITH OPERATING STANDARD AS g
			 */
			stepNumber++;
			executeTestStepToChangeOperatingStandard(device, testCaseId, BroadBandTestConstants.BAND_2_4GHZ, stepNumber,
					BroadBandTestConstants.OPERATING_STANDARDS_G);

			/**
			 * Step 58 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHz SSID AND
			 * VERIFY CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 59 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHz SSID AND VERIFY
			 * CONNECTION STATUS
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToConnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);

			/**
			 * SETP 60 - 63 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 2.4 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber++;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceOne,
					stepNumber);

			/**
			 * SETP 64 - 67 : VERIFY THE CORRECT IPV4 AND IPV6 ADDRESS FOR CLIENT CONNECTED
			 * WITH 5 GHz AND INTERNET CONNECTIVITY USING IPV4 AND IPV6 INTERFACE.
			 */
			stepNumber = 64;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testCaseId, ssidVisibleDeviceTwo,
					stepNumber);

			/**
			 * Step 68: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHz
			 */
			stepNumber = 68;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceOne,
					BroadBandTestConstants.BAND_2_4GHZ, stepNumber);

			/**
			 * Step 69: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHz
			 */
			stepNumber++;
			BroadBandWiFiUtils.executeTestStepToDisconnectPrivateWiFi(device, testCaseId, ssidVisibleDeviceTwo,
					BroadBandTestConstants.BAND_5GHZ, stepNumber);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURRED WHILE VALIDATING WIFI CONNECTIVITY ON DUAL BAND RADIO FOR CLIENT FOR 2.4 AND 5 GHz WITH SECURITY MODE WPA-WPA2-Personal : "
							+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS:");
			/**
			 * POST CONDITION 1 : REVERT THE DEFAULT CHANNEL SELECTION MODE, OPERATING
			 * STANDARD, DEFAULT CHANNEL VALUE, SECURITY MODE FOR 2.4 GHz AND 5 GHz
			 */
			executePostConditionToSetChannelAndOprStd(device);

			/**
			 * POST CONDITION 2 : SET AND VERIFY THE SECURITY MODE FOR 2.4 AND 5 GHz PRIVATE
			 * WIFI
			 */
			if (isSecurityModeChanged) {
				postConStepNumber++;
				BroadBandPostConditionUtils.executePostConditionToSetSecurityMode(device, tapEnv, postConStepNumber,
						BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-SECTY-WPA-WPA2-5003");
	}
}
