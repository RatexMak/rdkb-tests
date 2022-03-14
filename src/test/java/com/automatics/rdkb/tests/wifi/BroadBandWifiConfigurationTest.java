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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;

public class BroadBandWifiConfigurationTest extends AutomaticsTestBase {

	/** Constant holds the test step number **/
	private static int stepNumber = 0;
	/** Constant holds the test pre condition step number **/
	private static int preConStepNumber = 0;
	/** Constant holds the test post condition step number **/
	private static int postConStepNumber = 0;

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
		String testId = "TC-XB-WIFI-KEY-PHARSE-501";
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
}
