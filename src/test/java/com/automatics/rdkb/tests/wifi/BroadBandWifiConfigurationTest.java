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

import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.constants.BroadBandTestConstants;
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
	
    /**
     * Test Case : Verify the WIFI Connectivity on Dual Band Radio 802.11n client with security mode open.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1 :Set and verify the value of operational transmission rate of the 2.4 GHz with operating standard as
     * g/n</li>
     * <li>Step 2 :Set and verify the channel value to 11.</li>
     * <li>Step 3 :Set and verify the security mode as "none" for 2.4 GHz SSID</li>
     * <li>Step 4 :Set and verify the value of operational transmission rate of the 5GHz with operating standard as n
     * </li>
     * <li>Step 5 :Set and verify the channel value to 161.</li>
     * <li>Step 6 :Set and verify the security mode as "none" for 5 GHz SSID</li>
     * <li>Step 7 :Connect the connected client in the setup to 2.4 GHz SSID and verify connection status</li>
     * <li>Step 8 :Verify number of connected clients associated with 2.4 GHz</li>
     * <li>Step 9 :Verify the client mac address and connection type connected with 2.4 GHz</li>
     * <li>Step 10 :Connect the connected client in the setup to 5 GHz SSID and verify connection status</li>
     * <li>Step 11 :Verify number of connected clients associated with 5 GHz</li>
     * <li>Step 12 :Verify the client mac address and connection type connected with 5 GHz</li>
     * <li>Step 13 :Verify the correct IPv4 address for client connected with 2.4 GHz SSID</li>
     * <li>Step 14 :Verify the correct IPv6 address for client connected with 2.4 GHz SSID</li>
     * <li>Step 15 :Verify whether have connectivity using that particular interface using IPV4 for client connected
     * with 2.4 GHz</li>
     * <li>Step 16 :Verify whether have connectivity using that particular interface using IPV6 for client connected
     * with 2.4 GHz</li>
     * <li>Step 17 :Verify the correct IPv4 address for client connected with 5 GHz SSID</li>
     * <li>Step 18 :Verify the correct IPv6 address for client connected with 5 GHz SSID</li>
     * <li>Step 19 :Verify whether have connectivity using that particular interface using IPV4 for client connected
     * with 5 GHz</li>
     * <li>Step 20 :Verify whether have connectivity using that particular interface using IPV6 for client connected
     * with 5 GHz</li>
     * <li>Step 21 :Verify disconnecting the 2.4 GHz SSID</li>
     * <li>Step 22 :Verify disconnecting the 5 GHz SSID</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
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
    	     * Step 1 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 2.4 GHZ WITH OPERATING STANDARD
    	     * AS g/n
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
    	     * Step 4 : SET AND VERIFY THE VALUE OF OPERATIONAL TRANSMISSION RATE OF THE 5 GHZ WITH OPERATING STANDARD
    	     * AS n
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
    	
    	    defaultOperatingStandard = CommonMethods.isAtomSyncAvailable(deviceConnectedWith5Ghz, tapEnv)
    		    || DeviceModeHandler.isBusinessClassDevice(device)
    			    ? WifiOperatingStandard.OPERATING_STANDARD_A_N
    			    : WifiOperatingStandard.OPERATING_STANDARD_A_N_AC;
    	    errorMessage = "UNABLE TO SET OPERATING STANDARD AS " + defaultOperatingStandard.getOperatingmode();
    	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
    		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
    		    WebPaDataTypes.STRING.getValue(), defaultOperatingStandard.getOperatingmode());
    	    
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
    		    WebPaDataTypes.INTEGER.getValue(), BroadBandTestConstants.CHANNEL_NO_161);
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
    	     * Step 21: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4 GHZ
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
    	     * Step 22: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5 GHZ.
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
    		
    	}catch (Exception exception) {
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
     * Post-Condition method to set the chanel selection mode as AUTO .
     * 
     * @param device
     *            {@link Dut}
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
     * @param device
     *            {@link Dut}
     * @refactor Athira
     */
    private void executePostConditionToSetDefaultOperStandard(Dut device) throws TestException {
	String errorMessage = null;
	boolean status = false;
	try {
	    /**
	     * POSTCONDITION :SET THE DEFAULT VALUE FOR OPERATING STANDARD 2.4 GHZ USING WEBPA
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
	     * POSTCONDITION 4 :SET THE DEFAULT VALUE FOR OPERATING STANDARD 5 GHZ USING WEBPA
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
     * Post-Condition method to set the default operating standard Bandwidth for 2.4 GHz
     * 
     * @param device
     *            {@link Dut}
     * @param defaultOperStandBandWidth
     *            Instance for default operating standard bandwidth for 2.4 GHz
     * @refactor Athira
     *            
     */
    private void executePostConditionToSetDefaultOperStandBandWidthFor2GHZ(Dut device,
	    String defaultOperStandBandWidth) throws TestException {
	try {
	    /**
	     * POSTCONDITION :SET THE DEFAULT VALUE FOR OPERATING STANDARD BANDWIDTH FOR 2.4 GHZ USING WEBPA
	     */
		Boolean isSpecificDevice = null;
		isSpecificDevice = BroadbandPropertyFileHandler.isSpecificDevice(device);
		
	    if (bandwidthSetStatus && DeviceModeHandler.isFibreDevice(device) || DeviceModeHandler.isBusinessClassDevice(device)
		    || isSpecificDevice) {
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
     * @param device
     *            {@link Dut}
     * @refactor Athira
     */
    private void executePreConditionToSetChannelSelectionModeManual(Dut device) throws TestException {
	String errorMessage = null;
	boolean status = false;
	/**
	 * PRECONDITION :SET THE CHANNEL SELECTION MODE TO MANUAL FOR 2.4 GHz USING WEBPA
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
     * @param device
     *            {@link Dut}
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
     * @param device
     *            {@link Dut}
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
     * Pre-Condition method to verify the private ssid enabled status for 2.4 & 5 Ghz .
     * 
     * @param device
     *            {@link Dut}
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
     * @param device
     *            {@link Dut}
     * @refactor Athira
     */
    private void executePostConditionToSetDefaultChannel(Dut device) throws TestException {
	String errorMessage = null;
	boolean status = false;
	try {
	    /**
	     * POSTCONDITION :SET AND VERIFY THE DEFAULT CHANNEL VALUE FOR 2.4 GHz USING WEBPA
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
     * @param device
     *            {@link Dut}
     * @param isSecModeChanged2Ghz
     *            Instance for device security mode changed for 2.4 Ghz SSID
     * @param isSecModeChanged5Ghz
     *            Instance for device security mode changed for 5 Ghz SSID
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
		 * POSTCONDITION :SET THE SECURITY MODE 'WPA2-Personal' FOR 2.4 GHZ SSID USING WEBPA
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
		 * POSTCONDITION :SET THE SECURITY MODE 'WPA2-Personal' FOR 5 GHZ SSID USING WEBPA
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
     * @param device
     *            {@link Dut}
     * @param deviceConnectedWith2Ghz
     *            Instance for device connected with 2.4 Ghz SSID
     * @param deviceConnectedWith5Ghz
     *            Instance for device connected with 5 Ghz SSID
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
		 * POSTCONDITION :DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 2.4GHz SSID USING WEBPA
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
		 * POSTCONDITION :DISCONNECTING THE WI-FI CLIENT CONNECTED WITH 5GHz SSID USING WEBPA
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
     * Method to verify the IPv4 and IPv6 connection interface & Internet connectivity for 2.4/5 GHz .
     * 
     * @param stepNumber
     *            Step Number
     * @param device
     *            {@link Dut}
     * @param testId
     *            Test case ID
     * @param device
     *            Device Connected
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
	     * Step : VERIFY THE CORRECT IPV4 ADDRESS FOR CONNECTED CLIENT WITH GIVEN GHZ SSID .
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
	     * Step : VERIFY THE CORRECT IPV6 ADDRESS FOR CONNECTED CLIENT WITH GIVEN GHZ SSID .
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :VERIFY THE CORRECT IPV6 ADDRESS FOR CONNECTED CLIENT WITH "
			    + wifiBand + "  SSID FOR DEVICE MODEL " + model);
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 'Wireless LAN adapter Wi-Fi' |grep -i 'IPv6 Address' or LINUX : ifconfig | grep 'inet6 ' ON THE CONNECTED CLIENT");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : IT SHOULD RETURN THE CORRECT IPV4 ADDRESS FOR DEVICE MODEL " + model);
	    LOGGER.info("#####################################################################################");
	    errorMessage = "UNABLE TO GET THE CORRECT IPV6 ADDRESS FROM CLIENT WITH " + wifiBand
		    + " SSID FOR DEVICE MODEL " + model;
	    status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
		    deviceConnected, tapEnv);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : SUCCESSFYLLY VERIFIED CORRECT IPV6 ADDRESS FROM CLIENT WITH " + wifiBand
			+ " SSID FOR DEVICE MODEL " + model);
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    /**
	     * Step : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH GIVEN GHZ SSID INTERFACE USING IPV4 .
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
	     * Step : VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH GIVEN GHZ SSID INTERFACE USING IPV6 .
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
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
	} catch (TestException e) {
	    LOGGER.error(errorMessage);
	    throw new TestException(errorMessage);
	}
	LOGGER.debug("ENDING METHOD : verifyIpv4AndIpV6ConnectionInterface()");
    }
    
    /**
     * Method to verify the IPv4 and IPv6 connection interface & Internet connectivity for 2.4/5 GHz .
     * 
     * @param device
     *            {@link Dut}
     * @param testId
     *            Test case ID
     * @param deviceConnected
     *            {@link Dut}
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
	     * Step : VERIFY THE CLIENT MAC ADDRESS AND CONNECTION TYPE CONNECTED WITH GIVEN GHZ .
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
     * Method to verify and set Operating Standard BandWidth and Channel value for 2.4 GHz
     * 
     * @param device
     *            {@link Dut}
     * @param testId
     *            Test case ID
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
		    && (DeviceModeHandler.isFibreDevice(device)|| isSpecificDevice || DeviceModeHandler.isBusinessClassDevice(device))) {

		errorMessage = "UNABLE TO CHANGE THE CHANNEL OPERATING BANDWIDTH VALUE AS 20MHz FOR 2.4 GHz";
		bandwidthSetStatus = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND,
			WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.OPERATING_BANDWIDTH_20_MMZ);
		if(bandwidthSetStatus) {
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
    * Test Case : Verify that when WIFI Radio is disabled in CM, wireless CPE should not get an ip address.
    * 
    * <p>
    * STEPS:
    * </p>
    * <ol>
    * <li>PRE-CONDITION 1 : Verify 2.4 GHz SSID is enabled</li>
    * <li>PRE-CONDITION 2 : Verify 5 GHz SSID is enabled</li>
    * <li>Step 1 : Set and verify 2.4 GHz radio is disabled</li>
    * <li>Step 2 : Set and verify 5 GHz radio is disabled</li>
    * <li>Step 3 : Verify the private wifi 2.4 GHz SSID is broadcasting in connected client</li>
    * <li>Step 4 : Verify the private wifi 5 GHz SSID is broadcasting in connected client</li>
    * <li>Step 5 : Connect the connected client1 in the setup to 2.4 GHz SSID and verify connection status</li>
    * <li>Step 6 : Connect the connected client2 in the setup to 5 GHz SSID and verify connection status</li>
    * <li>Step 7 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID</li>
    * <li>Step 8 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID</li>
    * <li>Step 9 : Verify whether have connectivity using that particular interface using IPV4 for client connected
    * with 2.4 GHz</li>
    * <li>Step 10 : Verify whether have connectivity using that particular interface using IPV6 for client connected
    * with 2.4 GHz</li>
    * <li>Step 11 : Verify the correct IPv4 address for client connected with 5 GHz SSID</li>
    * <li>Step 12 : Verify the correct IPv6 address for client connected with 5 GHz SSID</li>
    * <li>Step 13 : Verify whether have connectivity using that particular interface using IPV4 for client connected
    * with 5 GHz</li>
    * <li>Step 14 : Verify whether have connectivity using that particular interface using IPV6 for client connected
    * with 5 GHz</li>
    * <li>Step 15 : Set and verify 2.4 GHz radio is enabled</li>
    * <li>Step 16 : Set and verify 5 GHz radio is enabled</li>
    * <li>Step 17 : Verify the private wifi 2.4 GHz SSID is broadcasting in connected client</li>
    * <li>Step 18 : Verify the private wifi 5 GHz SSID is broadcasting in connected client</li>
    * <li>Step 19 : Connect the connected client1 in the setup to 2.4 GHz SSID and verify connection status</li>
    * <li>Step 20 : Connect the connected client2 in the setup to 5 GHz SSID and verify connection status</li>
    * <li>Step 21 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID</li>
    * <li>Step 22 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID</li>
    * <li>Step 23 : Verify whether have connectivity using that particular interface using IPV4 for client connected
    * with 2.4 GHz</li>
    * <li>Step 24 : Verify whether have connectivity using that particular interface using IPV6 for client connected
    * with 2.4 GHz</li>
    * <li>Step 25 : Verify the correct IPv4 address for client connected with 5 GHz SSID</li>
    * <li>Step 26 : Verify the correct IPv6 address for client connected with 5 GHz SSID</li>
    * <li>Step 26 : Verify whether have connectivity using that particular interface using IPV4 for client connected
    * with 5 GHz</li>
    * <li>Step 28 : Verify whether have connectivity using that particular interface using IPV6 for client connected
    * with 5 GHz</li>
    * <li>Step 29 : Verify disconnecting the 2.4 GHz private wifi SSID</li>
    * <li>Step 30 : Verify disconnecting the 5 GHz private wifi SSID</li>
    * <li>POST-CONDITION 1 : Verify 2.4 GHz SSID is enabled</li>
    * <li>POST-CONDITION 2 : Verify 5 GHz SSID is enabled</li>
    * </ol>
    * 
    * @author Muthukumar
    * @refactor Govardhan
    * 
    * @param device
    *            instance of {@link Dut}
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
	     * Step 3 : VERIFY THE PRIVATE WIFI 2.4 GHZ SSID IS BROADCASTING IN CONNECTED CLIENT
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
	     * Step 4 : VERIFY THE PRIVATE WIFI 5 GHZ SSID IS BROADCASTING IN CONNECTED CLIENT
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
	     * Step 5 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHZ SSID AND VERIFY CONNECTION STATUS
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
	     * Step 6 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHZ SSID AND VERIFY CONNECTION STATUS
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
	     * Step 7 : VERIFY THE IPV4 ADDRESS FOR CLIENT 2.4 GHZ SSID WHEN 2.4 GHZ RADIO IS DISABLED .
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
	     * Step 8 : VERIFY THE IPV6 ADDRESS FOR CLIENT 2.4 GHZ SSID WHEN 2.4 GHZ RADIO IS DISABLED .
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
	     * Step 9: VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 2.4 GHZ SSID INTERFACE USING IPV4 .
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
	     * Step 10: VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 2.4 GHZ SSID INTERFACE USING IPV6.
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
	     * Step 11 : VERIFY THE IPV4 ADDRESS FOR CLIENT 5 GHZ SSID WHEN 5 GHZ RADIO IS DISABLED .
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
	     * Step 12 : VERIFY THE IPV6 ADDRESS FOR CLIENT 5 GHZ SSID WHEN 5 GHZ RADIO IS DISABLED .
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
	     * Step 13: VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 5 GHZ SSID INTERFACE USING IPV4 .
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
	     * Step 14: VERIFY THE INTERNET CONNECTIVITY IN THE CLIENT WITH 5 GHZ SSID INTERFACE USING IPV6.
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
	     * Step 17 : VERIFY THE PRIVATE WIFI 2.4 GHZ SSID IS BROADCASTING IN CONNECTED CLIENT,WHEN 2.4 GHZ RADIO IS
	     * REENABLED
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
	     * Step 18 : VERIFY THE PRIVATE WIFI 5 GHZ SSID IS BROADCASTING IN CONNECTED CLIENT,WHEN 5 GHZ RADIO IS
	     * REENABLED
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
	     * Step 20 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHZ SSID AND VERIFY CONNECTION STATUS
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
	     * Step 29: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4 GHZ
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
	     * Step 30: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5 GHZ
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
    
}
