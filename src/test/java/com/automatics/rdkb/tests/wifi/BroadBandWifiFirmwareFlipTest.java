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
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Class to hold the WiFi Configuration tests scripts for connected client
 * setups
 * 
 * @author Muthukumar
 * @refactor yamini.s
 */

public class BroadBandWifiFirmwareFlipTest extends AutomaticsTestBase {

	/** Constant holds the test step number **/
	private static int stepNumber = 0;
	/** Constant holds the default operating standard for 2.4 Ghz **/
	private static String defaultOperStandard_2_4Ghz = null;
	/** Constant holds the default operating standard for 5 Ghz **/
	private static String defaultOperStandard_5Ghz = null;
	/** Variable holds the device reactivated status **/
	private static boolean isReactivated = false;
	/** Constant holds the test pre condition step number **/
	private static int preConStepNumber = 0;
	/** Constant holds the test post condition step number **/
	private static int postConStepNumber = 0;
	/** Constant holds the Current Firmware version **/
	private static String initialFirmwareVersion = null;
	/** Constant holds the Firmware version after factory reset **/
	private static String firmwareVersionAfterFactoryReset = null;

	/**
	 *
	 * Test Case : Verify the Time frame validation for DUT online
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1 : Verify 2.4 GHz SSID is enabled</li>
	 * <li>PRE-CONDITION 2 : Verify 5 GHz SSID is enabled</li>
	 * <li>Step 1 : Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>Step 2 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 3 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 4 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 5 : Verify whether have connectivity using that particular interface
	 * using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 6 : Connect the client 2 to 5 GHz Private Wi-Fi Network and verify
	 * connection status</li>
	 * <li>Step 7 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 8 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 9 : Verify whether have connectivity using that particular interface
	 * using IPV4 for client connected with 5 Ghz</li>
	 * <li>Step 10 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 Ghz</li>
	 * <li>Step 11 : Verify Triggered CDL Successful with latest firmware
	 * version</li>
	 * <li>Step 12 : Verify the time for device boot up time</li>
	 * <li>Step 13 : Connect the connected client1 in the setup to 2.4 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 14 : Connect the connected client2 in the setup to 5 GHz SSID and
	 * verify connection status</li>
	 * <li>Step 15 : Verify the correct IPv4 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 16 : Verify the correct IPv6 address for client connected with 2.4
	 * GHz SSID</li>
	 * <li>Step 17 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 2.4 GHz</li>
	 * <li>Step 18 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 2.4 GHz</li>
	 * <li>Step 19 : Verify the correct IPv4 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 20 : Verify the correct IPv6 address for client connected with 5 GHz
	 * SSID</li>
	 * <li>Step 21 : Verify whether have connectivity using that particular
	 * interface using IPV4 for client connected with 5 Ghz</li>
	 * <li>Step 22 : Verify whether have connectivity using that particular
	 * interface using IPV6 for client connected with 5 Ghz</li>
	 * <li>Step 23 : Verify disconnecting the connected client connected with 2.4
	 * GHz SSID in the setup</li>
	 * <li>Step 24 : Verify disconnecting the connected client connected with 5 GHz
	 * SSID in the setup</li>
	 * <li>Step 25 : Verify Triggered CDL Successful with previous firmware
	 * version</li>
	 * <li>POST-CONDITION 1 : Verify that device is upgraded with the previous
	 * firmware version in #S25, if not try CDL again</li> .
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Muthukumar
	 * @refactor yamini.s
	 *
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WIFI-FIRM-DWNL-5001")
	public void testToVerifyWiFiConnAfterFrimwareUpgrade(Dut device) {
		String testId = "TC-RDKB-WIFI-FIRM-DWNL-501";
		String step = null;
		String errorMessage = null;
		boolean status = false;
		Dut deviceConnectedWith2Ghz = null;
		Dut deviceConnectedWith5Ghz = null;
		stepNumber = 1;
		preConStepNumber = 1;
		postConStepNumber = 1;
		step = "S" + stepNumber;
		initialFirmwareVersion = null;
		boolean hasOriginalBuildChanged = false;
		boolean hasLatestBuildChanged = false;
		BroadBandResultObject result = null;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-FIRM-DWNL-5001");
			LOGGER.info("TEST DESCRIPTION: Verify the Time frame validation for DUT online.");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info(" 1 : Connect the client 1 to 2.4 GHz Private Wi-Fi Network and verify connection status ");
			LOGGER.info(" 2 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(" 3 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(
					" 4 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 5 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 6 : Connect the client 2 to 5 GHz Private Wi-Fi Network and verify connection status ");
			LOGGER.info(" 7 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
			LOGGER.info(" 8 : Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
			LOGGER.info(
					" 9 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 Ghz ");
			LOGGER.info(
					" 10 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 Ghz ");
			LOGGER.info(" 11 : Verify  Triggered CDL  Successful with latest firmware version ");
			LOGGER.info(" 12 : Verify the time for device boot up time ");
			LOGGER.info(
					" 13 : Connect  the connected client1  in the setup to 2.4 GHz SSID and verify connection status ");
			LOGGER.info(
					" 14 : Connect  the connected client2  in the setup to 5 GHz SSID and verify connection status ");
			LOGGER.info(" 15 : Verify  the correct IPv4  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(" 16 : Verify  the correct IPv6  address for client connected with 2.4 GHz SSID ");
			LOGGER.info(
					" 17 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 2.4 GHz ");
			LOGGER.info(
					" 18 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 2.4 GHz ");
			LOGGER.info(" 19 : Verify  the correct IPv4  address for client connected with 5 GHz SSID ");
			LOGGER.info(" 20 : Verify  the correct IPv6  address for client connected with 5 GHz SSID ");
			LOGGER.info(
					" 21 : Verify whether have connectivity using that particular interface using IPV4 for client connected with 5 Ghz ");
			LOGGER.info(
					" 22 : Verify whether have connectivity using that particular interface using IPV6 for client connected with 5 Ghz ");
			LOGGER.info(" 23 : Verify disconnecting the connected client connected with 2.4 GHz SSID in the setup ");
			LOGGER.info(" 24 : Verify disconnecting the connected client connected with 5 GHz SSID in the setup ");
			LOGGER.info(" 25 : Verify  Triggered CDL  Successful  with previous firmware version ");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS:");
			BroadBandPreConditionUtils.executePreConditionToVerifyRadioStatus(device, tapEnv, preConStepNumber);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 2.4GHZ SSID
			 */
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY CONNECTING THE WIFI CLIENT INTO 2.4GHZ SSID");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : THE CONNECTION MUST BE SUCCESSFUL FOR 2.4 GHZ SSID");
			LOGGER.info("#####################################################################################");
			errorMessage = "UNABLE TO CONNECT THE CONNECTED CLIENT WITH 2.4 GHZ SSID";
			deviceConnectedWith2Ghz = BroadBandConnectedClientUtils
					.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
			status = (null != deviceConnectedWith2Ghz);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : SUCCESSFYLLY CONNECTED THE CLIENT WITH 2.4 GHZ SSID.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 2- 5
			 */
			stepNumber++;
			step = "S" + stepNumber;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					stepNumber);

			/**
			 * Step 6: VERIFY CONNECTING THE WIFI CLIENT IN THE SETUP TO THE 5 GHZ SSID
			 */
			stepNumber = 6;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : CONNECT THE CLIENT TO 5 GHZ PRIVATE WI-FI NETWORK AND VERIFY CONNECTION STATUS ");
			LOGGER.info("STEP " + stepNumber + " : ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD");
			LOGGER.info(
					"STEP " + stepNumber + " : EXPECTED : DEVICE SHOULD BE CONNECTED WITH 5 GHZ PRIVATE WI-FI NETWORK");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO CONNECT TO 5 GHZ PRIVATE WI-FI NETWORK OR 5 GHZ WIFI CAPABLE DEVICES ARE NOT AVAILABLE";
			try {
				deviceConnectedWith5Ghz = BroadBandConnectedClientUtils.getOtherWiFiCapableClientDeviceAndConnect(
						device, tapEnv, deviceConnectedWith2Ghz, BroadBandTestConstants.BAND_5GHZ);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			status = (null != deviceConnectedWith5Ghz);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : DEVICE HAS BEEN SUCCESSFULLY CONNECTED WITH 5 GHZ PRIVATE WI-FI NETWORK");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 7- 10
			 */
			stepNumber++;
			step = "S" + stepNumber;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					stepNumber);

			/**
			 * Step 11 : VERIFY TRIGGERED CDL SUCCESSFULL WITH LATEST FIRMWARE VERSION
			 */
			stepNumber = 11;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY TRIGGERED CDL SUCCESSFULL WITH LATEST FIRMWARE VERSION ");
			LOGGER.info(
					"STEP " + stepNumber + ": ACTION : TRIGGER THE CDL WITH LATEST FIRMWARE VERSION USING TR-181/SNMP");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : CDL MUST SUCCESSFUL WITH LATEST FIRMWARE VERSION.");
			LOGGER.info("#######################################################################################");
			errorMessage = "UNABLE TO TRIGGER THE IMAGE UPGRADE ON THE DEVICE WITH LATEST FIRMWARE VERSION.";
			initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
			LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
			hasLatestBuildChanged = BroadBandCodeDownloadUtils.triggerLatestCodeDownload(device, tapEnv,
					initialFirmwareVersion);
			status = hasLatestBuildChanged;
			LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : CDL SUCCESSFUL WITH LATEST FIRMWARE VERSION");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 12 : VERIFY THE TIME FOR DEVICE BOOT UP TIME
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : VERIFY THE TIME FOR DEVICE BOOT UP TIME ");
			LOGGER.info("STEP " + stepNumber + ": ACTION : VERIFY THE TIME FOR DEVICE BOOT UP TIME USING WEBPA");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : MUST RETURN THE DEVICE BOOT UP TIME IN MILLI SECONDS.");
			LOGGER.info("#######################################################################################");
			tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			result = BroadBandCommonUtils.calculateDeviceUpTime(device, tapEnv);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : ACTUAL AND EXPECTED BOOTTIME IS EQUAL.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

			/**
			 * Step 13 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 2.4 GHZ SSID AND
			 * VERIFY CONNECTION STATUS AFTER FIRMWARE UPGRADE
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : CONNECT  THE CONNECTED CLIENT  IN THE SETUP TO 2.4 GHZ SSID WITH PASSWORD AND VERIFY CONNECTION STATUS AFTER FIRMWARE UPGRADE");
			LOGGER.info("STEP " + stepNumber + ": ACTION : CONNECT THE WI-FI CLIENT WITH 2.4GHz SSID AND PASSWORD ");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST NOT BE SUCCESSFUL FOR 2.4 GHZ SSID  AND PASSWORD AFTER FIRMWARE UPGRADE");
			LOGGER.info("#####################################################################################");
			errorMessage = "CONNECTION FAILED FOR 2.4 GHZ SSID WITH PASSWORD AFTER FIRMWARE UPGRADE";
			String ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			String password = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device,
					tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
			if (CommonMethods.isNotNull(ssid) && CommonMethods.isNotNull(password)) {
				status = ConnectedNattedClientsUtils.connectToSSID(deviceConnectedWith2Ghz, tapEnv, ssid, password);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTION SUCCESSFUL FOR FOR 2.4 GHZ SSID WITH PASSWORD AFTER FIRMWARE UPGRADE.");
			} else {
				deviceConnectedWith2Ghz = null;
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 14 : CONNECT THE CONNECTED CLIENT IN THE SETUP TO 5 GHZ SSID AND VERIFY
			 * CONNECTION STATUS AFTER FIRMWARE UPGRADE
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			errorMessage = null;
			LOGGER.info("#####################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : CONNECT  THE CONNECTED CLIENT  IN THE SETUP TO 5 GHZ SSID WITH PASSWORD AND VERIFY CONNECTION STATUS AFTER FIRMWARE UPGRADE");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : CONNECT THE WI-FI CLIENT WITH 5GHz SSID AND PASSWORD AFTER FIRMWARE UPGRADE");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : THE CONNECTION MUST NOT BE SUCCESSFUL FOR 5 GHZ SSID  AND PASSWORD");
			LOGGER.info("#####################################################################################");
			errorMessage = "CONNECTION FAILED FOR 5 GHZ SSID WITH PASSWORD AFTER FIRMWARE UPGRADE";
			ssid = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			password = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			if (CommonMethods.isNotNull(ssid) && CommonMethods.isNotNull(password)) {
				status = ConnectedNattedClientsUtils.connectToSSID(deviceConnectedWith5Ghz, tapEnv, ssid, password);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : CONNECTION SUCCESSFUL FOR FOR 5 GHZ SSID WITH PASSWORD AFTER FIRMWARE UPGRADE .");
			} else {
				deviceConnectedWith5Ghz = null;
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * SETP 15- 18
			 */
			stepNumber++;
			step = "S" + stepNumber;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith2Ghz,
					stepNumber);

			/**
			 * SETP 19- 22
			 */
			stepNumber = 19;
			step = "S" + stepNumber;
			BroadBandWiFiUtils.verifyIpv4AndIpV6ConnectionInterface(device, testId, deviceConnectedWith5Ghz,
					stepNumber);

			/**
			 * Step 23: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 2.4
			 * GHZ
			 */
			stepNumber = 23;
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
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

			/**
			 * Step 24: VERIFY DISCONNECTING THE WIFI CLIENT FROM THE GATEWAY DEVICE FOR 5
			 * GHZ
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

			/**
			 * Step 25 : VERIFY TRIGGERED CDL SUCCESSFULL WITH PREVIOUS FIRMWARE VERSION
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : VERIFY TRIGGERED CDL SUCCESSFULL WITH PREVIOUS FIRMWARE VERSION ");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : CDL MUST SUCCESSFUL WITH PREVIOUS FIRMWARE VERSION.");
			errorMessage = "UNABLE TO TRIGGER THE IMAGE UPGRADE ON THE DEVICE WITH PREVIOUS FIRMWARE VERSION.";
			LOGGER.info("initial firmware version :"+initialFirmwareVersion);
			hasOriginalBuildChanged = BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv,
					initialFirmwareVersion);
			status = hasOriginalBuildChanged;
			LOGGER.info("FLASHED THE ORIGINAL BUILD ON THE DEVICE: " + status);
			if (status) {
				LOGGER.info("STEP " + stepNumber + " : ACTUAL : CDL SUCCESSFUL WITH PREVIOUS FIRMWARE VERSION.");
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING WIFI CLIENT CONNECTIVITY AFTER FIRMWARE UPGRADE : "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, status, errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith2Ghz(device, tapEnv,
					deviceConnectedWith2Ghz, postConStepNumber);
			postConStepNumber++;
			BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith5Ghz(device, tapEnv,
					deviceConnectedWith5Ghz, postConStepNumber);
			postConStepNumber++;
			BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, hasLatestBuildChanged,
					hasOriginalBuildChanged, postConStepNumber, initialFirmwareVersion);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-FIRM-DWNL-5001");
		LOGGER.info("#######################################################################################");
	}
}