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
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils.WifiOperatingStandard;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWiFiConnectedClientTests extends AutomaticsTestBase{
	
	/**
     * Device device MUST have the ability to change from a/n/ac to ac only, a/n/ac to n only, a-n-ac to n-ac only radio
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1: Check the operating mode for 5GHz Wi-Fi from RDKB device</li>
     * <li>Step 2: Check the mode of operation in 5 Ghz client is 802.11a/n/ac</li>
     * <li>Step 3: Change the mode of operation in 5GHz client from 802.11a/n/ac to 802.11ac and verify the status</li>
     * <li>Step 4: Check 5 Ghz client is connected and IPV4 address obtained</li>
     * <li>Step 5: Check the mode of operation in 5GHz client is 802.11ac</li>
     * <li>Step 6: Change the mode of operation in 5 Ghz client to 802.11n</li>
     * <li>Step 7: Check 5 Ghz client is connected and IPV4 address obtained</li>
     * <li>Step 8: Check the mode of operation in 5Ghz client is 802.11n</li>
     * <li>Step 9: Change the mode of operation in 5GHz client to 802.11 n/ac</li>
     * <li>Step 10: Check 5 Ghz client is connected and IPV4 address obtained</li>
     * <li>Step 11: Check the mode of operation in 5Ghz client is 802.11n/ac</li>
     * <li>STEP 12: Verify the mode of operation in 5 Ghz client"</li>
     * </ol>
     * 
     * @author anandam.s
     * @refactor yamini.s
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-WEBPA-3003")

    public void testVerifyChangeOperatingModeFor5GHz(Dut device) {

	// string variable to store test case id
	String testCaseId = "TC-RDKB-WIFI-WEBPA-303";
	// Boolean variable to store the status of each step
	boolean status = false;
	// String variable to store the errorMessage in each step
	String errorMessage = null;
	// String variable to store the successMessage
	String successMessage = null;
	// String variable to store the step number
	String step = "s1";
	// variable to store the instance of connected client
	Dut connectedClientDevice = null;
	// variable to store the ostype of connected client
	String clientOsType = null;
	WifiOperatingStandard defaultOperatingStandard = null;
	BroadBandResultObject resultObject = null;
	try {

	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-WEBPA-3003");

	    LOGGER.info("**************************************************************");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify the ability to change mode from a/n/ac to ac only, a/n/ac to n only, a-n-ac to n-ac only radio on the 5 GHz radio");

	    LOGGER.info("*************************************************************************");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: Verify default operating mode from gateway device for 5 GHz wifi network");
	    LOGGER.info("STEP 2: Verify 5 Ghz client is connected and IPV4 address obtained");
	    LOGGER.info("STEP 3: Verify default operating mode from client device for 5 GHz wifi network");
	    LOGGER.info("STEP 4: Change the operating mode to 802.11ac");
	    LOGGER.info("STEP 5: Verify 5 Ghz client is connected and IPV4 address obtained");
	    LOGGER.info("STEP 6: Verify the mode of operation in 5 Ghz client ");
	    LOGGER.info("STEP 7: Change the operating mode to 802.11n in 5Ghz client ");
	    LOGGER.info("STEP 8: Verify 5 Ghz client is connected and IPV4 address obtained ");
	    LOGGER.info("STEP 9: Verify the mode of operation in 5 Ghz client ");
	    LOGGER.info("STEP 10: Change the operating mode to 802.11n/ac in 5Ghz client ");
	    LOGGER.info("STEP 11: Verify 5 Ghz client is connected and IPV4 address obtained");
	    LOGGER.info("STEP 12: Verify the mode of operation in 5 Ghz client");
	    LOGGER.info("**********************************************************************************");

	    /**
	     * Step1 : Verify the operating mode for 5GHz WiFi from RDKB device
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION :Verify default operating mode from gateway device for 5 GHz wifi network");
	    LOGGER.info("STEP 1: ACTION : Execute command to verify default operating mode 5 Ghz mode of operation");
	    LOGGER.info(
		    "STEP 1: EXPECTED :Operating mode should be 802.11a/n for atom console devices, n for DSL devices and for rest of the devices expected if 802.11a/n/ac ");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		defaultOperatingStandard = WifiOperatingStandard.OPERATING_STANDARD_A_N;
	    } else if (BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)) {
		defaultOperatingStandard = WifiOperatingStandard.OPERATING_STANDARD_A_N_AC_AX;
	    } else {
		defaultOperatingStandard = WifiOperatingStandard.OPERATING_STANDARD_A_N_AC;
	    }
	    errorMessage = "Received Operating standard is Not as Expected "
		    + defaultOperatingStandard.getOperatingmode();
	    try {
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
			defaultOperatingStandard.getOperatingmode());
	    } catch (TestException exception) {
		errorMessage = errorMessage + exception.getMessage();
		LOGGER.info(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL :The operating mode for 5GHz network in RDKB device is as expected ");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /**
	     * Step2 : Check 5 GHz client is connected and IPV4 address obtained
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION :Verify 5 Ghz client is connected and IPV4 address obtained");
	    LOGGER.info("STEP 2: ACTION : Execute command to connect 5 Ghz and get IPV4 address");
	    LOGGER.info("STEP 2: EXPECTED :IPV4 address should be obtained");
	    LOGGER.info("**********************************************************************************");
	    step = "s2";
	    status = false;
	    errorMessage = "Client device is NOT connected properly over WiFi with default operating standard - 802.11a/n/ac";

	    try {
		connectedClientDevice = BroadBandConnectedClientUtils
			.get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
		if (connectedClientDevice != null) {
		    clientOsType = ((Device) connectedClientDevice).getOsType();
		    LOGGER.info("Waiting for 30 seconds to connect to device and acquire IP address");
		    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

		    status = BroadBandConnectedClientUtils
			    .verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(clientOsType,
				    connectedClientDevice, tapEnv)
			    && checkConnectivityOfDevice(connectedClientDevice);
		} else {
		    errorMessage = "Failed to get a Connected client device in 5Ghz band";
		    LOGGER.error(errorMessage);
		}
	    } catch (TestException exception) {
		errorMessage = errorMessage + exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL :Client device is connected properly over WiFi with default operating standard ");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /**
	     * Step3 : Verify the operating mode for 5GHz WiFi from client device
	     */

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION :Verify default operating mode from client device for 5 GHz wifi network");
	    LOGGER.info("STEP 3: ACTION : Execute command to verify  default operating mode from 5 Ghz");
	    LOGGER.info("STEP 3: EXPECTED :Operating mode should be 802.11a or 802.11n or 802.11ac");
	    LOGGER.info("**********************************************************************************");

	    step = "s3";
	    status = false;
	    errorMessage = "The default value for operating mode in 2.4GHz network for client device is NOT either 802.11a 802.11n or 802.11ac'";
	    try {
		status = BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(connectedClientDevice,
			tapEnv, defaultOperatingStandard.getClientOperatingMode());
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL :The operating mode for 5GHz network in client device is a/n/ac ");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step4 : Change the mode of operation in 5GHz client to 802.11 ac; Change the mode of operation in 5GHz
	     * client to 802.11a/n/ac for devices
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION :Change the operating mode to 802.11ac in gateway using WebPA - Device.WiFi.Radio.10100.OperatingStandards");
	    LOGGER.info("STEP 4: ACTION : Execute command to change the operating mode from 5 Ghz");
	    LOGGER.info(
		    "STEP 4: EXPECTED :The operating mode should change to 802.11ac; for model mode should change to a/n/ac ");
	    LOGGER.info("**********************************************************************************");
	    step = "s4";
	    status = false;

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))) {
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    } else {
		errorMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			? "Failed to change the operating mode for 5GHz network in Gateway from a/n/ac/ax to a/n/ac mode "
				+ "using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
			: "Failed to change the operating mode for 5GHz network in Gateway from a/n/ac"
				+ " to ac mode using WebPA - Device.WiFi.Radio.10100.OperatingStandards.";
		status = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			? BroadBandWiFiUtils.setWebPaParams(device,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
				WifiOperatingStandard.OPERATING_STANDARD_A_N_AC.getOperatingmode(),
				WebPaDataTypes.STRING.getValue())
			: BroadBandWiFiUtils.setWebPaParams(device,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
				WifiOperatingStandard.OPERATING_STANDARD_AC.getOperatingmode(),
				WebPaDataTypes.STRING.getValue());
		successMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			? "Changed the operating mode for 5GHz network from a/n/ac/ax to a/n/ac "
				+ "using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
			: "Changed the operating mode for 5GHz network from a/n/ac to ac "
				+ "using WebPA - Device.WiFi.Radio.10100.OperatingStandards.";
		if (status) {
		    LOGGER.info("STEP 4: ACTUAL :" + successMessage);
		} else {
		    LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		LOGGER.info(
			"waiting for two minutes after changing the operating standard for client to change its operating standard");
		tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
		// }
		/**
		 * Step5 : Check 5 GHz client is connected and IPV4 address obtained
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 5: DESCRIPTION :Verify 5 Ghz client is connected and IPV4 address obtained");
		LOGGER.info("STEP 5: ACTION : Execute command to connect 5 Ghz nd IPV4 address");
		LOGGER.info("STEP 5: EXPECTED :IPV4 address should be obtained");
		LOGGER.info("**********************************************************************************");
		step = "s5";
		status = false;
		errorMessage = "Client device is NOT connected properly over WiFi after changing opearting mode in gateway to ac -";
		if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {

		    try {
			resultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
				connectedClientDevice, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
			if (resultObject.isStatus()) {
			    status = BroadBandConnectedClientUtils
				    .verifyIpv4AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(clientOsType,
					    connectedClientDevice, tapEnv)
				    && checkConnectivityOfDevice(connectedClientDevice);
			} else {
			    errorMessage = "Failed to Connect client device after change in operating standard with message -"
				    + resultObject.getErrorMessage();
			    LOGGER.error(errorMessage);
			}
		    } catch (TestException exception) {
			errorMessage = errorMessage + exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		    if (status) {
			LOGGER.info("STEP 5: ACTUAL : 5GHz client is connected and IPV4 address obtained");
		    } else {
			LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
		}
		/**
		 * Step6 : Check the mode of operation in 5 Ghz client
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 6: DESCRIPTION :Verify the mode of operation in 5 Ghz client");
		LOGGER.info("STEP 6: ACTION : Execute command to verify mode of operation 5 Ghz");
		LOGGER.info(
			"STEP 6: EXPECTED :The mode of operation should be 802.11ac; for models mode should be a/n/ac");
		LOGGER.info("**********************************************************************************");
		step = "s6";
		status = false;

		if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {
		    try {

			errorMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? "The operating mode for 5GHz network is not changed from a/n/ac/ax to a/n/ac mode in client device."
				: "The operating mode for 5GHz network is not changed from \'a/n/ac\' to \'ac\' mode in client device.";
			status = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
					connectedClientDevice, tapEnv,
					WifiOperatingStandard.OPERATING_STANDARD_A_N_AC.getClientOperatingMode())
				: BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
					connectedClientDevice, tapEnv,
					WifiOperatingStandard.OPERATING_STANDARD_AC.getClientOperatingMode());
			successMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? "Changed the operating mode for 5GHz network from a/n/ac/ax to a/n/ac "
					+ "using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
				: "Changed the operating mode for 5GHz network from a/n/ac to ac "
					+ "using WebPA - Device.WiFi.Radio.10100.OperatingStandards.";
		    } catch (TestException exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		    if (status) {
			LOGGER.info("STEP 6: ACTUAL :" + successMessage);
		    } else {
			LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");

		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		}
		/**
		 * Step7 : Change the mode of operation in 5GHz client to 802.11 n; For devices change mode from a/n/ac
		 * to a/n/ac/ax
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 7: DESCRIPTION :Change the operating mode to 802.11n in 5Ghz gateway using WebPA - Device.WiFi.Radio.10100.OperatingStandards");
		LOGGER.info("STEP 7: ACTION : Execute command to change the mode of operation 5 Ghz");
		LOGGER.info(
			"STEP 7: EXPECTED :The mode of operation should be 802.11n; for devices mode should change to a/n/ac/ax");
		LOGGER.info("**********************************************************************************");
		step = "s7";
		status = false;

		if (DeviceModeHandler.isDSLDevice(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    BroadBandTestConstants.NA_MSG_FOR_DSL_DEVICES, false);
		} else {

		    errorMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			    ? "Failed to change the operating mode for 5GHz network from a/n/ac to a/n/ac/ax mode using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
			    : "Failed to change the operating mode for 5GHz network from ac to n mode using WebPA - Device.WiFi.Radio.10100.OperatingStandards.";
		    status = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			    ? BroadBandWiFiUtils.setWebPaParams(device,
				    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
				    WifiOperatingStandard.OPERATING_STANDARD_A_N_AC_AX.getOperatingmode(),
				    WebPaDataTypes.STRING.getValue())
			    : BroadBandWiFiUtils.setWebPaParams(device,
				    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
				    WifiOperatingStandard.OPERATING_STANDARD_N.getOperatingmode(),
				    WebPaDataTypes.STRING.getValue());
		    successMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
			    ? "Changed the operating mode for 5GHz network from a/n/ac to a/n/ac/ax using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
			    : "Changed the operating mode for 5GHz network from ac to n using WebPA - Device.WiFi.Radio.10100.OperatingStandards.";
		    if (status) {
			LOGGER.info("STEP 7: ACTUAL :" + successMessage);
		    } else {
			LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		    LOGGER.info(
			    "waiting for two minutes after changing the operating standard for client to change its operating standard");
		    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
		}
		/**
		 * Step8 : Check 5 GHz client is connected and IPV4 address obtained
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 8: DESCRIPTION :Verify 5 Ghz client is connected and IPV4 address obtained");
		LOGGER.info("STEP 8: ACTION : Execute command to connect to 5 Ghz and get IPV4 address");
		LOGGER.info("STEP 8: EXPECTED :IPV4 address should be obtained");
		LOGGER.info("**********************************************************************************");
		step = "s8";
		status = false;
		errorMessage = "Client device is NOT connected properly over WiFi after changing opearting mode in gateway to n -";
		if (DeviceModeHandler.isDSLDevice(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    BroadBandTestConstants.NA_MSG_FOR_DSL_DEVICES, false);
		} else {
		    try {
			resultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
				connectedClientDevice, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
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
		    if (status) {
			LOGGER.info(
				"STEP 8: ACTUAL :Client device is connected properly over WiFi after changing opearting mode in gateway.");
		    } else {
			LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		}
		/**
		 * Step9 : Check the mode of operation in 5 Ghz client
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 9: DESCRIPTION :Verify the mode of operation in 5 Ghz client");
		LOGGER.info("STEP 9: ACTION : Execute command to verify mode of operation 5 Ghz");
		LOGGER.info(
			"STEP 9: EXPECTED :The mode of operation should be 802.11n; for devices mode should be a/n/ac/ax");
		LOGGER.info("**********************************************************************************");
		step = "s9";
		status = false;

		if (DeviceModeHandler.isDSLDevice(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    BroadBandTestConstants.NA_MSG_FOR_DSL_DEVICES, false);
		} else {
		    try {

			errorMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? "The operating mode for 5GHz network is not changed from a/n/ac to a/n/ac/ax mode in client device."
				: "The operating mode for 5GHz network is not changed from \'ac\' to \'n' mode in client device.";
			status = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
					connectedClientDevice, tapEnv,
					WifiOperatingStandard.OPERATING_STANDARD_A_N_AC_AX.getClientOperatingMode())
				: BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
					connectedClientDevice, tapEnv,
					WifiOperatingStandard.OPERATING_STANDARD_N.getClientOperatingMode());
			successMessage = BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)
				? "Successfully verified the operating mode for 5GHz network as 802.11a/n/ac/ax"
				: "Successfully verified the operating mode for 5GHz network as 802.11n";
		    } catch (TestException exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		    if (status) {
			LOGGER.info("STEP 9: ACTUAL :" + successMessage);
		    } else {
			LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		}

		/**
		 * Step10 : Change the mode of operation in 5GHz client to 802.11 n/ac
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 10: DESCRIPTION :Change the operating mode to 802.11n/ac in 5Ghz gateway using WebPA - Device.WiFi.Radio.10100.OperatingStandards");
		LOGGER.info("STEP 10: ACTION : Execute command to change mode of operation in 5 Ghz");
		LOGGER.info("STEP 10: EXPECTED :The mode of operation should be 802.11n/ac");
		LOGGER.info("**********************************************************************************");
		step = "s10";
		status = false;
		errorMessage = "Failed to change the operating mode for 5GHz network from n to n/ac mode in Gateway device using WebPA - Device.WiFi.Radio.10100.OperatingStandards";

		if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))
			|| BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {
		    status = BroadBandWiFiUtils.setWebPaParams(device,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
			    WifiOperatingStandard.OPERATING_STANDARD_N_AC.getOperatingmode(),
			    WebPaDataTypes.STRING.getValue());
		    if (status) {
			LOGGER.info(
				"STEP 10: ACTUAL :Changed the operating mode for 5GHz network from n to n/ac using WebPA - Device.WiFi.Radio.10100.OperatingStandards.");
		    } else {
			LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		    LOGGER.info(
			    "waiting for two minutes after changing the operating standard for client to change its operating standard");
		    tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
		}
		/**
		 * Step11 : Check 5 GHz client is connected and IPV4 address obtained
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 11: DESCRIPTION :Verify 5 Ghz client is connected and IPV4 address obtained");
		LOGGER.info("STEP 11: ACTION : Execute command to connect to 5 Ghz and get IPV4 address");
		LOGGER.info("STEP 11: EXPECTED :IPV4 address should be obtained");
		LOGGER.info("**********************************************************************************");
		step = "s11";
		status = false;
		errorMessage = "Client device is NOT connected properly over WiFi after changing opearting mode in gateway to n/ac -";

		if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))
			|| BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {

		    try {
			resultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
				connectedClientDevice, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
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
		    if (status) {
			LOGGER.info(
				"STEP 11: ACTUAL :Successfully connected to 5Ghz wifi client and verified IPV4 address");
		    } else {
			LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");

		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		}
		/**
		 * Step12 : Check the mode of operation in 5 Ghz client
		 */
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 12: DESCRIPTION :Verify the mode of operation in 5 Ghz client");
		LOGGER.info("STEP 12: ACTION : Execute command to verify the mode of operation in 5 Gh");
		LOGGER.info("STEP 12: EXPECTED :The mode of operation should be 802.11n or 802.11ac");
		LOGGER.info("**********************************************************************************");
		step = "s12";
		status = false;
		errorMessage = "The operating mode for 5GHz network is not changed from \'802.11n\' to \'802.11n or 802.11ac\' mode in client device.";

		if (CommonMethods.isAtomSyncAvailable(device, tapEnv) || (DeviceModeHandler.isDSLDevice(device))
			|| BroadbandPropertyFileHandler.getStatusForDeviceCheck(device)) {
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {
		    try {
			status = BroadBandConnectedClientUtils.verifyOperatingStandardInConnectedClient(
				connectedClientDevice, tapEnv,
				WifiOperatingStandard.OPERATING_STANDARD_N_AC.getClientOperatingMode());
		    } catch (TestException exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(errorMessage);
		    }
		    if (status) {
			LOGGER.info("STEP 12: ACTUAL :Successfully verified operating mode for 5GHz network ");
		    } else {
			LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");

		    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		}
	    }
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "Exception occured while changing the operating mode from a/n/ac to ac only, a/n/ac to n only, a-n-ac to n-ac only for 5 GHz network using WebPA - Device.WiFi.Radio.10100.OperatingStandards"
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	} finally {

	    status = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_5GHZ_OPERATING_STANDARD,
		    defaultOperatingStandard.getOperatingmode(), WebPaDataTypes.STRING.getValue());
	    LOGGER.info("Finally : " + (status
		    ? "Changed the operating mode for 5GHz network to default value - a/n/ac using WebPA - Device.WiFi.Radio.10100.OperatingStandards."
		    : "Operating mode is not changed to default value using WebPA - Device.WiFi.Radio.10100.OperatingStandards"));

	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-WEBPA-3003");
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
